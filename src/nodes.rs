use bytemuck::{cast_mut, cast_ref};
use num_enum::{IntoPrimitive, TryFromPrimitive};
use static_assertions::const_assert_eq;
use std::mem::size_of;

pub type NodeHandle = u32;
const NODE_SIZE: usize = 128;

#[derive(IntoPrimitive, TryFromPrimitive)]
#[repr(u8)]
pub enum NodeTag {
    Uninitialized = 0,
    InnerNode = 1,
    LeafNode = 2,
    FreeNode = 3,
    LastFreeNode = 4,
}

#[derive(Debug, Clone, Copy, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(C)]
pub struct InnerNode {
    pub tag: u8, // NodeTag
    pub padding: [u8; 3],
    /// number of highest `key` bits that all children share
    /// e.g. if it's 2, the two highest bits of `key` will be the same on all children
    pub prefix_len: u32,
    pub padding2: [u8; 8],

    /// only the top `prefix_len` bits of `key` are relevant
    pub key: u128,

    /// indexes into `BookSide::nodes`
    pub children: [NodeHandle; 2],

    /// The earliest expiry timestamp for the left and right subtrees.
    ///
    /// Needed to be able to find and remove expired orders without having to
    /// iterate through the whole bookside.
    pub child_earliest_expiry: [u64; 2],

    pub reserved: [u8; 72],
}
const_assert_eq!(size_of::<InnerNode>(), NODE_SIZE);
const_assert_eq!(size_of::<InnerNode>() % 8, 0);
const_assert_eq!(align_of::<InnerNode>(), 16);

impl InnerNode {
    pub fn new(prefix_len: u32, key: u128) -> Self {
        Self {
            tag: NodeTag::InnerNode.into(),
            padding: Default::default(),
            prefix_len,
            padding2: Default::default(),
            key,
            children: [0; 2],
            child_earliest_expiry: [u64::MAX; 2],
            reserved: [0; 72],
        }
    }

    /// Returns the handle of the child that may contain the search key
    /// and 0 or 1 depending on which child it was.
    pub(crate) fn walk_down(&self, search_key: u128) -> (NodeHandle, bool) {
        let crit_bit_mask = 1u128 << (127 - self.prefix_len);
        let crit_bit = (search_key & crit_bit_mask) != 0;
        (self.children[crit_bit as usize], crit_bit)
    }

    /// The lowest timestamp at which one of the contained LeafNodes expires.
    #[inline(always)]
    pub fn earliest_expiry(&self) -> u64 {
        std::cmp::min(self.child_earliest_expiry[0], self.child_earliest_expiry[1])
    }
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy, bytemuck::Pod, bytemuck::Zeroable)]
pub struct Pubkey(pub(crate) [u8; 32]);

/// LeafNodes represent an order in the binary tree
#[derive(Debug, Copy, Clone, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(C)]
pub struct LeafNode {
    /// NodeTag
    pub tag: u8,

    /// Index into the owning MangoAccount's PerpOpenOrders
    pub owner_slot: u8,

    /// PostOrderType, this was added for TradingView move order
    pub order_type: u8,

    pub padding: [u8; 1],

    /// Time in seconds after `timestamp` at which the order expires.
    /// A value of 0 means no expiry.
    pub time_in_force: u16,

    pub padding2: [u8; 10],

    /// The binary tree key, see new_node_key()
    pub key: u128,

    /// Address of the owning MangoAccount
    pub owner: Pubkey,

    /// Number of base lots to buy or sell, always >=1
    pub quantity: i64,

    /// The time the order was placed
    pub timestamp: u64,

    /// If the effective price of an oracle pegged order exceeds this limit,
    /// it will be considered invalid and may be removed.
    ///
    /// Only applicable in the oracle_pegged OrderTree
    pub peg_limit: i64,

    /// User defined id for this order, used in FillEvents
    pub client_order_id: u64,

    pub reserved: [u8; 32],
}
const_assert_eq!(size_of::<LeafNode>(), NODE_SIZE);
const_assert_eq!(size_of::<LeafNode>() % 8, 0);

#[derive(Eq, PartialEq, Copy, Clone, TryFromPrimitive, IntoPrimitive, Debug)]
#[repr(u8)]
pub enum PostOrderType {
    /// Take existing orders up to price, max_base_quantity and max_quote_quantity.
    /// If any base_quantity or quote_quantity remains, place an order on the book
    Limit = 0,

    /// Never take any existing orders, post the order on the book if possible.
    /// If existing orders can match with this order, do nothing.
    PostOnly = 2,

    /// If existing orders match with this order, adjust the price to just barely
    /// not match. Always places an order on the book.
    PostOnlySlide = 4,
}

impl LeafNode {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        owner_slot: u8,
        key: u128,
        owner: Pubkey,
        quantity: i64,
        timestamp: u64,
        order_type: PostOrderType,
        time_in_force: u16,
        peg_limit: i64,
        client_order_id: u64,
    ) -> Self {
        Self {
            tag: NodeTag::LeafNode.into(),
            owner_slot,
            order_type: order_type.into(),
            padding: Default::default(),
            time_in_force,
            padding2: Default::default(),
            key,
            owner,
            quantity,
            timestamp,
            peg_limit,
            client_order_id,
            reserved: [0; 32],
        }
    }

    /// The order's price_data as stored in the key
    ///
    /// Needs to be unpacked differently for fixed and oracle pegged orders.
    #[inline(always)]
    pub fn price_data(&self) -> u64 {
        (self.key >> 64) as u64
    }

    /// Time at which this order will expire, u64::MAX if never
    #[inline(always)]
    pub fn expiry(&self) -> u64 {
        if self.time_in_force == 0 {
            u64::MAX
        } else {
            self.timestamp + self.time_in_force as u64
        }
    }

    /// Returns if the order is expired at `now_ts`
    #[inline(always)]
    pub fn is_expired(&self, now_ts: u64) -> bool {
        self.time_in_force > 0 && now_ts >= self.timestamp + self.time_in_force as u64
    }
}

#[derive(Copy, Clone, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(C)]
pub struct FreeNode {
    pub(crate) tag: u8, // NodeTag
    pub(crate) padding: [u8; 3],
    pub(crate) next: NodeHandle,
    pub(crate) reserved: [u8; NODE_SIZE - 16],
    // ensure that FreeNode has the same 8-byte alignment as other nodes
    pub(crate) force_align: u64,
}
const_assert_eq!(size_of::<FreeNode>(), NODE_SIZE);
const_assert_eq!(size_of::<FreeNode>() % 8, 0);

#[derive(Copy, Clone, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(C)]
pub struct AnyNode {
    pub tag: u8,
    pub data: [u8; 127],
}
const_assert_eq!(size_of::<AnyNode>(), NODE_SIZE);
const_assert_eq!(size_of::<AnyNode>() % 8, 0);
const_assert_eq!(size_of::<AnyNode>(), size_of::<InnerNode>());
const_assert_eq!(size_of::<AnyNode>(), size_of::<LeafNode>());
const_assert_eq!(size_of::<AnyNode>(), size_of::<FreeNode>());

pub(crate) enum NodeRef<'a> {
    Inner(&'a InnerNode),
    Leaf(&'a LeafNode),
}

pub(crate) enum NodeRefMut<'a> {
    Inner(&'a mut InnerNode),
    Leaf(&'a mut LeafNode),
}

impl AnyNode {
    pub fn key(&self) -> Option<u128> {
        match self.case()? {
            NodeRef::Inner(inner) => Some(inner.key),
            NodeRef::Leaf(leaf) => Some(leaf.key),
        }
    }

    pub(crate) fn children(&self) -> Option<[NodeHandle; 2]> {
        match self.case().unwrap() {
            NodeRef::Inner(&InnerNode { children, .. }) => Some(children),
            NodeRef::Leaf(_) => None,
        }
    }

    pub(crate) fn case(&self) -> Option<NodeRef> {
        match NodeTag::try_from(self.tag) {
            Ok(NodeTag::InnerNode) => Some(NodeRef::Inner(cast_ref(self))),
            Ok(NodeTag::LeafNode) => Some(NodeRef::Leaf(cast_ref(self))),
            _ => None,
        }
    }

    fn case_mut(&mut self) -> Option<NodeRefMut> {
        match NodeTag::try_from(self.tag) {
            Ok(NodeTag::InnerNode) => Some(NodeRefMut::Inner(cast_mut(self))),
            Ok(NodeTag::LeafNode) => Some(NodeRefMut::Leaf(cast_mut(self))),
            _ => None,
        }
    }

    #[inline]
    pub fn as_leaf(&self) -> Option<&LeafNode> {
        match self.case() {
            Some(NodeRef::Leaf(leaf_ref)) => Some(leaf_ref),
            _ => None,
        }
    }

    #[inline]
    pub fn as_leaf_mut(&mut self) -> Option<&mut LeafNode> {
        match self.case_mut() {
            Some(NodeRefMut::Leaf(leaf_ref)) => Some(leaf_ref),
            _ => None,
        }
    }

    #[inline]
    pub fn as_inner(&self) -> Option<&InnerNode> {
        match self.case() {
            Some(NodeRef::Inner(inner_ref)) => Some(inner_ref),
            _ => None,
        }
    }

    #[inline]
    pub fn as_inner_mut(&mut self) -> Option<&mut InnerNode> {
        match self.case_mut() {
            Some(NodeRefMut::Inner(inner_ref)) => Some(inner_ref),
            _ => None,
        }
    }

    #[inline]
    pub fn earliest_expiry(&self) -> u64 {
        match self.case().unwrap() {
            NodeRef::Inner(inner) => inner.earliest_expiry(),
            NodeRef::Leaf(leaf) => leaf.expiry(),
        }
    }
}

impl AsRef<AnyNode> for InnerNode {
    fn as_ref(&self) -> &AnyNode {
        cast_ref(self)
    }
}

impl AsRef<AnyNode> for LeafNode {
    #[inline]
    fn as_ref(&self) -> &AnyNode {
        cast_ref(self)
    }
}

/// Creates price data for a fixed order's price
///
/// Reverse of fixed_price_lots()
pub fn fixed_price_data(price_lots: i64) -> u64 {
    assert!(price_lots >= 1);
    price_lots as u64
}

/// Retrieves the price (in lots) from a fixed order's price data
///
/// Reverse of fixed_price_data().
pub fn fixed_price_lots(price_data: u64) -> i64 {
    assert!(price_data <= i64::MAX as u64);
    price_data as i64
}
