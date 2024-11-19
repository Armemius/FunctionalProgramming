use std::{
    cell::RefCell, rc::{Rc, Weak}
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Color {
    Red,
    Black,
}

pub(super) type NodeRef<K, V> = Rc<RefCell<Node<K, V>>>;
pub(super) type WeakNodeRef<K, V> = Weak<RefCell<Node<K, V>>>;

pub(super) struct Node<K, V>
where
    K: Ord,
{
    pub key: K,
    pub value: V,
    pub color: Color,
    pub left: Option<NodeRef<K, V>>,
    pub right: Option<NodeRef<K, V>>,
    pub parent: Option<WeakNodeRef<K, V>>,
}

impl<K, V> PartialEq for Node<K, V>
where
    K: Ord,
{
    fn eq(&self, other: &Self) -> bool {
        self.key == other.key
    }
}

impl<K, V> Node<K, V>
where
    K: Ord,
{
    pub fn new_red(key: K, value: V) -> Self {
        Self {
            key,
            value,
            color: Color::Red,
            left: None,
            right: None,
            parent: None,
        }
    }

    pub fn new_black(key: K, value: V) -> Self {
        Self {
            key,
            value,
            color: Color::Black,
            left: None,
            right: None,
            parent: None,
        }
    }
}
