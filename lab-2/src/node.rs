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

impl<K, V> Node<K, V>
where
    K: Ord + Clone,
    V: Clone,
{
    #[allow(dead_code)]
    fn clone(&self) -> NodeRef<K, V> {
        let new = Self { 
            key: self.key.clone(), 
            value: self.value.clone(), 
            color: self.color, 
            left: self.left.clone(), 
            right: self.right.clone(), 
            parent: None 
        };

        let current_ref = Rc::new(RefCell::new(new));

        {
            let left_node = current_ref.clone();
            let left_parent = &mut left_node.borrow_mut().parent;
            *left_parent = Some(Rc::downgrade(&current_ref))
        }

        {
            let right_node = current_ref.clone();
            let right_parent = &mut right_node.borrow_mut().parent;
            *right_parent = Some(Rc::downgrade(&current_ref))
        }

        current_ref.clone()
    }
}
