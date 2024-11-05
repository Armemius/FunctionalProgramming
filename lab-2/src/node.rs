use std::{
    cell::RefCell, rc::{Rc, Weak}
};
use std::fmt::Display;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Color {
    Red,
    Black,
}

type NodeRef<K, V> = Rc<RefCell<Node<K, V>>>;
type WeakNodeRef<K, V> = Weak<RefCell<Node<K, V>>>;

struct Node<K, V>
where
    K: Ord,
{
    key: K,
    value: V,
    color: Color,
    left: Option<NodeRef<K, V>>,
    right: Option<NodeRef<K, V>>,
    parent: Option<WeakNodeRef<K, V>>,
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

pub struct Tree<K, V>
where
    K: Ord,
{
    root: Option<Rc<RefCell<Node<K, V>>>>,
}

impl<K, V> Tree<K, V>
where
    K: Ord,
{
    pub fn new() -> Self {
        Self { root: None }
    }

    fn balance(&mut self, node: NodeRef<K, V>) {
        // todo!();
    }

    fn insert_helper(&self, current: NodeRef<K, V>, key: K, value: V) -> Option<NodeRef<K, V>> {
        let mut current_node = current.borrow_mut();
        if current_node.key > key {
            if let Some(left) = current_node.left.clone() {
                self.insert_helper(left, key, value)
            } else {
                let new_node = Rc::new(RefCell::new(Node::new_red(key, value)));
                current_node.left = Some(new_node.clone());
                new_node.borrow_mut().parent = Some(Rc::downgrade(&current));
                Some(new_node.clone())
            }
        } else if current_node.key < key {
            if let Some(right) = current_node.right.clone() {
                self.insert_helper(right, key, value)
            } else {
                let new_node = Rc::new(RefCell::new(Node::new_red(key, value)));
                current_node.right = Some(new_node.clone());
                new_node.borrow_mut().parent = Some(Rc::downgrade(&current));
                Some(new_node.clone())
            }
        } else {
            current_node.value = value;
            None
        }
    }

    pub fn insert(&mut self, key: K, value: V) {
        if let Some(root) = self.root.clone() {
            let new_node = self.insert_helper(root, key, value);
            if let Some(node) = new_node {
                self.balance(node);
            }
        } else {
            self.root = Some(Rc::new(RefCell::new(Node::new_black(key, value))));
        }
    }
}


// Ouput 

impl<K, V> Tree<K, V>
where
    K: Ord + Display,
    V: Display,
{
    pub fn dump_tree(&self) {
        if let Some(root) = &self.root {
            self.print_node(root.clone(), "", true);
        } else {
            println!("(Empty)");
        }
    }

    fn print_node(&self, node: NodeRef<K, V>, prefix: &str, is_tail: bool) {
        let node_ref = node.borrow();

        // Print the current node
        println!(
            "{}{}─ [{}, {}] ({:?})",
            prefix,
            if is_tail { "└" } else { "├" },
            node_ref.key,
            node_ref.value,
            node_ref.color
        );

        // Collect children nodes
        let mut children = Vec::new();
        if node_ref.left.is_some() {
            children.push((node_ref.left.clone().unwrap(), false));
        }
        if node_ref.right.is_some() {
            children.push((node_ref.right.clone().unwrap(), true));
        }

        // Iterate through the children
        let len = children.len();
        for (i, (child, is_right)) in children.into_iter().enumerate() {
            let is_last = i == len - 1;
            let new_prefix = format!(
                "{}{}   ",
                prefix,
                if is_tail { "    " } else { "│   " }
            );
            self.print_node(child, &new_prefix, is_right);
        }
    }
}
