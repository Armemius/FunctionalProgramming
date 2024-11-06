use std::fmt::Display;
use std::{
    cell::RefCell,
    rc::{Rc, Weak},
};

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
    V: Clone,
{
    key: K,
    value: V,
    color: Color,
    left: Option<NodeRef<K, V>>,
    right: Option<NodeRef<K, V>>,
    parent: Option<WeakNodeRef<K, V>>,
}

impl<K, V> PartialEq for Node<K, V>
where
    K: Ord,
    V: Clone,
{
    fn eq(&self, other: &Self) -> bool {
        self.key == other.key
    }
}

impl<K, V> Node<K, V>
where
    K: Ord,
    V: Clone
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
    V: Clone
{
    root: Option<Rc<RefCell<Node<K, V>>>>,
}

impl<K, V> Tree<K, V>
where
    K: Ord,
    V: Clone
{
    pub fn new() -> Self {
        Self { root: None }
    }

    fn balance_insertion(&mut self, node: NodeRef<K, V>) {
        let parent_option = {
            let node_ref = node.borrow();
            node_ref.parent.clone()
        };
        if parent_option.is_none() {
            node.borrow_mut().color = Color::Black;
            return;
        }

        let parent = parent_option.unwrap().upgrade();
        let uncle = Self::get_uncle(node.clone());
        let grandparent = Self::get_grandparent(node.clone());

        let parent_color = Self::get_node_color(parent.clone());
        let uncle_color = Self::get_node_color(uncle.clone());

        match (parent_color, uncle_color) {
            (Color::Black, _) => {}
            (_, Color::Red) => {
                let parent_rc = parent.unwrap();
                {
                    let mut parent_ref = parent_rc.borrow_mut();
                    parent_ref.color = Color::Black;
                }
                let uncle_rc = uncle.unwrap();
                {
                    let mut uncle_ref = uncle_rc.borrow_mut();
                    uncle_ref.color = Color::Black;
                }
                let grandparent_rc = grandparent.unwrap();
                {
                    let mut grandparent_ref = grandparent_rc.borrow_mut();
                    grandparent_ref.color = Color::Red;
                }
                self.balance_insertion(grandparent_rc);
            }
            (_, Color::Black) => {
                let parent_rc = parent.unwrap();
                let grandparent_rc = grandparent.unwrap();
                let is_left_child = {
                    let grandparent_ref = grandparent_rc.borrow();
                    if let Some(left_child) = &grandparent_ref.left {
                        Rc::ptr_eq(&parent_rc, left_child)
                    } else {
                        false
                    }
                };
                if is_left_child {
                    let node_is_right_child = {
                        let parent_ref = parent_rc.borrow();
                        if let Some(right_child) = &parent_ref.right {
                            Rc::ptr_eq(&node, right_child)
                        } else {
                            false
                        }
                    };
                    if node_is_right_child {
                        self.rotate_left(parent_rc.clone());
                        self.balance_insertion(parent_rc);
                    } else {
                        {
                            let mut grandparent_ref = grandparent_rc.borrow_mut();
                            grandparent_ref.color = Color::Red;
                        }
                        {
                            let mut parent_ref = parent_rc.borrow_mut();
                            parent_ref.color = Color::Black;
                        }
                        self.rotate_right(grandparent_rc);
                    }
                } else {
                    let node_is_left_child = {
                        let parent_ref = parent_rc.borrow();
                        if let Some(left_child) = &parent_ref.left {
                            Rc::ptr_eq(&node, left_child)
                        } else {
                            false
                        }
                    };
                    if node_is_left_child {
                        self.rotate_right(parent_rc.clone());
                        self.balance_insertion(parent_rc);
                    } else {
                        {
                            let mut grandparent_ref = grandparent_rc.borrow_mut();
                            grandparent_ref.color = Color::Red;
                        }
                        {
                            let mut parent_ref = parent_rc.borrow_mut();
                            parent_ref.color = Color::Black;
                        }
                        self.rotate_left(grandparent_rc);
                    }
                }
            }
        }
    }

    fn insert_helper(&self, current: NodeRef<K, V>, key: K, value: V) -> Option<NodeRef<K, V>> {
        let mut current_node = current.borrow_mut();
        if current_node.key > key {
            if let Some(left) = current_node.left.clone() {
                drop(current_node);
                self.insert_helper(left, key, value)
            } else {
                let new_node = Rc::new(RefCell::new(Node::new_red(key, value)));
                current_node.left = Some(new_node.clone());
                new_node.borrow_mut().parent = Some(Rc::downgrade(&current));
                Some(new_node)
            }
        } else if current_node.key < key {
            if let Some(right) = current_node.right.clone() {
                drop(current_node);
                self.insert_helper(right, key, value)
            } else {
                let new_node = Rc::new(RefCell::new(Node::new_red(key, value)));
                current_node.right = Some(new_node.clone());
                new_node.borrow_mut().parent = Some(Rc::downgrade(&current));
                Some(new_node)
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
                self.balance_insertion(node);
            }
        } else {
            self.root = Some(Rc::new(RefCell::new(Node::new_black(key, value))));
        }
    }

    fn get_node_color(node: Option<NodeRef<K, V>>) -> Color {
        if let Some(node) = node {
            node.borrow().color
        } else {
            Color::Black
        }
    }

    fn get_grandparent(node: Rc<RefCell<Node<K, V>>>) -> Option<NodeRef<K, V>> {
        let parent_weak = {
            let node_ref = node.borrow();
            node_ref.parent.clone()
        };
        let parent_rc = parent_weak?.upgrade()?;
        let grandparent_weak = {
            let parent_ref = parent_rc.borrow();
            parent_ref.parent.clone()
        };
        grandparent_weak?.upgrade()
    }

    fn get_uncle(node: Rc<RefCell<Node<K, V>>>) -> Option<NodeRef<K, V>> {
        let parent_weak = {
            let node_ref = node.borrow();
            node_ref.parent.clone()
        };
        let parent_rc = parent_weak?.upgrade()?;
        let grandparent_weak = {
            let parent_ref = parent_rc.borrow();
            parent_ref.parent.clone()
        };
        let grandparent_rc = grandparent_weak?.upgrade()?;

        let parent_is_left_child = {
            let grandparent_ref = grandparent_rc.borrow();
            if let Some(left_child) = &grandparent_ref.left {
                Rc::ptr_eq(&parent_rc, left_child)
            } else {
                false
            }
        };

        if parent_is_left_child {
            let grandparent_ref = grandparent_rc.borrow();
            grandparent_ref.right.clone()
        } else {
            let grandparent_ref = grandparent_rc.borrow();
            grandparent_ref.left.clone()
        }
    }

    fn rotate_left(&mut self, x: Rc<RefCell<Node<K, V>>>) {
        let y_option = {
            let x_ref = x.borrow();
            x_ref.right.clone()
        };
        let y = match y_option {
            Some(node) => node,
            None => return, // Cannot rotate left if x.right is None
        };

        // Begin rotation
        {
            // Move y.left to x.right
            let mut x_mut = x.borrow_mut();
            x_mut.right = {
                let y_ref = y.borrow();
                y_ref.left.clone()
            };
        }

        // Update y.left.parent to x if y.left is not None
        if let Some(ref x_right) = x.borrow().right {
            x_right.borrow_mut().parent = Some(Rc::downgrade(&x));
        }

        // Update y.parent to x.parent
        {
            let x_parent_option = {
                let x_ref = x.borrow();
                x_ref.parent.clone()
            };
            y.borrow_mut().parent = x_parent_option.clone();
        }

        // Update x.parent's child pointer to y
        {
            let x_parent_option = {
                let x_ref = x.borrow();
                x_ref.parent.clone()
            };

            if let Some(x_parent_weak) = x_parent_option {
                if let Some(x_parent) = x_parent_weak.upgrade() {
                    let mut x_parent_mut = x_parent.borrow_mut();
                    if let Some(ref left_child) = x_parent_mut.left {
                        if Rc::ptr_eq(&x, left_child) {
                            x_parent_mut.left = Some(y.clone());
                        }
                    }
                    if let Some(ref right_child) = x_parent_mut.right {
                        if Rc::ptr_eq(&x, right_child) {
                            x_parent_mut.right = Some(y.clone());
                        }
                    }
                }
            } else {
                // x was root, update self.root to y
                self.root = Some(y.clone());
            }
        }

        // Set y.left = x
        {
            let mut y_mut = y.borrow_mut();
            y_mut.left = Some(x.clone());
        }

        // Update x.parent to y
        {
            x.borrow_mut().parent = Some(Rc::downgrade(&y));
        }
    }

    fn rotate_right(&mut self, x: Rc<RefCell<Node<K, V>>>) {
        let y_option = {
            let x_ref = x.borrow();
            x_ref.left.clone()
        };
        let y = match y_option {
            Some(node) => node,
            None => return, // Cannot rotate right if x.left is None
        };

        // Begin rotation
        {
            // Move y.right to x.left
            let mut x_mut = x.borrow_mut();
            x_mut.left = {
                let y_ref = y.borrow();
                y_ref.right.clone()
            };
        }

        // Update y.right.parent to x if y.right is not None
        if let Some(ref x_left) = x.borrow().left {
            x_left.borrow_mut().parent = Some(Rc::downgrade(&x));
        }

        // Update y.parent to x.parent
        {
            let x_parent_option = {
                let x_ref = x.borrow();
                x_ref.parent.clone()
            };
            y.borrow_mut().parent = x_parent_option.clone();
        }

        // Update x.parent's child pointer to y
        {
            let x_parent_option = {
                let x_ref = x.borrow();
                x_ref.parent.clone()
            };

            if let Some(x_parent_weak) = x_parent_option {
                if let Some(x_parent) = x_parent_weak.upgrade() {
                    let mut x_parent_mut = x_parent.borrow_mut();
                    if let Some(ref left_child) = x_parent_mut.left {
                        if Rc::ptr_eq(&x, left_child) {
                            x_parent_mut.left = Some(y.clone());
                        }
                    }
                    if let Some(ref right_child) = x_parent_mut.right {
                        if Rc::ptr_eq(&x, right_child) {
                            x_parent_mut.right = Some(y.clone());
                        }
                    }
                }
            } else {
                // x was root, update self.root to y
                self.root = Some(y.clone());
            }
        }

        // Set y.right = x
        {
            let mut y_mut = y.borrow_mut();
            y_mut.right = Some(x.clone());
        }

        // Update x.parent to y
        {
            x.borrow_mut().parent = Some(Rc::downgrade(&y));
        }
    }

    pub fn get(&self, key: &K) -> Option<V> {
        let mut current = self.root.clone();
        while let Some(node) = current {
            let node_ref = node.borrow();
            if node_ref.key == *key {
                return Some(node_ref.value.clone());
            } else if node_ref.key > *key {
                current = node_ref.left.clone();
            } else {
                current = node_ref.right.clone();
            }
        }
        None
    }
}

// Output

impl<K, V> Tree<K, V>
where
    K: Ord + Display,
    V: Display + Clone
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
            let new_prefix = format!("{}{}   ", prefix, if is_tail { "    " } else { "│   " });
            self.print_node(child, &new_prefix, is_right);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    impl<K, V> Tree<K, V>
    where 
        K: Ord,
        V: Clone {

        fn test_root_black(&self) -> bool {
            if let Some(root) = self.root.clone() {
                let root_ref = root.borrow();
                root_ref.color == Color::Black
            } else {
                true
            }
        }

        fn test_black_nodes_helper(&self, node: Option<NodeRef<K, V>>, current_count: usize, count: &mut Option<usize>) -> bool {
            if let Some(node) = node {
                let current_count = if node.borrow().color == Color::Black {
                    current_count + 1
                } else {
                    current_count
                };
                self.test_black_nodes_helper(node.borrow().left.clone(), current_count, count) 
                && self.test_black_nodes_helper(node.borrow().right.clone(), current_count, count)
            } else {
                if let Some(count) = count {
                    current_count == *count
                } else {
                    *count = Some(current_count);
                    true
                }
            }
        }

        fn test_black_nodes_count(&self) -> bool {
            self.test_black_nodes_helper(self.root.clone(), 0, &mut None)
        }
    }

    #[test]
    fn test_properties() {
        let mut tree = Tree::<i32, i32>::new();

        for it in 0..128 {
            tree.insert(it, it * 3);
        }

        assert!(tree.test_root_black());
        assert!(tree.test_black_nodes_count());
    }

    #[test]
    fn test_insert() {
        let mut tree = Tree::<i32, i32>::new();

        for it in 64..128 {
            tree.insert(it, it * 3);
        }
        for it in (0..64).rev() {
            tree.insert(it, it * 2);
        }

        assert_eq!(tree.get(&8), Some(16));
        assert_eq!(tree.get(&100), Some(300));
        assert_eq!(tree.get(&200), None);

        tree.insert(8, 228);
        assert_eq!(tree.get(&8), Some(228));
    }
}