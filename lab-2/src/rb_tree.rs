use std::fmt::Display;
use std::{
    cell::RefCell,
    rc::{Rc, Weak},
};

// Abstract allocator for type T

use std::collections::VecDeque;

#[derive(Debug)]
pub struct Memory<T> {
    buffer: Vec<Option<T>>,
    freed: VecDeque<usize>,
}

impl<T> Memory<T> {
    pub fn new() -> Self {
        Self {
            buffer: Vec::<Option<T>>::new(),
            freed: VecDeque::<usize>::new(),
        }
    }

    pub fn allocate(&mut self, value: T) -> usize {
        match self.freed.pop_front() {
            Some(index) => {
                self.buffer[index] = Some(value);
                index
            }
            None => {
                self.buffer.push(Some(value));
                self.buffer.len() - 1
            }
        }
    }

    pub fn deallocate(&mut self, index: usize) {
        if self.buffer.len() <= index {
            return;
        }
        self.buffer[index] = None;
        self.freed.push_back(index);
    }

    pub fn access(&self, index: usize) -> Option<&T> {
        if self.buffer.len() <= index {
            return None;
        }
        self.buffer[index].as_ref()
    }

    pub fn modify(&mut self, index: usize, value: T) {
        if self.buffer.len() <= index {
            return;
        }
        self.buffer[index] = Some(value);
    }
}

impl<T> Memory<T>
where
    T: std::fmt::Debug,
{
    pub fn dump(&self) {
        print!("{self:?}");
    }
}

// Nodes

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
    V: Clone,
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

// Tree

pub struct Tree<K, V>
where
    K: Ord,
    V: Clone,
{
    root: Option<Rc<RefCell<Node<K, usize>>>>,
    memory: Memory<V>,
}

impl<K, V> Tree<K, V>
where
    K: Ord,
    V: Clone,
{
    pub fn new() -> Self {
        Self {
            root: None,
            memory: Memory::new(),
        }
    }

    fn balance_insertion(&mut self, node: NodeRef<K, usize>) {
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

    fn insert_helper(
        &mut self,
        current: NodeRef<K, usize>,
        key: K,
        value: V,
    ) -> Option<NodeRef<K, usize>> {
        let mut current_node = current.borrow_mut();
        if current_node.key > key {
            if let Some(left) = current_node.left.clone() {
                self.insert_helper(left, key, value)
            } else {
                let new_node = Rc::new(RefCell::new(Node::new_red(
                    key,
                    self.memory.allocate(value),
                )));
                current_node.left = Some(new_node.clone());
                new_node.borrow_mut().parent = Some(Rc::downgrade(&current));
                Some(new_node)
            }
        } else if current_node.key < key {
            if let Some(right) = current_node.right.clone() {
                self.insert_helper(right, key, value)
            } else {
                let new_node = Rc::new(RefCell::new(Node::new_red(
                    key,
                    self.memory.allocate(value),
                )));
                current_node.right = Some(new_node.clone());
                new_node.borrow_mut().parent = Some(Rc::downgrade(&current));
                Some(new_node)
            }
        } else {
            self.memory.modify(current_node.value, value);
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
            self.root = Some(Rc::new(RefCell::new(Node::new_black(
                key,
                self.memory.allocate(value),
            ))));
        }
    }

    fn get_node_color(node: Option<NodeRef<K, usize>>) -> Color {
        if let Some(node) = node {
            node.borrow().color
        } else {
            Color::Black
        }
    }

    fn get_grandparent(node: Rc<RefCell<Node<K, usize>>>) -> Option<NodeRef<K, usize>> {
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

    fn get_uncle(node: Rc<RefCell<Node<K, usize>>>) -> Option<NodeRef<K, usize>> {
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

    fn rotate_left(&mut self, x: Rc<RefCell<Node<K, usize>>>) {
        let y_option = {
            let x_ref = x.borrow();
            x_ref.right.clone()
        };
        let y = match y_option {
            Some(node) => node,
            None => return, // Cannot rotate left if x.right is None
        };

        {
            let mut x_mut = x.borrow_mut();
            x_mut.right = {
                let y_ref = y.borrow();
                y_ref.left.clone()
            };
        }

        if let Some(ref x_right) = x.borrow().right {
            x_right.borrow_mut().parent = Some(Rc::downgrade(&x));
        }

        {
            let x_parent_option = {
                let x_ref = x.borrow();
                x_ref.parent.clone()
            };
            y.borrow_mut().parent = x_parent_option.clone();
        }

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
                self.root = Some(y.clone());
            }
        }

        {
            let mut y_mut = y.borrow_mut();
            y_mut.left = Some(x.clone());
        }

        {
            x.borrow_mut().parent = Some(Rc::downgrade(&y));
        }
    }

    fn rotate_right(&mut self, x: Rc<RefCell<Node<K, usize>>>) {
        let y_option = {
            let x_ref = x.borrow();
            x_ref.left.clone()
        };
        let y = match y_option {
            Some(node) => node,
            None => return, // Cannot rotate right if x.left is None
        };

        {
            let mut x_mut = x.borrow_mut();
            x_mut.left = {
                let y_ref = y.borrow();
                y_ref.right.clone()
            };
        }

        if let Some(ref x_left) = x.borrow().left {
            x_left.borrow_mut().parent = Some(Rc::downgrade(&x));
        }

        {
            let x_parent_option = {
                let x_ref = x.borrow();
                x_ref.parent.clone()
            };
            y.borrow_mut().parent = x_parent_option.clone();
        }

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
                self.root = Some(y.clone());
            }
        }

        {
            let mut y_mut = y.borrow_mut();
            y_mut.right = Some(x.clone());
        }

        {
            x.borrow_mut().parent = Some(Rc::downgrade(&y));
        }
    }

    pub fn delete(&mut self, key: &K) {
        let node_to_delete = self.get_node_by_key(key, self.root.clone());
        if let Some(node) = node_to_delete {
            self.delete_node(node);
        }
    }

    fn delete_node(&mut self, node: NodeRef<K, usize>) {
        let mut node = node;

        if node.borrow().left.is_some() && node.borrow().right.is_some() {
            let successor = Self::minimum_node(node.borrow().right.as_ref().unwrap().clone());
            {
                let mut node_borrow = node.borrow_mut();
                let mut successor_borrow = successor.borrow_mut();
                std::mem::swap(&mut node_borrow.key, &mut successor_borrow.key);
                std::mem::swap(&mut node_borrow.value, &mut successor_borrow.value);
            }
            node = successor;
        }

        let child = if node.borrow().left.is_some() {
            node.borrow().left.clone()
        } else {
            node.borrow().right.clone()
        };

        let parent = node.borrow().parent.clone();

        if let Some(child_node) = child.clone() {
            child_node.borrow_mut().parent = parent.clone();
        }

        if let Some(parent_weak) = parent.clone() {
            let parent = parent_weak.upgrade().unwrap();
            let mut parent_borrow = parent.borrow_mut();
            if Some(node.clone()) == parent_borrow.left {
                parent_borrow.left = child.clone();
            } else {
                parent_borrow.right = child.clone();
            }
        } else {
            // Node is root
            self.root = child.clone();
        }

        let node_color = node.borrow().color;
        let child_color = Self::get_node_color(child.clone());

        self.memory.deallocate(node.borrow().value);

        if node_color == Color::Black {
            if child_color == Color::Red {
                if let Some(child_node) = child {
                    child_node.borrow_mut().color = Color::Black;
                }
            } else {
                self.balance_deletion(child, parent);
            }
        }
    }

    fn balance_deletion(
        &mut self,
        mut node: Option<NodeRef<K, usize>>,
        mut parent: Option<WeakNodeRef<K, usize>>,
    ) {
        while node != self.root && Self::get_node_color(node.clone()) == Color::Black {
            if let Some(parent_weak) = parent.clone() {
                let parent_rc = parent_weak.upgrade().unwrap();
                if node == parent_rc.borrow().left {
                    let mut sibling = parent_rc.borrow().right.clone();
                    if Self::get_node_color(sibling.clone()) == Color::Red {
                        sibling.as_ref().unwrap().borrow_mut().color = Color::Black;
                        parent_rc.borrow_mut().color = Color::Red;
                        self.rotate_left(parent_rc.clone());
                        sibling = parent_rc.borrow().right.clone();
                    }
                    if Self::get_node_color(sibling.as_ref().unwrap().borrow().left.clone())
                        == Color::Black
                        && Self::get_node_color(sibling.as_ref().unwrap().borrow().right.clone())
                            == Color::Black
                    {
                        sibling.as_ref().unwrap().borrow_mut().color = Color::Red;
                        node = Some(parent_rc.clone());
                        parent = node.as_ref().unwrap().borrow().parent.clone();
                    } else {
                        if Self::get_node_color(sibling.as_ref().unwrap().borrow().right.clone())
                            == Color::Black
                        {
                            sibling
                                .as_ref()
                                .unwrap()
                                .borrow()
                                .left
                                .as_ref()
                                .unwrap()
                                .borrow_mut()
                                .color = Color::Black;
                            sibling.as_ref().unwrap().borrow_mut().color = Color::Red;
                            self.rotate_right(sibling.as_ref().unwrap().clone());
                            sibling = parent_rc.borrow().right.clone();
                        }
                        sibling.as_ref().unwrap().borrow_mut().color = parent_rc.borrow().color;
                        parent_rc.borrow_mut().color = Color::Black;
                        sibling
                            .as_ref()
                            .unwrap()
                            .borrow()
                            .right
                            .as_ref()
                            .unwrap()
                            .borrow_mut()
                            .color = Color::Black;
                        self.rotate_left(parent_rc.clone());
                        node = self.root.clone();
                        parent = None;
                    }
                } else {
                    let mut sibling = parent_rc.borrow().left.clone();
                    if Self::get_node_color(sibling.clone()) == Color::Red {
                        sibling.as_ref().unwrap().borrow_mut().color = Color::Black;
                        parent_rc.borrow_mut().color = Color::Red;
                        self.rotate_right(parent_rc.clone());
                        sibling = parent_rc.borrow().left.clone();
                    }
                    if Self::get_node_color(sibling.as_ref().unwrap().borrow().left.clone())
                        == Color::Black
                        && Self::get_node_color(sibling.as_ref().unwrap().borrow().right.clone())
                            == Color::Black
                    {
                        sibling.as_ref().unwrap().borrow_mut().color = Color::Red;
                        node = Some(parent_rc.clone());
                        parent = node.as_ref().unwrap().borrow().parent.clone();
                    } else {
                        if Self::get_node_color(sibling.as_ref().unwrap().borrow().left.clone())
                            == Color::Black
                        {
                            sibling
                                .as_ref()
                                .unwrap()
                                .borrow()
                                .right
                                .as_ref()
                                .unwrap()
                                .borrow_mut()
                                .color = Color::Black;
                            sibling.as_ref().unwrap().borrow_mut().color = Color::Red;
                            self.rotate_left(sibling.as_ref().unwrap().clone());
                            sibling = parent_rc.borrow().left.clone();
                        }
                        sibling.as_ref().unwrap().borrow_mut().color = parent_rc.borrow().color;
                        parent_rc.borrow_mut().color = Color::Black;
                        sibling
                            .as_ref()
                            .unwrap()
                            .borrow()
                            .left
                            .as_ref()
                            .unwrap()
                            .borrow_mut()
                            .color = Color::Black;
                        self.rotate_right(parent_rc.clone());
                        node = self.root.clone();
                        parent = None;
                    }
                }
            } else {
                break;
            }
        }
        if let Some(node_rc) = node {
            node_rc.borrow_mut().color = Color::Black;
        }
    }

    fn minimum_node(node: NodeRef<K, usize>) -> NodeRef<K, usize> {
        let mut current = node;
        loop {
            let left_option = {
                let current_ref = current.borrow();
                current_ref.left.clone()
            };
            match left_option {
                Some(left) => current = left,
                None => break,
            }
        }
        current
    }

    fn get_node_by_key(
        &self,
        key: &K,
        current_node: Option<NodeRef<K, usize>>,
    ) -> Option<NodeRef<K, usize>> {
        if let Some(node) = current_node {
            match node.borrow().key.cmp(key) {
                std::cmp::Ordering::Less => self.get_node_by_key(key, node.borrow().right.clone()),
                std::cmp::Ordering::Greater => {
                    self.get_node_by_key(key, node.borrow().left.clone())
                }
                std::cmp::Ordering::Equal => Some(node.clone()),
            }
        } else {
            None
        }
    }

    fn get_value(&self, node_option: Option<NodeRef<K, usize>>) -> Option<&V> {
        let index = node_option.map(|node| node.borrow().value.clone());
        if let Some(index) = index {
            self.memory.access(index)
        } else {
            None
        }
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        self.get_value(self.get_node_by_key(key, self.root.clone()))
    }
}

// Output

impl<K, V> Tree<K, V>
where
    K: Ord + Display,
    V: Display + Clone,
{
    pub fn dump_tree(&self) {
        if let Some(root) = &self.root {
            self.print_node(root.clone(), "", true);
        } else {
            println!("(Empty)");
        }
    }

    fn print_node(&self, node: NodeRef<K, usize>, prefix: &str, is_tail: bool) {
        let node_ref = node.borrow();

        println!(
            "{}{}─ [{}, {}] ({:?})",
            prefix,
            if is_tail { "└" } else { "├" },
            node_ref.key,
            self.memory.access(node_ref.value).unwrap(),
            node_ref.color
        );

        let mut children = Vec::new();
        if node_ref.left.is_some() {
            children.push((node_ref.left.clone().unwrap(), false));
        }
        if node_ref.right.is_some() {
            children.push((node_ref.right.clone().unwrap(), true));
        }

        for (_, (child, is_right)) in children.into_iter().enumerate() {
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
        V: Clone,
    {
        fn test_root_black(&self) -> bool {
            if let Some(root) = self.root.clone() {
                let root_ref = root.borrow();
                root_ref.color == Color::Black
            } else {
                true
            }
        }

        fn test_black_nodes_helper(
            &self,
            node: Option<NodeRef<K, usize>>,
            current_count: usize,
            count: &mut Option<usize>,
        ) -> bool {
            if let Some(node) = node {
                let current_count = if node.borrow().color == Color::Black {
                    current_count + 1
                } else {
                    current_count
                };
                self.test_black_nodes_helper(node.borrow().left.clone(), current_count, count)
                    && self.test_black_nodes_helper(
                        node.borrow().right.clone(),
                        current_count,
                        count,
                    )
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

        fn get_depth_helper(
            &self,
            node: Option<NodeRef<K, usize>>,
            current_depth: usize,
            depth_count: &mut usize,
            leaf_count: &mut usize,
        ) {
            if let Some(node) = node {
                self.get_depth_helper(
                    node.borrow().left.clone(),
                    current_depth + 1,
                    depth_count,
                    leaf_count,
                );
                self.get_depth_helper(
                    node.borrow().right.clone(),
                    current_depth + 1,
                    depth_count,
                    leaf_count,
                );
            } else {
                *leaf_count += 1;
                *depth_count += current_depth;
            }
        }

        // Retuns avg depth of tree (e.g. sum(leaf_depth) / N)
        fn get_depth(&self) -> f64 {
            let mut depths_count: usize = 0;
            let mut leafs_count: usize = 0;
            self.get_depth_helper(self.root.clone(), 0, &mut depths_count, &mut leafs_count);
            match (depths_count, leafs_count) {
                (0, _) | (_, 0) => 0.0,
                (_, _) => depths_count as f64 / leafs_count as f64,
                
            }
        }
    }

    #[test]
    fn test_depth() {
        let mut tree = Tree::<i32, i32>::new();

        let mut depths: Vec<f64> = vec![];

        const MAX_DEPTH: usize = 10;

        for it in 0..MAX_DEPTH {
            for jt in (1 << it)..(1 << (it + 1)) {
                tree.insert(jt, jt);
            }
            depths.push(tree.get_depth());
        }

        let expected_depths: Vec<f64> = (1..=MAX_DEPTH).map(|x| x as f64).collect();
        let mean_expected: f64 = expected_depths.iter().sum::<f64>() / expected_depths.len() as f64;
        let ss_tot: f64 = expected_depths.iter().map(|&x| (x - mean_expected).powi(2)).sum();
        let ss_res: f64 = depths.iter().zip(expected_depths.iter()).map(|(&d, &e)| (d - e).powi(2)).sum();
        let r2: f64 = 1.0 - (ss_res / ss_tot);

        assert!(r2 > 0.95);
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

        assert_eq!(*tree.get(&8).unwrap(), 16);
        assert_eq!(*tree.get(&100).unwrap(), 300);
        assert_eq!(tree.get(&200), None);

        tree.insert(8, 228);
        assert_eq!(*tree.get(&8).unwrap(), 228);
    }

    #[test]
    fn test_delete() {
        let mut tree = Tree::<i32, i32>::new();

        for it in 0..128 {
            tree.insert(it, it * 3);
        }

        for it in 0..128 {
            assert!(*tree.get(&it).unwrap() == it * 3);
            tree.delete(&it);
            assert!(tree.get(&it).is_none());
        }

        tree.insert(0, 0);
        tree.delete(&0);
        tree.delete(&0);

        assert!(tree.root.is_none());
    }
}