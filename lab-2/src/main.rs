use std::{cell::RefCell, fmt::Display, rc::Rc};

fn main() {
    let mut tree = Tree::<i32, i32>::new();

    tree.insert(10, 100);
    tree.insert(20, 200);
    tree.insert(5, 50);

    println!("{}", tree);
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Color {
    Red,
    Black,
}

type NodeRef<K, V> = Rc<RefCell<Node<K, V>>>;

struct Node<K: PartialOrd, V> {
    key: K,
    value: V,
    color: Color,
    left: Option<NodeRef<K, V>>,
    right: Option<NodeRef<K, V>>,
    parent: Option<NodeRef<K, V>>,
}

impl<K: PartialOrd + std::fmt::Debug, V: std::fmt::Debug> Display for Node<K, V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Node {{ key: {:?}, value: {:?}, color: {:?} }}",
            self.key, self.value, self.color
        )
    }
}

impl<K: PartialOrd, V> Node<K, V> {
    fn new(key: K, value: V) -> Self {
        Node {
            key,
            value,
            color: Color::Red,
            left: None,
            right: None,
            parent: None,
        }
    }
}

struct Tree<K: PartialOrd, V> {
    root: Option<NodeRef<K, V>>,
}

impl<K: PartialOrd + std::fmt::Debug, V: std::fmt::Debug> Tree<K, V> {
    fn print_node(
        f: &mut std::fmt::Formatter<'_>,
        node: &Option<NodeRef<K, V>>,
        depth: usize,
    ) -> std::fmt::Result {
        if let Some(node) = node {
            let node = node.borrow();
            writeln!(f, "{:indent$}{}", "", node, indent = depth * 4)?;

            Self::print_node(f, &node.left, depth + 1)?;
            Self::print_node(f, &node.right, depth + 1)?;
        }
        Ok(())
    }
}

impl<K: PartialOrd + std::fmt::Debug, V: std::fmt::Debug> Display for Tree<K, V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Tree:")?;
        Self::print_node(f, &self.root, 0)
    }
}

impl<K: PartialOrd, V> Tree<K, V> {
    fn new() -> Self {
        Tree { root: None }
    }

    fn insert(&mut self, key: K, value: V) {
        let new_node = Rc::new(RefCell::new(Node::new(key, value)));

        if let Some(root) = self.root.clone() {
            self.insert_node(root, new_node);
        } else {
            new_node.borrow_mut().color = Color::Black;
            self.root = Some(new_node);
        }
    }

    fn insert_node(&self, current: NodeRef<K, V>, new_node: NodeRef<K, V>) {
        let mut current_borrow = current.borrow_mut();
        if new_node.borrow().key < current_borrow.key {
            if let Some(left) = current_borrow.left.clone() {
                drop(current_borrow);
                self.insert_node(left, new_node);
            } else {
                current_borrow.left = Some(new_node.clone());
                new_node.borrow_mut().parent = Some(current.clone());
            }
        } else {
            if let Some(right) = current_borrow.right.clone() {
                drop(current_borrow);
                self.insert_node(right, new_node);
            } else {
                current_borrow.right = Some(new_node.clone());
                new_node.borrow_mut().parent = Some(current.clone());
            }
        }
    }
}
