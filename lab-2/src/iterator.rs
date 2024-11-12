use std::{cell::RefCell, rc::Rc};

use super::{node::Node, tree::Tree};

pub struct TreeIterator<'a, K, V>
where
    K: Ord + Clone,
    V: Clone,
{
    stack: Vec<Rc<RefCell<Node<K, usize>>>>,
    current: Option<Rc<RefCell<Node<K, usize>>>>,
    tree: &'a Tree<K, V>,
}

impl<'a, K, V> TreeIterator<'a, K, V>
where
    K: Ord + Clone,
    V: Clone,
{
    pub fn new(tree: &'a Tree<K, V>) -> Self {
        TreeIterator {
            stack: Vec::new(),
            current: tree.root.clone(),
            tree,
        }
    }
}

impl<'a, K, V> Iterator for TreeIterator<'a, K, V>
where
    K: Ord + Clone,
    V: Clone,
{
    type Item = (K, V);

    /// Returns the next item in the in-order traversal.
    fn next(&mut self) -> Option<Self::Item> {
        // Traverse to the leftmost node.
        while let Some(current) = self.current.clone() {
            self.stack.push(current.clone());
            self.current = current.borrow().left.clone();
        }

        // If the stack is empty, traversal is complete.
        if self.stack.is_empty() {
            return None;
        }

        // Pop the top node from the stack.
        let node_rc = self.stack.pop().unwrap();
        let node_ref = node_rc.borrow();

        // Retrieve the key and value from the current node.
        let key = node_ref.key.clone();
        let value = self.tree.memory.access(node_ref.value)?.clone();

        // Move to the right subtree.
        self.current = node_ref.right.clone();

        Some((key, value))
    }
}

impl<'a, K, V> IntoIterator for &'a Tree<K, V>
where
    K: Ord + Clone,
    V: Clone,
{
    type Item = (K, V);
    type IntoIter = TreeIterator<'a, K, V>;

    fn into_iter(self) -> Self::IntoIter {
        TreeIterator::new(self)
    }
}

#[cfg(test)]
mod tests {
    use crate::tree::Tree;

    #[test]
    fn test_iterator() {
        let mut tree = Tree::<i32, String>::new();

        tree = tree.insert(5, "five".to_string());
        tree = tree.insert(3, "three".to_string());
        tree = tree.insert(7, "seven".to_string());
        tree = tree.insert(2, "two".to_string());
        tree = tree.insert(4, "four".to_string());
        tree = tree.insert(6, "six".to_string());
        tree = tree.insert(8, "eight".to_string());

        for (key, value) in &tree {
            let res = match key {
                1 => "one",
                2 => "two",
                3 => "three",
                4 => "four",
                5 => "five",
                6 => "six",
                7 => "seven",
                8 => "eight",
                9 => "nine",
                _ => "n/a",
            };

            assert!(res == value);
        }
    }

    #[test]
    fn test_filter() {
        let mut tree_a = Tree::<i32, i32>::new();
        tree_a = tree_a.insert(1, 1);
        tree_a = tree_a.insert(2, 2);
        tree_a = tree_a.insert(3, 3);
        tree_a = tree_a.insert(4, 4);
        tree_a = tree_a.insert(5, 5);

        let mut tree_b = Tree::<i32, i32>::new();
        tree_b = tree_b.insert(1, 1);
        tree_b = tree_b.insert(3, 3);
        tree_b = tree_b.insert(5, 5);

        assert!(tree_a
            .into_iter()
            .filter(|(_, value)| value % 2 != 0)
            .eq(tree_b.into_iter()));
    }

    #[test]
    fn test_map() {
        let mut tree_a = Tree::<i32, i32>::new();

        tree_a = tree_a.insert(1, 1);
        tree_a = tree_a.insert(2, 2);
        tree_a = tree_a.insert(3, 3);

        let mut tree_b = Tree::<i32, i32>::new();

        tree_b = tree_b.insert(1, 1);
        tree_b = tree_b.insert(2, 4);
        tree_b = tree_b.insert(3, 9);

        assert!(tree_a
            .into_iter()
            .map(|(key, value)| (key, value.pow(2)))
            .eq(tree_b.into_iter()));
    }

    #[test]
    fn test_fold() {
        let mut tree_a = Tree::<i32, i32>::new();

        tree_a = tree_a.insert(1, 1);
        tree_a = tree_a.insert(2, 2);
        tree_a = tree_a.insert(3, 3);

        assert_eq!(
            tree_a.into_iter().fold(0, |prev, (_, value)| prev + value),
            6
        );
    }

    #[test]
    #[allow(unused_must_use)]
    fn test_lazy() {
        let mut tree_a = Tree::<i32, i32>::new();

        tree_a = tree_a.insert(1, 1);
        for it in 2..128 {
            tree_a = tree_a.insert(it, it);
        }

        tree_a.into_iter().map(|(_, value)| {
            if value > 1 {
                panic!("This expression should not be evaluated");
            }
        });
    }
}
