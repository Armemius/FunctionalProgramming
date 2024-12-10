use proptest::prelude::*;
use std::rc::Rc;

#[derive(Debug, Clone, Default)]
pub enum Tree<T>
where
    T: PartialOrd + Clone,
{
    #[default] 
    Leaf,
    Node {
        value: T,
        left: Rc<Tree<T>>,
        right: Rc<Tree<T>>,
    },
}

impl<T> Tree<T>
where
    T: PartialOrd + Clone,
{
    pub fn insert(&self, value: T) -> Self {
        match self {
            Tree::Node {
                value: curr_value,
                left,
                right,
            } => {
                if value > *curr_value {
                    Tree::Node {
                        value: curr_value.clone(),
                        left: left.clone(),
                        right: Rc::new(right.insert(value)),
                    }
                } else if value < *curr_value {
                    Tree::Node {
                        value: curr_value.clone(),
                        left: Rc::new(left.insert(value)),
                        right: right.clone(),
                    }
                } else {
                    Tree::Node {
                        value: curr_value.clone(),
                        left: left.clone(),
                        right: right.clone(),
                    }
                }
            }
            Tree::Leaf => Tree::Node {
                value,
                left: Rc::new(Tree::Leaf),
                right: Rc::new(Tree::Leaf),
            },
        }
    }

    pub fn merge(&self, other: &Self) -> Self {
        match other {
            Tree::Leaf => self.clone(),
            Tree::Node { value, left, right } => {
                self.merge(left).merge(right).insert(value.clone())
            }
        }
    }
}

impl<T> PartialEq for Tree<T>
where
    T: PartialOrd + Clone,
{
    fn eq(&self, other: &Self) -> bool {
        fn collect_values<T: PartialOrd + Clone>(tree: &Tree<T>, values: &mut Vec<T>) {
            match tree {
                Tree::Leaf => {}
                Tree::Node { value, left, right } => {
                    collect_values(left, values);
                    values.push(value.clone());
                    collect_values(right, values);
                }
            }
        }

        let mut self_values = Vec::new();
        let mut other_values = Vec::new();

        collect_values(self, &mut self_values);
        collect_values(other, &mut other_values);

        self_values == other_values
    }
}

proptest! {
    #[test]
    fn test_merge_associativity(a_val in any::<Vec<i32>>(),
                                b_val in any::<Vec<i32>>(),
                                c_val in any::<Vec<i32>>()) {
        let mut a = Tree::<i32>::default();
        let mut b = Tree::<i32>::default();
        let mut c = Tree::<i32>::default();

        for value in a_val {
            a = a.insert(value);
        }
        for value in b_val {
            b = b.insert(value);
        }
        for value in c_val {
            c = c.insert(value);
        }

        let left_associative = a.merge(&b).merge(&c);
        let right_associative = a.merge(&b.merge(&c));

        assert_eq!(left_associative, right_associative);
    }

    #[test]
    fn test_neutral_element(val in any::<Vec<i32>>()) {
        let mut tree = Tree::<i32>::default();

        for value in val {
            tree = tree.insert(value);
        }

        assert_eq!(tree, tree.merge(&Tree::default()));
        
    }

    #[test]
    fn test_funcional_insert(val in any::<Vec<i32>>(), new_val in any::<i32>()) {
        let mut tree = Tree::<i32>::default();

        for value in val {
            tree = tree.insert(value);
        }

        let old_tree = tree.clone();
        tree.insert(new_val);

        assert_eq!(tree, old_tree);
    }

    #[test]
    fn test_funcional_merge(a_val in any::<Vec<i32>>(), b_val in any::<Vec<i32>>()) {
        let mut a = Tree::<i32>::default();
        let mut b = Tree::<i32>::default();

        for value in a_val {
            a = a.insert(value);
        }
        for value in b_val {
            b = b.insert(value);
        }

        let old_a = a.clone();
        a.merge(&b);

        assert_eq!(a, old_a);
    }
}
