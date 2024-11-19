use tree::Tree;

mod iterator;
mod memory;
mod node;
pub mod tree;

fn main() {
    let tree: Tree<i32, i32> = Tree::new()
        .insert(1, 1)
        .insert(2, 2)
        .insert(3, 3);
    let new_tree = tree.clone();

    let tree = tree.insert(2, 28);

    tree.dump_tree();
    new_tree.dump_tree();
}