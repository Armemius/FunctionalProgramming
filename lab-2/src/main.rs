use node::Tree;

mod node;

fn main() {
    let mut tree = Tree::<i32, i32>::new();

    for it in (0..128).rev() {
        tree.insert(it, it);
    }
    // tree.insert(14, "g");

    tree.dump_tree();
}