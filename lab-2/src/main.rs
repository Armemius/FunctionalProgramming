use node::Tree;

mod node;

fn main() {
    let mut tree = Tree::<i32, &str>::new();

    tree.insert(8, "a");
    tree.insert(4, "b");
    tree.insert(12, "c");
    tree.insert(2, "d");
    tree.insert(6, "e");
    tree.insert(10, "f");
    tree.insert(14, "g");

    tree.dump_tree();
}