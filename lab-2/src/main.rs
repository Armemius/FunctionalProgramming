use rb_tree::Tree;

mod rb_tree;

fn main() {
    let mut tree = Tree::<i32, String>::new();

    tree = tree.insert(5, "five".to_string());
    tree = tree.insert(3, "three".to_string());
    tree = tree.insert(7, "seven".to_string());
    tree = tree.insert(2, "two".to_string());
    tree = tree.insert(4, "four".to_string());
    tree = tree.insert(6, "six".to_string());
    tree = tree.insert(8, "eight".to_string());

    for (key, value) in &tree {
        println!("Key: {}, Value: {}", key, value);
    }
}
