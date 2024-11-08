use rb_tree::Tree;

mod rb_tree;

fn main() {
    let mut tree = Tree::<i32, i32>::new();

    for it in 0..16 {
        tree = tree.insert(it, it);
    }

    tree = tree.delete(&3);
    // println!("{:?}", tree.get(&2));

    tree.dump_tree();
}