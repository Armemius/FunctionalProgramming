use node::Tree;

mod node;

fn main() {
    let mut tree = Tree::<i32, i32>::new();

    for it in 0..4 {
        tree.insert(it, it);
    }

    let key = 8;

    println!("{:?}", tree.get(&key));

    tree.dump_tree();
}