use std::fs;

fn main() {
    let path = "2015d2.input";

    let mut paper_order = 0;
    let mut ribbon_order = 0;
    for line in fs::read_to_string(path).unwrap().lines() {
        let mut v = line.split('x').map(|x| x.parse().unwrap()).collect::<Vec<u32>>();
        v.sort_unstable();

        paper_order += 3 * v[0] * v[1] + 2 * v[0] * v[2] + 2 * v[1] * v[2];
        ribbon_order += 2 * v[0] + 2 * v[1] + v[0] * v[1] * v[2];
    }

    println!("Elves should order {paper_order} square feet of wrapping paper and {ribbon_order} feet of ribbon!");
}
