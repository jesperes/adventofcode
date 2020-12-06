use std::io::{self, BufRead};

fn main() {
    let reader = io::stdin();
    let vec : Vec<i32> =
        reader.lock().lines()
        .map(|s| s.unwrap().parse::<i32>().unwrap())
        .collect();

    for x in &vec {
        for y in &vec {
            if x < y && x + y == 2020 {
                println!("Part 1: {}", x * y);
            }

            for z in &vec {
                if x < y && y < z && x + y + z == 2020 {
                    println!("Part 2: {}", x * y * z);
                }
            }
        }
    }
}

#[test]
fn main_test() {

}
