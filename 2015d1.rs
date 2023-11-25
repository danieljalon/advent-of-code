use std::fs;

fn main() {
    let path = "2015d1.input";
    let input = fs::read_to_string(path).unwrap();

    let mut floor = 0;
    let mut basement_reached : Option<usize> = None;

    for (index, ch) in input.chars().enumerate() {
        if ch == '(' {
            floor += 1;
        } else if ch == ')' {
            floor -= 1;
        }
        if floor == -1 && basement_reached.is_none() {
            println!("Reached {floor} @ {index}!");
            basement_reached = Some(index + 1);
        }
    }

    println!("Santa must go to floor {floor}!");
    match basement_reached {
        None => println!("Santa did not pass through the basement"),
        Some(pos) => println!("Santa passed through the basement @ position {pos}"),
    }
}
