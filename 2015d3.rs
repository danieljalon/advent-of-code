use std::env;
use std::fs;
use std::collections::HashMap;


#[derive(Eq, Hash, PartialEq, Copy, Clone)]
struct Coords {
    x : i32,
    y : i32,
}

impl Coords {
}

fn update_location(loc : Coords, dir : char, houses : &mut HashMap<Coords, u32>) -> Option<Coords> {
    let new_loc : Coords;
    match dir {
        '^' => new_loc = Coords {x: loc.x, y: loc.y + 1},
        '>' => new_loc = Coords {x: loc.x + 1, y: loc.y},
        'v' => new_loc = Coords {x: loc.x, y: loc.y - 1},
        '<' => new_loc = Coords {x: loc.x - 1, y: loc.y},
        _ => return None,
    }
    let times = houses.entry(new_loc).or_insert(0);
    *times += 1;

    Some(new_loc)
}


fn main() {
    let path = "2015d3.input";
    let input = fs::read_to_string(path).unwrap();

    let mut santa_at = Coords {x : 0, y : 0};
    let mut houses = HashMap::<Coords, u32>::new();
    houses.insert(santa_at, 1);

    let part = &env::args().collect::<Vec<_>>()[1];
    match part.as_str() {
        "a" => {
            for ch in input.chars() {
                if let Some(at) = update_location(santa_at, ch, &mut houses) {
                    santa_at = at;
                };
            }
            println!("Santa has visited {} houses!", houses.len());
        },
        "b" => {
            let mut robot_at = Coords {x : 0, y : 0};
            let times = houses.entry(robot_at).or_insert(0);
            *times += 1;

            let mut iter = input.chars();
            while let (Some(ch1), Some(ch2)) = (iter.next(), iter.next()) {
                if let Some(at) = update_location(santa_at, ch1, &mut houses) {
                    santa_at = at;
                }
                if let Some(at) = update_location(robot_at, ch2, &mut houses) {
                    robot_at = at;
                }
            }
            println!("Santa and Robo-Santa have visited {} houses!", houses.len());
        },
        _ => panic!("Invalid parameter. Must be 'a' or 'b'"),
    }
}
