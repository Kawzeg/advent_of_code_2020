#[macro_use]
extern crate lazy_static;
use regex::Regex;

use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::{self, prelude::*, BufReader, Error, ErrorKind};


#[derive(Debug)]
enum Bag {
    Empty,
    Contains { bags: HashMap<String, u32> }
}

fn parse_bag(line: &String) -> io::Result<(&str, Bag)> {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"^(\w+ \w+) bags contain").unwrap();
        static ref CONTAIN_RE: Regex = Regex::new(r"(\d+) (\w+ \w+) bag").unwrap();
    }
    let key = RE.captures(line)
        .ok_or(Error::new(ErrorKind::Other, "Line doesn't match"))?
        .get(1).unwrap().as_str();
    let mut bags = HashMap::new();
    for cap in CONTAIN_RE.captures_iter(line) {
        if key == "shiny gold" {
            println!("Parsing shiny gold");
            println!("Cap: {:?}", cap);
        }
        let num = cap.get(1).unwrap().as_str().parse::<u32>().unwrap();
        let color = cap.get(2).unwrap().as_str().to_string();
        if key == "shiny gold" {
            println!("Parsing shiny gold");
            println!("Inserting: {} {}", num, color);

        }
        bags.insert(color, num);
    }
    if bags.is_empty() {
        Ok((key, Bag::Empty))
    } else {
        Ok((key, Bag::Contains{bags}))
    }
}

fn find_size(color: &str, all_bags: &HashMap<&str, Bag>) -> u32 {
    let bag = all_bags.get(color).unwrap();
    println!("Checking Bag: {} {:?}", color, bag);
    match bag {
        Bag::Empty => 0,
        Bag::Contains{bags} => {
            let mut sum = 0;
            for (bag, num) in bags {
                sum += num * (1 + find_size(bag, &all_bags));
            }
            sum
        }
    }
}

fn add_colors<'a>(v: &mut Vec<&'a str>, s: &'a String) {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"(\w+ \w+) bags contain").unwrap();
    }
    for cap in RE.captures_iter(s) {
        let new_color = cap.get(1).unwrap().as_str();
        v.push(new_color);
    }
}

fn main() -> io::Result<()> {
    let file = File::open("./resources/input")?;
    let mut targets = HashSet::new();
    targets.insert("shiny gold");

    let lines: Vec<String> = BufReader::new(file).lines()
        .map(|x| x.unwrap())
        .collect();
    loop {
        let mut new_targets = vec![];
        for line in &lines {
            for target in &targets {
                if line.contains(target) && !line.starts_with(target) {
                    add_colors(&mut new_targets, &line);
                }
            }
        }
        let old_len = targets.len();
        targets.extend(new_targets);
        let new_targets = targets.len() - old_len;
        if new_targets == 0 {
            break;
        }

    }
    println!("Colors: {:?}", targets);
    // Shiny gold bags are not in shiny gold bags, but they are in targets
    println!("Part 1: {}", targets.len() - 1);

    // Part 2
    let mut bags = HashMap::new();

    for line in &lines {
        let (key, bag) = parse_bag(&line)?;
        if key == "shiny gold" {
            println!("Shiny gold bag: {:?}", bag);
        }
        bags.insert(key, bag);
    }

    println!("Part 2: {}", find_size("shiny gold", &bags));

    Ok(())
}
