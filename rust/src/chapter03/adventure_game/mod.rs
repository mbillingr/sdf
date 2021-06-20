//! Allright!
//! Generic procedures are kinda weird to implement in Rust, but the result is somewhat OK-ish.
//! Using the type system to implement a hierarchy of structures is painful. Maybe implementing
//! these types as runtime entities would have been a better approach. The real pain, however, is
//! in combining the type hierarchy, which is not native to rust with dynamic types and
//! multifunctions. Implementing generic procedures first, turned out to be a bad idea. When trying
//! to make them as generic as possible (taking `&dyn Any` arguments) I blocked my to passing
//! owned game objects on from generic functions. It was also easy to make mistakes such as
//! forgetting to dereference a game object, which would result in dispatch failures.
//! I won't pursue this further in Rust.
//! Implementing multiple dispatch and property types in Python could be useful. I'll try that next.
//! I wonder how all this would fare in Julia with its first-class multiple-dispatch...

use std::collections::HashMap;
use std::io::Write;
use std::sync::Arc;

use rand::{thread_rng, Rng};

use objects::{exit, place};

use crate::chapter03::adventure_game::clock::Clock;
use crate::chapter03::adventure_game::objects::avatar::{look_around, make_avatar};
use crate::chapter03::adventure_game::objects::mobile_thing::{is_mobile_thing, make_mobile_thing};
use crate::chapter03::adventure_game::objects::place::{add_vista, is_place};
use crate::chapter03::adventure_game::objects::screen::make_screen;
use crate::chapter03::adventure_game::objects::student::make_student;
use crate::chapter03::adventure_game::objects::thing::make_thing;
use crate::chapter03::adventure_game::property_table::Properties;
use crate::chapter03::dynamic_type::Obj;
use crate::chapter03::generic_procedures::GenericResult;

pub mod clock;
pub mod generic_procedures;
pub mod objects;
pub mod property_table;

#[derive(Debug, Copy, Clone)]
pub enum Direction {
    North,
    South,
    East,
    West,
    In,
    Out,
    Up,
    Down,
    Skew,
}

pub fn random_bias(x: i64) -> f64 {
    1.0 / thread_rng().gen_range(1..=x) as f64
}

pub fn flip_coin(bias: f64) -> bool {
    thread_rng().gen::<f64>() >= bias
}

pub fn random_choice<T>(items: Vec<T>) -> T {
    unimplemented!()
}

pub fn connect(place1: &Obj, d1: Direction, d2: Direction, place2: &Obj) {
    assert!(place::is_place(&**place1));
    assert!(place::is_place(&**place2));
    let exit12 = exit::make_exit(place1.clone(), place2.clone(), d1);
    let exit21 = exit::make_exit(place2.clone(), place1.clone(), d2);
    place::add_exit(place1, exit12);
    place::add_exit(place2, exit21);
}

pub fn connect_one_way(place1: &Obj, d1: Direction, place2: &Obj) {
    assert!(place::is_place(&**place1));
    assert!(place::is_place(&**place2));
    let exit12 = exit::make_exit(place1.clone(), place2.clone(), d1);
    place::add_exit(place1, exit12);
}

pub fn move_internal(mobile_thing: &Obj, from: &Obj, to: &Obj) {
    assert!(is_mobile_thing(&**mobile_thing));
    assert!(is_place(&**from));
    assert!(is_place(&**to));
    /*leave_place(mobile_thing);
    remove_thing(from, mobile_thing);
    mobile_thing.set_property("location", to.clone());
    add_thing(to, mobile_thing);
    enter_place(mobile_thing);*/
    unimplemented!()
}

pub struct AdventureGame {
    my_avatar: Obj,
    clock: Clock,
    commands: HashMap<&'static str, CommandHandler>,
}

impl AdventureGame {
    pub fn new(my_name: &str) -> Self {
        let mut clock = Clock::new();
        let all_places = create_world(&mut clock);
        let start_location = all_places
            .iter()
            .find(|place| {
                let p = place.get_property("name").unwrap();
                p.downcast_ref::<String>().unwrap() == "Lobby"
            })
            .cloned()
            .unwrap();
        let screen = make_screen();
        Self {
            my_avatar: make_avatar(my_name, start_location, screen),
            clock,
            commands: Self::make_commands(),
        }
    }

    pub fn repl(&mut self) {
        self.whats_here().map_err(|e| e.to_string()).unwrap();
        loop {
            print!("\n> ");
            std::io::stdout().flush().unwrap();
            let mut buffer = String::new();
            std::io::stdin().read_line(&mut buffer).unwrap();
            let cmd = self.parse_command(&buffer);
            match self.invoke_command(cmd[0], &cmd[1..]) {
                Ok(_) => {}
                Err(e) => println!("{}", e.to_string()),
            }
        }
    }

    fn parse_command<'a>(&self, buffer: &'a str) -> Vec<&'a str> {
        let cmd: Vec<_> = buffer.trim().split(char::is_whitespace).collect();
        cmd
    }

    pub fn invoke_command(&mut self, cmd: &str, args: &[&str]) -> GenericResult {
        match self.commands.get(cmd) {
            Some(CommandHandler::Nullary(handler)) if args.is_empty() => handler(self),
            Some(CommandHandler::Nullary(_)) => Err(Arc::new("expected no arguments")),
            Some(CommandHandler::Unary(handler)) if args.len() == 1 => handler(self, args[0]),
            Some(CommandHandler::Unary(_)) => Err(Arc::new("expected one argument")),
            None => Err(Arc::new("unknown command")),
        }
    }

    fn make_commands() -> HashMap<&'static str, CommandHandler> {
        let mut commands = HashMap::new();
        commands.insert("whats-here", CommandHandler::Nullary(Self::whats_here));
        commands.insert("hang-out", CommandHandler::Unary(Self::hang_out_cmd));
        commands
    }

    pub fn whats_here(&mut self) -> GenericResult {
        look_around(&self.my_avatar)
    }

    pub fn go_cmd(&mut self, direction: &str) -> GenericResult {
        unimplemented!()
    }

    pub fn go(&mut self, direction: Direction) -> GenericResult {
        unimplemented!()
    }

    pub fn hang_out_cmd(&mut self, ticks: &str) -> GenericResult {
        let ticks = ticks
            .parse::<u64>()
            .map_err(|e| -> Arc<dyn ToString> { Arc::new(e) })?;
        self.hang_out(ticks)
    }

    pub fn hang_out(&mut self, ticks: u64) -> GenericResult {
        for _ in 0..ticks {
            self.clock.tick()?;
        }
        Ok(None)
    }
}

enum CommandHandler {
    Nullary(fn(&mut AdventureGame) -> GenericResult),
    Unary(fn(&mut AdventureGame, &str) -> GenericResult),
}

fn create_world(clock: &mut Clock) -> Vec<Obj> {
    use Direction::*;

    let lobby = place::make_place("Lobby");
    let restroom = place::make_place("Restroom");
    let infinite_corridor = place::make_place("Infinite Corridor");
    let mountains = place::make_place("distant mountains");

    add_vista(&lobby, mountains.clone());

    connect(&lobby, East, West, &restroom);
    connect_one_way(&lobby, West, &infinite_corridor);
    connect(&infinite_corridor, North, South, &infinite_corridor);

    make_thing("pot-plant", lobby.clone());
    make_thing("reception-desk", lobby.clone());
    make_mobile_thing("pen", lobby.clone());

    let student = make_student("Student", lobby.clone(), 0.1, 0.1, clock);

    vec![lobby, restroom, infinite_corridor]
}
