use crate::chapter03::adventure_game::generic_procedures::SEND_MESSAGE;
use crate::chapter03::adventure_game::objects::container::get_things;
use crate::chapter03::adventure_game::objects::exit::get_direction;
use crate::chapter03::adventure_game::objects::person;
use crate::chapter03::adventure_game::objects::person::{
    exits_here, get_bag, is_person, people_here, things_here, vistas_here,
};
use crate::chapter03::adventure_game::objects::screen::{is_message, tell};
use crate::chapter03::adventure_game::objects::thing::get_location;
use crate::chapter03::adventure_game::property_table::Properties;
use crate::chapter03::dynamic_type::{obj, Obj};
use crate::chapter03::generic_procedures::predicate::declare_superset;
use crate::chapter03::generic_procedures::{
    define_generic_procedure_handler, match_args, GenericResult,
};
use crate::chapter03::DebugAny;

pub fn is_avatar(obj: &dyn DebugAny) -> bool {
    person::is_person(obj) && obj.has_property("screen")
}

pub fn make_avatar(name: impl ToString, location: Obj, screen: Obj) -> Obj {
    let obj = person::make_person(name, location);
    obj.set_property("screen", screen);
    obj
}

pub fn install_generic_procedure_handlers() {
    declare_superset(is_avatar, is_person);

    define_generic_procedure_handler(
        &SEND_MESSAGE,
        match_args(&[is_message, is_avatar]),
        |args| {
            let screen = args[1].get_property("screen").unwrap();
            SEND_MESSAGE(&[args[0], &screen])
        },
    );
}

pub fn look_around(avatar: &Obj) -> GenericResult {
    assert!(is_avatar(&**avatar));

    tell(&obj(vec![obj("You are in"), get_location(avatar)]), avatar)?;

    let my_bag = get_bag(avatar);
    let my_things = get_things(&my_bag);
    if !my_things.is_empty() {
        tell(
            &obj(vec![obj("Your bag contains:"), obj(my_things)]),
            avatar,
        )?;
    }

    let mut things = things_here(avatar);
    things.extend(people_here(avatar));
    if !things.is_empty() {
        tell(&obj(vec![obj("You see here:"), obj(things)]), avatar)?;
    }

    let vistas = vistas_here(avatar);
    if !vistas.is_empty() {
        tell(&obj(vec![obj("You can see:"), obj(vistas)]), avatar)?;
    }

    let exits = exits_here(avatar);
    tell(
        &obj(if exits.is_empty() {
            vec![
                obj("There are no exits..."),
                obj("you are dead and gone to heaven!"),
            ]
        } else {
            let directions: Vec<_> = exits.iter().map(get_direction).collect();
            vec![obj("You can exit:"), obj(directions)]
        }),
        avatar,
    )
}
