use crate::chapter03::adventure_game::dynamic_type::{obj, Obj};
use crate::chapter03::adventure_game::generic_procedures::SEND_MESSAGE;
use crate::chapter03::adventure_game::objects::person;
use crate::chapter03::adventure_game::objects::person::is_person;
use crate::chapter03::adventure_game::objects::screen::{is_message, tell};
use crate::chapter03::adventure_game::objects::thing::get_location;
use crate::chapter03::adventure_game::property_table::Properties;
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
            SEND_MESSAGE(&[args[0], &*screen])
        },
    );
}

pub fn look_around(avatar: &Obj) -> GenericResult {
    assert!(is_avatar(&**avatar));
    tell(&obj(vec![obj("You are in"), get_location(avatar)]), avatar)?;
    unimplemented!()
}
