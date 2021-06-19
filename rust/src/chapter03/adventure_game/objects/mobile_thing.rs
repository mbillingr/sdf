use crate::chapter03::adventure_game::generic_procedures::GENERIC_MOVE;
use crate::chapter03::adventure_game::objects::person::is_person;
use crate::chapter03::adventure_game::objects::place::is_place;
use crate::chapter03::adventure_game::objects::thing;
use crate::chapter03::adventure_game::objects::thing::is_thing;
use crate::chapter03::adventure_game::property_table::Properties;
use crate::chapter03::dynamic_type::Obj;
use crate::chapter03::generic_procedures::predicate::declare_superset;
use crate::chapter03::generic_procedures::{define_generic_procedure_handler, match_args};
use crate::chapter03::DebugAny;

pub fn is_mobile_thing(obj: &dyn DebugAny) -> bool {
    thing::is_thing(obj) && obj.has_property("origin")
}

pub fn make_mobile_thing(name: impl ToString, location: Obj) -> Obj {
    let obj = thing::make_thing(name, location.clone());
    obj.set_property("origin", location);
    return obj;
}

pub fn install_generic_procedure_handlers() {
    define_generic_procedure_handler(
        &GENERIC_MOVE,
        match_args(&[is_person, is_place, is_place, is_person]),
        |_args| unimplemented!(),
    );

    declare_superset(is_mobile_thing, is_thing);
}
