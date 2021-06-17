use crate::chapter03::adventure_game::dynamic_type::Obj;
use crate::chapter03::adventure_game::objects::person;
use crate::chapter03::adventure_game::objects::person::is_person;
use crate::chapter03::adventure_game::property_table::Properties;
use crate::chapter03::generic_procedures::predicate::declare_superset;
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
}
