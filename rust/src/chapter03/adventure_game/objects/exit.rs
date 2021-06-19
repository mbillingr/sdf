use crate::chapter03::adventure_game::objects::object;
use crate::chapter03::adventure_game::objects::object::is_object;
use crate::chapter03::adventure_game::property_table::Properties;
use crate::chapter03::adventure_game::Direction;
use crate::chapter03::dynamic_type::Obj;
use crate::chapter03::generic_procedures::predicate::declare_superset;
use crate::chapter03::DebugAny;

pub fn is_exit(obj: &dyn DebugAny) -> bool {
    object::is_object(obj)
        && obj.has_property("from")
        && obj.has_property("to")
        && obj.has_property("direction")
}

pub fn make_exit(from: Obj, to: Obj, direction: Direction) -> Obj {
    let obj = object::make_object(String::new());
    obj.set_property("from", from);
    obj.set_property("to", to);
    obj.set_raw_property("direction", direction);
    return obj;
}

pub fn get_direction(exit: &Obj) -> Direction {
    assert!(is_exit(&**exit));
    let direction = exit.get_property("direction").unwrap();
    *direction.downcast_ref().unwrap()
}

pub fn install_generic_procedure_handlers() {
    declare_superset(is_exit, is_object);
}
