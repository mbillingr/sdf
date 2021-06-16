use crate::chapter03::adventure_game::dynamic_type::Obj;
use crate::chapter03::adventure_game::objects::object;
use crate::chapter03::adventure_game::property_table::Properties;
use crate::chapter03::adventure_game::Direction;
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
