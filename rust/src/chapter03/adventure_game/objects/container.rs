use crate::chapter03::adventure_game::dynamic_type::Obj;
use crate::chapter03::adventure_game::objects::object;
use crate::chapter03::adventure_game::property_table::Properties;
use crate::chapter03::DebugAny;

pub fn is_container(obj: &dyn DebugAny) -> bool {
    object::is_object(obj) && obj.has_property("things")
}

pub fn make_container(name: impl ToString) -> Obj {
    let obj = object::make_object(name);
    obj.set_raw_property("things", Vec::<Obj>::new());
    return obj;
}
