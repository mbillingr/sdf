use crate::chapter03::adventure_game::dynamic_type::Obj;
use crate::chapter03::adventure_game::objects::person;
use crate::chapter03::adventure_game::property_table::Properties;
use crate::chapter03::DebugAny;

fn is_avatar(obj: &dyn DebugAny) -> bool {
    person::is_person(obj) && obj.has_property("screen")
}

fn make_avator(name: impl ToString, location: Obj, screen: Obj) -> Obj {
    let obj = person::make_person(name, location);
    obj.set_property("screen", screen);
    obj
}
