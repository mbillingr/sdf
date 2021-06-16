use crate::chapter03::adventure_game::dynamic_type::Obj;
use crate::chapter03::adventure_game::objects::autonomous_agent;
use crate::chapter03::adventure_game::property_table::Properties;
use crate::chapter03::DebugAny;

fn is_troll(obj: &dyn DebugAny) -> bool {
    autonomous_agent::is_autonomous_agent(obj) && obj.has_property("hunger")
}

fn make_troll(name: impl ToString, place: Obj, restlessness: f32, hunger: f32) -> Obj {
    let obj = autonomous_agent::make_autonomous_agent(name, place, restlessness, 1.0 / 10.0);
    obj.set_raw_property("hunger", hunger);
    obj
}
