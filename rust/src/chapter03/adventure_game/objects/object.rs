use crate::chapter03::adventure_game::dynamic_type;
use crate::chapter03::adventure_game::dynamic_type::Obj;
use crate::chapter03::adventure_game::property_table::{Properties, Table};
use crate::chapter03::DebugAny;

pub fn is_object(obj: &dyn DebugAny) -> bool {
    dynamic_type::as_table(obj).is_some()
        && obj.has_property("name")
        && obj.has_property("description")
}

pub fn make_object(name: impl ToString) -> Obj {
    let name = name.to_string();
    let table = Table::new();
    table.set_raw_property("name", name.clone());
    table.set_raw_property("description", name);
    return dynamic_type::obj(table);
}

pub fn install_generic_procedure_handlers() {}
