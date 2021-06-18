use crate::chapter03::adventure_game::{
    dynamic_type,
    dynamic_type::Obj,
    generic_procedures::DEBUG_FORMAT,
    property_table::{Properties, Table},
};
use crate::chapter03::generic_procedures::{define_generic_procedure_handler, match_args};
use crate::chapter03::DebugAny;
use std::sync::Arc;

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

pub fn install_generic_procedure_handlers() {
    define_generic_procedure_handler(&DEBUG_FORMAT, match_args(&[is_object]), |args| {
        let name = args[0].get_property("name").unwrap();
        let name = name.downcast_ref::<String>().unwrap();
        Ok(Some(Arc::new(format!("{}", name))))
    });
}
