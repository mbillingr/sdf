use crate::chapter03::adventure_game::generic_procedures::{DEBUG_FORMAT, SEND_MESSAGE};
use crate::chapter03::adventure_game::objects::object::{is_object, make_object};
use crate::chapter03::adventure_game::property_table::Properties;
use crate::chapter03::dynamic_type::{as_table, Obj};
use crate::chapter03::generic_procedures::predicate::declare_superset;
use crate::chapter03::generic_procedures::{
    define_generic_procedure_handler, match_args, GenericResult,
};
use crate::chapter03::DebugAny;

pub fn is_screen(obj: &dyn DebugAny) -> bool {
    is_object(obj) && obj.has_property("port")
}

pub fn make_screen() -> Obj {
    let obj = make_object("the-screen");
    obj.set_raw_property("port", ());
    obj
}

pub fn install_generic_procedure_handlers() {
    define_generic_procedure_handler(
        &SEND_MESSAGE,
        match_args(&[is_message, is_screen]),
        |args| {
            display_message(args[0], &*args[1].get_property("port").unwrap());
            Ok(None)
        },
    );

    declare_superset(is_screen, is_object);
}

pub fn tell(message: &Obj, person: &Obj) -> GenericResult {
    SEND_MESSAGE(&[message, person])
}

pub fn is_message(obj: &dyn DebugAny) -> bool {
    obj.downcast_ref::<Vec<Obj>>().is_some()
}

fn as_message(obj: &Obj) -> Option<&[Obj]> {
    obj.downcast_ref::<Vec<Obj>>().map(Vec::as_slice)
}

fn display_message(message: &Obj, _port: &dyn DebugAny) {
    let message = as_message(message).unwrap();
    for item in message {
        let repr = DEBUG_FORMAT(&[item])
            .map_err(|e| e.to_string())
            .unwrap()
            .unwrap();
        let repr = repr.downcast_ref::<String>().unwrap();
        print!("{} ", repr);
    }
    println!();
}

fn as_str(item: &dyn DebugAny) -> Option<&str> {
    if let Some(s) = item.downcast_ref::<String>() {
        return Some(s);
    }
    if let Some(s) = item.downcast_ref::<&str>() {
        return Some(s);
    }
    None
}
