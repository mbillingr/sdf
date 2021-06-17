use crate::chapter03::adventure_game::dynamic_type::{as_table, Obj};
use crate::chapter03::adventure_game::generic_procedures::send_message;
use crate::chapter03::adventure_game::objects::object::{is_object, make_object};
use crate::chapter03::adventure_game::property_table::Properties;
use crate::chapter03::generic_procedures::predicate::declare_superset;
use crate::chapter03::generic_procedures::{define_generic_procedure_handler, match_args};
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
        &send_message,
        match_args(&[is_message, is_screen]),
        |args| {
            display_message(args[0], &*args[1].get_property("port").unwrap());
            Ok(None)
        },
    );

    declare_superset(is_screen, is_object);
}

fn is_message(obj: &dyn DebugAny) -> bool {
    obj.downcast_ref::<Vec<Obj>>().is_some()
}

fn as_message(obj: &dyn DebugAny) -> Option<&[Obj]> {
    obj.downcast_ref::<Vec<Obj>>().map(Vec::as_slice)
}

fn display_message(message: &dyn DebugAny, _port: &dyn DebugAny) {
    let message = as_message(message).unwrap();
    for item in message {
        display_item(item);
        print!(" ");
    }
    println!();
}

fn display_item(item: &Obj) {
    if is_object(&**item) {
        let name = as_table(&**item).unwrap().get_property("name").unwrap();
        display_item(&name);
    } else if let Some(s) = as_str(&**item) {
        print!("{}", s);
    } else {
        print!("{:?}", item);
    }
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
