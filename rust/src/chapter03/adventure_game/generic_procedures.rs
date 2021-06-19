use crate::chapter03::adventure_game::Direction;
use crate::chapter03::dynamic_type::{obj, Obj};
use crate::chapter03::generic_procedures::dispatch_store::{
    make_chaining_dispatch_store, make_most_specific_dispatch_store,
};
use crate::chapter03::generic_procedures::{
    define_generic_procedure_handler, make_generic_procedure_constructor, match_args, GenericArgs,
    GenericFn, GenericResult, Handler, SimpleDispatchStore,
};
use crate::chapter03::DebugAny;
use lazy_static::lazy_static;
use std::sync::Arc;

lazy_static! {
    pub static ref GENERIC_PROCEDURE: Arc<dyn Send + Sync + Fn(&str, Option<Handler>) -> GenericFn> =
        Arc::new(make_generic_procedure_constructor(SimpleDispatchStore::new));
    pub static ref MOST_SPECIFIC_GENERIC_PROCEDURE: Arc<dyn Send + Sync + Fn(&str, Option<Handler>) -> GenericFn> =
        Arc::new(make_generic_procedure_constructor(
            make_most_specific_dispatch_store
        ));
    pub static ref CHAINING_GENERIC_PROCEDURE: Arc<dyn Send + Sync + Fn(&str, Option<Handler>) -> GenericFn> =
        Arc::new(make_generic_procedure_constructor(
            make_chaining_dispatch_store
        ));
    pub static ref DEBUG_FORMAT: GenericFn = MOST_SPECIFIC_GENERIC_PROCEDURE(
        "debug-format",
        Some(Arc::new(debug_format_default_handler))
    );
    pub static ref CLOCK_TICK: GenericFn =
        CHAINING_GENERIC_PROCEDURE("clock-tick", Some(Arc::new(|_| Ok(None))));
    pub static ref SEND_MESSAGE: GenericFn = MOST_SPECIFIC_GENERIC_PROCEDURE("send-message", None);
    pub static ref GENERIC_MOVE: GenericFn = MOST_SPECIFIC_GENERIC_PROCEDURE("generic-move", None);
}

fn debug_format_default_handler(args: GenericArgs) -> GenericResult {
    //Ok(Some(obj(format!("{:#?}", args[0]))))
    Ok(Some(obj(format!("---"))))
}

pub fn install_generic_procedure_handlers() {
    define_generic_procedure_handler(&DEBUG_FORMAT, match_args(&[is_a::<&str>]), |args| {
        Ok(Some(obj(format!(
            "{}",
            args[0].downcast_ref::<&str>().unwrap()
        ))))
    });

    define_generic_procedure_handler(&DEBUG_FORMAT, match_args(&[is_a::<String>]), |args| {
        Ok(Some(obj(format!(
            "{}",
            args[0].downcast_ref::<String>().unwrap()
        ))))
    });

    define_generic_procedure_handler(&DEBUG_FORMAT, match_args(&[is_a::<Vec<Obj>>]), |args| {
        let items: Vec<_> = args[0]
            .downcast_ref::<Vec<Obj>>()
            .unwrap()
            .iter()
            .map(|x| {
                DEBUG_FORMAT(&[x])
                    .map_err(|e| e.to_string())
                    .unwrap()
                    .unwrap()
            })
            .map(|x| x.downcast_ref::<String>().unwrap().clone())
            .collect();
        Ok(Some(obj(format!("{}", items.join(", ")))))
    });

    define_generic_procedure_handler(&DEBUG_FORMAT, match_args(&[is_a::<Direction>]), |args| {
        Ok(Some(obj(format!(
            "{:?}",
            args[0].downcast_ref::<Direction>().unwrap()
        ))))
    });

    define_generic_procedure_handler(
        &DEBUG_FORMAT,
        match_args(&[is_a::<Vec<Direction>>]),
        |args| {
            let items: Vec<_> = args[0]
                .downcast_ref::<Vec<Direction>>()
                .unwrap()
                .iter()
                .map(|x| format!("{:?}", x))
                .collect();
            Ok(Some(obj(format!("{}", items.join(", ")))))
        },
    );
}

fn is_a<T: 'static>(obj: &dyn DebugAny) -> bool {
    obj.downcast_ref::<T>().is_some()
}
