use crate::chapter03::generic_procedures::dispatch_store::make_most_specific_dispatch_store;
use crate::chapter03::generic_procedures::{
    make_generic_procedure_constructor, GenericArgs, GenericFn, GenericResult, Handler,
    SimpleDispatchStore,
};
use lazy_static::lazy_static;
use std::sync::Arc;

lazy_static! {
    pub static ref GENERIC_PROCEDURE: Arc<dyn Send + Sync + Fn(&str, Option<Handler>) -> GenericFn> =
        Arc::new(make_generic_procedure_constructor(SimpleDispatchStore::new));
    pub static ref MOST_SPECIFIC_GENERIC_PROCEDURE: Arc<dyn Send + Sync + Fn(&str, Option<Handler>) -> GenericFn> =
        Arc::new(make_generic_procedure_constructor(
            make_most_specific_dispatch_store
        ));
    pub static ref DEBUG_FORMAT: GenericFn = MOST_SPECIFIC_GENERIC_PROCEDURE(
        "debug-format",
        Some(Arc::new(debug_format_default_handler))
    );
    pub static ref GENERIC_MOVE: GenericFn = MOST_SPECIFIC_GENERIC_PROCEDURE("generic-move", None);
    pub static ref SEND_MESSAGE: GenericFn = MOST_SPECIFIC_GENERIC_PROCEDURE("send-message", None);
}

fn debug_format_default_handler(args: GenericArgs) -> GenericResult {
    Ok(Some(Arc::new(format!("{:?}", args[0]))))
}
