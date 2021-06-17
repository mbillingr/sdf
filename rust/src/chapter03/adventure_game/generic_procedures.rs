use crate::chapter03::generic_procedures::dispatch_store::make_most_specific_dispatch_store;
use crate::chapter03::generic_procedures::{
    make_generic_procedure_constructor, GenericFn, Handler, SimpleDispatchStore,
};
use lazy_static::lazy_static;
use std::sync::Arc;

lazy_static! {
    pub static ref generic_procedure: Arc<dyn Send + Sync + Fn(&str, Option<Handler>) -> GenericFn> =
        Arc::new(make_generic_procedure_constructor(SimpleDispatchStore::new));
    pub static ref most_specific_generic_procedure: Arc<dyn Send + Sync + Fn(&str, Option<Handler>) -> GenericFn> =
        Arc::new(make_generic_procedure_constructor(
            make_most_specific_dispatch_store
        ));
    pub static ref generic_move: GenericFn = most_specific_generic_procedure("generic-move", None);
    pub static ref send_message: GenericFn = most_specific_generic_procedure("send-message", None);
}
