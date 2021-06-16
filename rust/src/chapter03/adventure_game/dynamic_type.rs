use std::sync::Arc;

use crate::chapter03::adventure_game::property_table::Table;
use crate::chapter03::DebugAny;

pub type Obj = Arc<dyn DebugAny>;

pub fn obj<T: DebugAny>(x: T) -> Obj {
    Arc::new(x)
}

pub fn as_table(obj: &dyn DebugAny) -> Option<&Table> {
    let result = obj.downcast_ref::<Table>();
    if result.is_none() {
        debug_assert!(
            obj.downcast_ref::<Obj>().is_none(),
            "Found a <&dyn Obj> type. Did you forget to dereference the Obj?"
        )
    }
    result
}
