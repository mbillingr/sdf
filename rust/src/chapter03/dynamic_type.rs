use crate::chapter03::adventure_game::generic_procedures::DEBUG_FORMAT;
use crate::chapter03::adventure_game::property_table::Table;
use crate::chapter03::DebugAny;
use std::any::TypeId;
use std::fmt;
use std::ops::Deref;
use std::sync::Arc;

#[derive(Debug, Clone)]
pub struct Obj(Arc<dyn DebugAny>);

pub fn obj<T: DebugAny>(x: T) -> Obj {
    assert_ne!(TypeId::of::<T>(), TypeId::of::<Obj>());
    Obj(Arc::new(x))
}

impl Deref for Obj {
    type Target = dyn DebugAny;
    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}

impl PartialEq for Obj {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}
/*
impl fmt::Debug for Obj {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if f.alternate() {
            write!(f, "{:?}", self.0)
        }
        else if let Ok(Some(str_obj)) = DEBUG_FORMAT(&[self]) {
            let fmt_str = str_obj
                .downcast_ref::<&str>()
                .copied()
                .or_else(|| str_obj.downcast_ref::<String>().map(|s| s.as_str()))
                .unwrap();
            write!(f, "{}", fmt_str)
        } else {
            unreachable!()
        }
    }
}*/

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
