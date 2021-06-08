use std::any::Any;

pub trait Object: Any + std::fmt::Debug {
    fn as_any(&self) -> &dyn Any;
}

impl<T: Any + std::fmt::Debug> Object for T {
    fn as_any(&self) -> &dyn Any {
        self
    }
}
