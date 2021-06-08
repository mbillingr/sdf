use crate::chapter03::generic_procedures::object::Object;

pub type PredicateFn = fn(&dyn Object) -> bool;

#[derive(Copy, Clone)]
pub struct Predicate(PredicateFn);

impl From<PredicateFn> for Predicate {
    fn from(func: PredicateFn) -> Self {
        Predicate(func)
    }
}

impl PartialEq for Predicate {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(&self.0, &other.0)
    }
}

impl Predicate {
    pub fn check(&self, obj: &dyn Object) -> bool {
        (self.0)(obj)
    }
}
