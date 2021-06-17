use crate::chapter03::DebugAny;
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::sync::RwLock;

pub type PredicateFn = fn(&dyn DebugAny) -> bool;

#[derive(Copy, Clone)]
pub struct Predicate(PredicateFn);

impl From<PredicateFn> for Predicate {
    fn from(func: PredicateFn) -> Self {
        Predicate(func)
    }
}

impl Eq for Predicate {}
impl PartialEq for Predicate {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.0 as *const PredicateFn, other.0 as *const PredicateFn)
    }
}

impl Hash for Predicate {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let ptr = self.0 as *const PredicateFn;
        ptr.hash(state)
    }
}

impl Predicate {
    pub fn check(&self, obj: &dyn DebugAny) -> bool {
        if (self.0)(obj) {
            return true;
        }

        SUBSETS
            .read()
            .unwrap()
            .get(self)
            .map(|subs| subs.iter().any(|s| s.check(obj)))
            .unwrap_or(false)
    }

    pub fn declare_superset(&self, sup: PredicateFn) {
        let sup = Predicate(sup);
        SUBSETS
            .write()
            .unwrap()
            .entry(sup)
            .or_insert(vec![])
            .push(self.clone());
    }

    pub fn is_sub_predicate(&self, sup: &Predicate) -> bool {
        if self == sup {
            return true;
        }

        SUBSETS
            .read()
            .unwrap()
            .get(sup)
            .map(|subs| subs.iter().any(|s| self.is_sub_predicate(s)))
            .unwrap_or(false)
    }
}

pub fn declare_superset(sub: PredicateFn, sup: PredicateFn) {
    let sub = Predicate(sub);
    let sup = Predicate(sup);
    if sub != sup {
        SUBSETS
            .write()
            .unwrap()
            .entry(sup)
            .or_insert(vec![])
            .push(sub);
    }
}

pub fn is_sub_predicate(a: PredicateFn, b: PredicateFn) -> bool {
    Predicate(a).is_sub_predicate(&Predicate(b))
}

lazy_static! {
    static ref SUBSETS: RwLock<HashMap<Predicate, Vec<Predicate>>> = RwLock::new(HashMap::new());
}

#[cfg(test)]
mod tests {
    use super::*;

    fn is_integer(obj: &dyn DebugAny) -> bool {
        obj.downcast_ref::<i64>().is_some()
            || obj.downcast_ref::<u64>().is_some()
            || obj.downcast_ref::<i32>().is_some()
            || obj.downcast_ref::<u32>().is_some()
            || obj.downcast_ref::<i16>().is_some()
            || obj.downcast_ref::<u16>().is_some()
            || obj.downcast_ref::<i8>().is_some()
            || obj.downcast_ref::<u8>().is_some()
    }

    fn is_even(obj: &dyn DebugAny) -> bool {
        obj.downcast_ref::<EvenInteger>().is_some()
    }

    #[derive(Debug)]
    struct EvenInteger(i64);
    impl EvenInteger {
        pub fn new(x: i64) -> Option<Self> {
            if x % 2 == 0 {
                Some(EvenInteger(x))
            } else {
                None
            }
        }
    }

    #[test]
    fn predicate_relationships() {
        let x: &dyn DebugAny = &EvenInteger::new(42).unwrap();

        assert!(Predicate(is_even).check(x));

        assert!(!Predicate(is_integer).check(x));

        declare_superset(is_even, is_integer);

        assert!(Predicate(is_integer).check(x));
    }
}
