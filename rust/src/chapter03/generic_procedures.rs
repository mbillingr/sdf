use lazy_static::lazy_static;
use std::any::Any;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::sync::{Arc, RwLock, Weak};

macro_rules! obj {
    (mut $trait:path) => { Arc<RwLock<dyn $trait + Send + Sync>> };
    ($trait:path) => { Arc<dyn $trait + Send + Sync> };

    (new mut $value:expr) => { Arc::new(RwLock::new($value)) };
}

pub trait Object: Any + std::fmt::Debug {
    fn as_any(&self) -> &dyn Any;
}
impl<T: Any + std::fmt::Debug> Object for T {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

type GenericFn = Arc<dyn 'static + Sync + Send + Fn(GenericArgs<'_, '_>) -> GenericResult>;
type GenericResult = Result<Arc<dyn Object>, Error>;
type GenericArgs<'a, 'o> = &'a [&'o dyn Object];

type Handler = GenericFn;

type Applicability = Vec<Vec<Predicate>>;
type PredicateFn = fn(&dyn Object) -> bool;

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
    fn check(&self, obj: &dyn Object) -> bool {
        (self.0)(obj)
    }
}

type Error = Arc<dyn ToString>;

pub fn make_generic_procedure_constructor<T: DispatchStore>(
    dispatch_store_maker: impl 'static + Fn() -> T,
) -> impl Fn(&str, Option<Handler>) -> GenericFn {
    let constructor = move |name: &str, default_handler| {
        let metadata =
            ConcreteMetadata::new(name.to_string(), dispatch_store_maker(), default_handler);
        let metadata: obj![mut Metadata] = obj!(new mut metadata);

        let the_generic_procedure: GenericFn = {
            let metadata = metadata.clone();
            Arc::new(move |args| metadata.read().unwrap().dispatch(args))
        };

        set_generic_metadata(&the_generic_procedure, metadata);

        the_generic_procedure
    };
    constructor
}

pub fn define_generic_procedure_handler(
    generic_procedure: &GenericFn,
    applicability: Applicability,
    handler: impl 'static + Sync + Send + Fn(GenericArgs<'_, '_>) -> GenericResult,
) {
    let handler = Arc::new(handler);
    let key = MetaKey(Arc::downgrade(generic_procedure));
    METADATA.write().unwrap()[&key]
        .write()
        .unwrap()
        .add_handler(applicability, handler);
}

trait Metadata {
    fn add_handler(&mut self, applicability: Applicability, handler: Handler);
    fn dispatch(&self, args: GenericArgs) -> GenericResult;
}

#[derive(Clone)]
struct ConcreteMetadata<T: DispatchStore> {
    dispatch_store: T,
    default_handler: Handler,
}

impl<T: DispatchStore> ConcreteMetadata<T> {
    pub fn new(name: String, dispatch_store: T, default_handler: Option<Handler>) -> Self {
        let default_handler = default_handler.unwrap_or_else(|| {
            Arc::new(move |args| {
                Err(Arc::new(format!(
                    "Cannot apply {} to ({})",
                    name,
                    args.iter()
                        .map(|a| format!("{:?}", a))
                        .collect::<Vec<_>>()
                        .join(", ")
                )))
            })
        });

        ConcreteMetadata {
            dispatch_store,
            default_handler,
        }
    }

    pub fn default_handler(&self, args: GenericArgs) -> GenericResult {
        (self.default_handler)(args)
    }
}

impl<T: DispatchStore> Metadata for ConcreteMetadata<T> {
    fn add_handler(&mut self, applicability: Applicability, handler: Handler) {
        self.dispatch_store.add_handler(applicability, handler)
    }

    fn dispatch(&self, args: GenericArgs) -> GenericResult {
        match self.dispatch_store.get_handler(args) {
            Some(handler) => handler(args),
            None => self.default_handler(args),
        }
    }
}

fn set_generic_metadata(genfn: &GenericFn, metadata: obj![mut Metadata]) {
    let key = MetaKey(Arc::downgrade(genfn));
    METADATA.write().unwrap().insert(key, metadata);
}

lazy_static! {
    static ref METADATA: RwLock<HashMap<MetaKey, obj![mut Metadata]>> = RwLock::new(HashMap::new());
}

struct MetaKey(Weak<dyn Sync + Send + Fn(GenericArgs<'_, '_>) -> GenericResult>);

impl MetaKey {
    fn as_ptr(&self) -> *const (dyn Sync + Send + Fn(GenericArgs<'_, '_>) -> GenericResult) {
        Weak::as_ptr(&self.0)
    }
}

impl Eq for MetaKey {}
impl PartialEq for MetaKey {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.as_ptr(), other.as_ptr())
    }
}

impl Hash for MetaKey {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_ptr().hash(state)
    }
}

pub trait DispatchStore: 'static + Send + Sync {
    fn add_handler(&mut self, applicability: Applicability, handler: Handler);
    fn get_handler(&self, args: GenericArgs) -> Option<&Handler>;
}

struct SimpleDispatchStore {
    rules: Vec<(Vec<Predicate>, Handler)>,
}

impl SimpleDispatchStore {
    pub fn new() -> Self {
        SimpleDispatchStore { rules: vec![] }
    }
    fn predicates_match(predicates: &[Predicate], args: GenericArgs) -> bool {
        if args.len() != predicates.len() {
            return false;
        }

        predicates
            .iter()
            .zip(args)
            .all(|(pred, arg)| pred.check(*arg))
    }
}

impl DispatchStore for SimpleDispatchStore {
    fn add_handler(&mut self, applicability: Applicability, handler: Handler) {
        for rule in applicability {
            self.rules.push((rule, handler.clone()))
        }
    }

    fn get_handler(&self, args: GenericArgs) -> Option<&Handler> {
        for (predicates, handler) in &self.rules {
            if Self::predicates_match(predicates, args) {
                return Some(handler);
            }
        }
        None
    }
}

fn all_args(arity: usize, predicate: PredicateFn) -> Applicability {
    vec![vec![predicate.into(); arity]]
}

fn is_i64(obj: &dyn Object) -> bool {
    obj.as_any().downcast_ref::<i64>().is_some()
}

fn is_string(obj: &dyn Object) -> bool {
    obj.as_any().downcast_ref::<String>().is_some()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let generic_procedure = make_generic_procedure_constructor(SimpleDispatchStore::new);

        let plus = generic_procedure("plus", None);
        define_generic_procedure_handler(&plus, all_args(2, is_i64), |args| match args {
            [a, b] => {
                let a = (*a).as_any().downcast_ref::<i64>().unwrap();
                let b = (*b).as_any().downcast_ref::<i64>().unwrap();
                Ok(Arc::new(a + b))
            }
            _ => Err(Arc::new("wrong number of arguments")),
        });

        println!("{:?}", plus(&[&1i64, &2i64]).map_err(|e| e.to_string()));

        println!(
            "{:?}",
            plus(&[&"foo".to_string(), &"bar".to_string()]).map_err(|e| e.to_string())
        );

        define_generic_procedure_handler(&plus, all_args(2, is_string), |args| match args {
            [a, b] => {
                let a = (*a).as_any().downcast_ref::<String>().unwrap();
                let b = (*b).as_any().downcast_ref::<String>().unwrap();
                Ok(Arc::new(a.clone() + b))
            }
            _ => Err(Arc::new("wrong number of arguments")),
        });

        println!(
            "{:?}",
            plus(&[&"foo".to_string(), &"bar".to_string()]).map_err(|e| e.to_string())
        );
    }
}
