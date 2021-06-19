use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::sync::{Arc, RwLock, Weak};

use lazy_static::lazy_static;

use crate::chapter03::generic_procedures::dispatch_store::DispatchStore;
use crate::chapter03::generic_procedures::{
    Applicability, GenericArgs, GenericFn, GenericResult, Handler,
};

type MetaStore = HashMap<MetaKey, obj![mut Metadata]>;

lazy_static! {
    static ref METADATA: RwLock<MetaStore> = RwLock::new(HashMap::new());
}

pub trait Metadata {
    fn add_handler(&mut self, applicability: Applicability, handler: Handler);
    fn dispatch(&self, args: GenericArgs) -> GenericResult;
}

#[derive(Clone)]
pub struct ConcreteMetadata<T: DispatchStore> {
    dispatch_store: T,
}

impl<T: DispatchStore> ConcreteMetadata<T> {
    pub fn new(name: String, mut dispatch_store: T, default_handler: Option<Handler>) -> Self {
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

        dispatch_store.set_default_handler(default_handler);

        ConcreteMetadata { dispatch_store }
    }

    pub fn default_handler(&self, args: GenericArgs) -> GenericResult {
        (self.dispatch_store.get_default_handler())(args)
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

pub fn set_generic_metadata(genfn: &GenericFn, metadata: obj![mut Metadata]) {
    let key = MetaKey::new(genfn);
    METADATA.write().unwrap().insert(key, metadata);
}

pub fn get_generic_metadata(genfn: &GenericFn) -> obj![mut Metadata] {
    let key = MetaKey::new(genfn);
    METADATA.read().unwrap()[&key].clone()
}

pub struct MetaKey(Weak<dyn Sync + Send + Fn(GenericArgs<'_, '_>) -> GenericResult>);

impl MetaKey {
    pub fn new(generic_procedure: &GenericFn) -> Self {
        MetaKey(Arc::downgrade(generic_procedure))
    }

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
