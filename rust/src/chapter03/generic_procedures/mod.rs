macro_rules! obj {
    (mut $trait:path) => { Arc<RwLock<dyn $trait + Send + Sync>> };
    ($trait:path) => { Arc<dyn $trait + Send + Sync> };

    (new mut $value:expr) => { Arc::new(RwLock::new($value)) };
}

mod dispatch_store;
mod metadata;
mod object;
pub mod predicate;

use crate::chapter03::generic_procedures::metadata::{
    get_generic_metadata, ConcreteMetadata, Metadata,
};
use crate::chapter03::generic_procedures::predicate::Predicate;
use dispatch_store::DispatchStore;
pub use dispatch_store::SimpleDispatchStore;
use object::Object;
use predicate::PredicateFn;
use std::sync::{Arc, RwLock};

type GenericFn = Arc<dyn 'static + Sync + Send + Fn(GenericArgs<'_, '_>) -> GenericResult>;
type GenericResult = Result<Arc<dyn Object>, Error>;
type GenericArgs<'a, 'o> = &'a [&'o dyn Object];

type Handler = GenericFn;
type Applicability = Vec<Vec<Predicate>>;
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

        metadata::set_generic_metadata(&the_generic_procedure, metadata);

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
    get_generic_metadata(generic_procedure)
        .write()
        .unwrap()
        .add_handler(applicability, handler);
}

pub fn all_args(arity: usize, predicate: PredicateFn) -> Applicability {
    vec![vec![predicate.into(); arity]]
}

#[cfg(test)]
mod tests {
    use crate::chapter03::generic_procedures::dispatch_store::SimpleDispatchStore;

    use super::*;

    fn is_i64(obj: &dyn Object) -> bool {
        obj.as_any().downcast_ref::<i64>().is_some()
    }

    fn is_string(obj: &dyn Object) -> bool {
        obj.as_any().downcast_ref::<String>().is_some()
    }

    #[test]
    fn it_works() {
        let generic_procedure = make_generic_procedure_constructor(SimpleDispatchStore::new);

        let plus = generic_procedure("plus", None);
        define_generic_procedure_handler(&plus, all_args(2, is_i64), |args| match args {
            [a, b] => {
                let a = (*a).as_any().downcast_ref::<i64>().unwrap();
                let b = (*b).as_any().downcast_ref::<i64>().unwrap();
                let c: i64 = a + b;
                Ok(Arc::new(c))
            }
            _ => Err(Arc::new("wrong number of arguments")),
        });

        assert_eq!(
            (*plus(&[&1i64, &2i64]).ok().unwrap())
                .as_any()
                .downcast_ref::<i64>(),
            Some(&3)
        );

        assert!(plus(&[&"foo".to_string(), &"bar".to_string()]).is_err());

        define_generic_procedure_handler(&plus, all_args(2, is_string), |args| match args {
            [a, b] => {
                let a = (*a).as_any().downcast_ref::<String>().unwrap();
                let b = (*b).as_any().downcast_ref::<String>().unwrap();
                Ok(Arc::new(a.clone() + b))
            }
            _ => Err(Arc::new("wrong number of arguments")),
        });

        assert_eq!(
            (*plus(&[&"foo".to_string(), &"bar".to_string()])
                .ok()
                .unwrap())
            .as_any()
            .downcast_ref::<String>()
            .unwrap(),
            "foobar"
        );
    }
}
