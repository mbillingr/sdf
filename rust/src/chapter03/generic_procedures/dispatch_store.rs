use crate::chapter03::generic_procedures::predicate::Predicate;
use crate::chapter03::generic_procedures::{Applicability, GenericArgs, Handler};

pub trait DispatchStore: 'static + Send + Sync {
    fn add_handler(&mut self, applicability: Applicability, handler: Handler);
    fn get_handler(&self, args: GenericArgs) -> Option<&Handler>;
}

pub struct SimpleDispatchStore {
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
        self.rules.extend(
            applicability
                .into_iter()
                .map(|rule| (rule, handler.clone())),
        )
    }

    fn get_handler(&self, args: GenericArgs) -> Option<&Handler> {
        self.rules
            .iter()
            .find(|(predicates, _)| Self::predicates_match(predicates, args))
            .map(|(_, handler)| handler)
    }
}
