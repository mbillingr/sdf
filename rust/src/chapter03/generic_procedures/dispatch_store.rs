use crate::chapter03::generic_procedures::predicate::Predicate;
use crate::chapter03::generic_procedures::{Applicability, GenericArgs, Handler};
use std::cmp::Ordering;

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

    pub fn get_rules(&self) -> &[(Vec<Predicate>, Handler)] {
        &self.rules
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

pub struct SubsettingDispatchStore {
    delegate: SimpleDispatchStore,
    choose_handler: fn(Vec<&Handler>) -> &Handler,
}

impl SubsettingDispatchStore {
    pub fn new(choose_handler: fn(Vec<&Handler>) -> &Handler) -> Self {
        Self {
            delegate: SimpleDispatchStore::new(),
            choose_handler,
        }
    }
}

impl DispatchStore for SubsettingDispatchStore {
    fn add_handler(&mut self, applicability: Applicability, handler: Handler) {
        self.delegate.add_handler(applicability, handler)
    }

    fn get_handler(&self, args: GenericArgs) -> Option<&Handler> {
        let mut matching: Vec<_> = self
            .delegate
            .get_rules()
            .iter()
            .filter(|(predicates, _)| SimpleDispatchStore::predicates_match(predicates, args))
            .collect();
        matching.sort_by(|(a, _), (b, _)| cmp_predicates(a, b));
        let handlers: Vec<_> = matching.into_iter().map(|(_, h)| h).collect();
        if handlers.is_empty() {
            None
        } else {
            Some((self.choose_handler)(handlers))
        }
    }
}

fn cmp_predicates(a: &[Predicate], b: &[Predicate]) -> Ordering {
    for (p1, p2) in a.iter().zip(b) {
        if p1 == p2 {
            continue;
        }
        if p1.is_sub_predicate(p2) {
            return Ordering::Less;
        }
        if p2.is_sub_predicate(p1) {
            return Ordering::Greater;
        }
    }
    Ordering::Equal
}

pub fn make_most_specific_dispatch_store() -> impl DispatchStore {
    SubsettingDispatchStore::new(|handlers| &handlers[0])
}

pub fn make_chaining_dispatch_store() -> impl DispatchStore {
    SubsettingDispatchStore::new(|_handlers| unimplemented!())
}
