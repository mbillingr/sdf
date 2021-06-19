use crate::chapter03::generic_procedures::predicate::Predicate;
use crate::chapter03::generic_procedures::{Applicability, GenericArgs, GenericResult, Handler};
use crate::chapter03::DebugAny;
use std::cmp::Ordering;
use std::sync::Arc;

pub trait DispatchStore: 'static + Send + Sync {
    fn add_handler(&mut self, applicability: Applicability, handler: Handler);
    fn get_handler(&self, args: GenericArgs) -> Option<Handler>;

    fn set_default_handler(&mut self, handler: Handler);
    fn get_default_handler(&self) -> &Handler;
}

fn make_default_handler(name: String) -> Handler {
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
}

pub struct SimpleDispatchStore {
    rules: Vec<(Vec<Predicate>, Handler)>,
    default_handler: Handler,
}

impl SimpleDispatchStore {
    pub fn new() -> Self {
        SimpleDispatchStore {
            rules: vec![],
            default_handler: make_default_handler("unknown".to_string()),
        }
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
            .all(|(pred, arg)| pred.check(&***arg))
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

    fn get_handler(&self, args: GenericArgs) -> Option<Handler> {
        self.rules
            .iter()
            .find(|(predicates, _)| Self::predicates_match(predicates, args))
            .map(|(_, handler)| handler)
            .cloned()
    }

    fn set_default_handler(&mut self, handler: Handler) {
        self.default_handler = handler;
    }
    fn get_default_handler(&self) -> &Handler {
        &self.default_handler
    }
}

pub struct SubsettingDispatchStore {
    delegate: SimpleDispatchStore,
    choose_handler: Arc<Send + Sync + Fn(Vec<&Handler>, &Handler) -> Handler>,
}

impl SubsettingDispatchStore {
    pub fn new(
        choose_handler: impl 'static + Send + Sync + Fn(Vec<&Handler>, &Handler) -> Handler,
    ) -> Self {
        Self {
            delegate: SimpleDispatchStore::new(),
            choose_handler: Arc::new(choose_handler),
        }
    }
}

impl DispatchStore for SubsettingDispatchStore {
    fn add_handler(&mut self, applicability: Applicability, handler: Handler) {
        self.delegate.add_handler(applicability, handler)
    }

    fn get_handler(&self, args: GenericArgs) -> Option<Handler> {
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
            Some((self.choose_handler)(handlers, self.get_default_handler()))
        }
    }

    fn set_default_handler(&mut self, handler: Handler) {
        self.delegate.set_default_handler(handler)
    }
    fn get_default_handler(&self) -> &Handler {
        self.delegate.get_default_handler()
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
    SubsettingDispatchStore::new(|handlers, _default_handler| handlers[0].clone())
}

pub fn make_chaining_dispatch_store() -> impl DispatchStore {
    SubsettingDispatchStore::new(|handlers, default_handler| {
        let default_handler = default_handler.clone();
        let handlers: Vec<_> = handlers.into_iter().rev().cloned().collect();

        Arc::new(move |args| {
            let mut result = default_handler(args)?;
            for handler in &handlers {
                result = handler(args)?;
            }
            Ok(result)
        })
    })
}
