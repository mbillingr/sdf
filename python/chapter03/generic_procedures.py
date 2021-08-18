import itertools

from chapter03.trie import Trie


def generic_procedure_constructor(dispatch_store_maker):
    def generic_procedure(name, arity, default_handler):
        metadata = Metadata(name, arity, dispatch_store_maker(),
                            default_handler or error_generic_procedure_handler(name))

        def the_generic_procedure(*args):
            return generic_procedure_dispatch(metadata, args)

        set_generic_procedure_metadata(the_generic_procedure, metadata)
        return the_generic_procedure

    return generic_procedure


def error_generic_procedure_handler(name):
    def handler(*args):
        raise TypeError(f"{name} not applicable to {args}")

    return handler


def generic_procedure_dispatch(metadata, args):
    handler = get_generic_procedure_handler(metadata, args)
    return handler(*args)


def get_generic_procedure_handler(metadata, args):
    return metadata.getter(args) or metadata.default_getter()


def define_generic_procedure_handler(generic_procedure, applicability, handler):
    generic_procedure_metadata(generic_procedure).dispatch_store.add_handler(applicability, handler)


class SimpleDispatchStore:
    def __init__(self):
        self.rules = {}
        self.default_handler = None

    def get_handler(self, args):
        for predicates, handler in self.rules.items():
            if predicates_match(predicates, args):
                return handler
        return None

    def add_handler(self, applicability, handler):
        for predicates in applicability:
            self.rules[predicates] = handler

    def get_default_handler(self):
        return self.default_handler

    def set_default_handler(self, handler):
        self.default_handler = handler


class TrieDispatchStore(SimpleDispatchStore):
    def __init__(self):
        super().__init__()
        self.trie = Trie()

    def get_handler(self, args):
        return self.trie.get_a_value(args)

    def add_handler(self, applicability, handler):
        super().add_handler(applicability, handler)
        for path in applicability:
            self.trie.set_path_value(path, handler)


def cache_wrap_dispatch_store(DispatchStore):
    class WrappedDispatchStore(DispatchStore):
        def __init__(self):
            super().__init__()
            self.cache = {}

        def get_handler(self, args):
            types = tuple([type(a) for a in args])
            if types in self.cache:
                return self.cache[types]
            else:
                h = super().get_handler(args)
                self.cache[types] = h
                return h

    return WrappedDispatchStore


simple_generic_procedure = generic_procedure_constructor(SimpleDispatchStore)


class Metadata:
    def __init__(self, name, arity, dispatch_store, default_handler):
        dispatch_store.set_default_handler(default_handler)
        self.name = name
        self.arity = arity
        self.dispatch_store = dispatch_store
        self.getter = dispatch_store.get_handler
        self.default_getter = dispatch_store.get_default_handler


GENERIC_PROCEDURE_METADATA = {}


def set_generic_procedure_metadata(procedure, metadata):
    GENERIC_PROCEDURE_METADATA[procedure] = metadata


def generic_procedure_metadata(procedure):
    return GENERIC_PROCEDURE_METADATA[procedure]


def predicates_match(predicates, args):
    return all(pred(arg) for pred, arg in zip(predicates, args))


def all_args(arity, predicate):
    return [(predicate,) * arity]


def any_arg(arity, predicate, base_predicate):
    return [(predicate,) * arity] + list(itertools.permutations((predicate, base_predicate), arity))


def match_args(*predicates):
    return [predicates]


if __name__ == '__main__':
    def is_number(obj):
        PREDICATE_COUNTS[is_number] += 1
        return isinstance(obj, int) or isinstance(obj, float)


    def is_symbolic(obj):
        PREDICATE_COUNTS[is_symbolic] += 1
        return isinstance(obj, str)


    PREDICATE_COUNTS = None


    def reset_predicate_counts():
        global PREDICATE_COUNTS
        PREDICATE_COUNTS = {is_number: 0, is_symbolic: 0}


    reset_predicate_counts()

    generic_procedure = generic_procedure_constructor(cache_wrap_dispatch_store(TrieDispatchStore))

    plus = generic_procedure("plus", 2, None)
    define_generic_procedure_handler(plus, all_args(2, is_number), lambda a, b: a + b)
    define_generic_procedure_handler(plus, any_arg(2, is_symbolic, is_number), lambda a, b: f"({a} + {b})")

    minus = generic_procedure("minus", 2, None)
    define_generic_procedure_handler(minus, all_args(2, is_number), lambda a, b: a - b)
    define_generic_procedure_handler(minus, any_arg(2, is_symbolic, is_number), lambda a, b: f"({a} - {b})")

    print(plus(1, 2))
    print(plus(1, "b"))
    print(plus("a", 2))
    print(plus("a", "b"))

    reset_predicate_counts()


    def fib(n):
        if n <= 2:
            return n
        else:
            return plus(fib(minus(n, 1)), fib(minus(n, 2)))


    print(fib(20))
    print(PREDICATE_COUNTS)
