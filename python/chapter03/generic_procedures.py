import itertools


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
    return metadata.getter(args) or metadata.default_getter


def define_generic_procedure_handler(generic_procedure, applicability, handler):
    generic_procedure_metadata(generic_procedure).dispatch_store("add_handler")(applicability, handler)


# todo: this could be a class
def make_simple_dispatch_store():
    rules = {}
    default_handler = None

    def get_handler(args):
        for predicates, handler in rules.items():
            if predicates_match(predicates, args):
                return handler
        return None

    def add_handler(applicability, handler):
        for predicates in applicability:
            rules[predicates] = handler

    def get_default_handler():
        return default_handler

    def set_default_handler(handler):
        nonlocal default_handler
        default_handler = handler

    def dispatcher(message):
        if message == "get_handler":
            return get_handler
        elif message == "add_handler":
            return add_handler
        elif message == "get_default_handler":
            return get_default_handler
        elif message == "set_default_handler":
            return set_default_handler
        elif message == "get_rules":
            return lambda: rules
        else:
            raise NameError(f"Unknown message: {message}")

    return dispatcher


class Metadata:
    def __init__(self, name, arity, dispatch_store, default_handler):
        dispatch_store("set_default_handler")(default_handler)
        self.name = name
        self.arity = arity
        self.dispatch_store = dispatch_store
        self.getter = dispatch_store("get_handler")
        self.default_getter = dispatch_store("get_default_handler")()


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


def is_number(obj):
    return isinstance(obj, int) or isinstance(obj, float)


def is_symbolic(obj):
    return isinstance(obj, str)


simple_generic_procedure = generic_procedure_constructor(make_simple_dispatch_store)

plus = simple_generic_procedure("plus", 2, None)

define_generic_procedure_handler(plus, all_args(2, is_number), lambda a, b: a + b)
define_generic_procedure_handler(plus, any_arg(2, is_symbolic, is_number), lambda a, b: f"({a} + {b})")

print(plus(1, 2))
print(plus(1, "b"))
print(plus("a", 2))
print(plus("a", "b"))
