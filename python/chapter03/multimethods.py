
class SimpleDispatchStore:
    def __init__(self):
        self.rules = {}

    def get_handler(self, args, kwargs):
        for signature, handler in self.rules.items():
            if signature.matches(args, kwargs):
                return handler
        return None

    def add_handler(self, applicability, handler):
        for signature in applicability:
            self.rules[signature] = handler


class MultiMethod:
    def __init__(self, name, default_handler=None, dispatch_store=SimpleDispatchStore):
        self.name = name
        self.default_handler = default_handler or make_inapplicable(name)
        self.dispatch_store = dispatch_store()

    def add_handler(self, applicability, handler):
        self.dispatch_store.add_handler(applicability, handler)

    def __call__(self, *args, **kwargs):
        handler = (self.dispatch_store.get_handler(args, kwargs)
                   or self.default_handler)
        return handler(*args, **kwargs)


def make_inapplicable(name):
    def handler(*args, **kwargs):
        raise TypeError(f"{name} not applicable to {args} {kwargs}")
    return handler


class Signature:
    def __init__(self, types, kwtypes):
        self.types = types
        self.kwtypes = kwtypes

    def matches(self, args, kwargs):
        if len(args) < len(self.types):
            return False

        if len(args) + len(kwargs) != len(self.types) + len(self.kwtypes):
            return False

        for a, t in zip(args, self.types):
            if not isinstance(a, t):
                return False

        for a, t in zip(args[len(self.types):], self.kwtypes.values()):
            if not isinstance(a, t):
                return False

        for name, a in kwargs.items():
            t = self.kwtypes.get(name)
            if not (t and isinstance(a, t)):
                return False

        return True


def match_args(*types, **kwtypes):
    assert all(isinstance(t, type) for t in types)
    assert all(isinstance(t, type) for t in kwtypes.values())
    return [Signature(types, kwtypes)]


if __name__ == '__main__':
    plus = MultiMethod("plus")

    plus.add_handler(match_args(a=int, b=int), lambda a, b: a + b)

    assert plus(1, 2) == 3

    # todo: pass keyword types as keyword arguments to handler, even if the concrete arguments were passed by value?
    plus.add_handler(match_args(a=int, c=int), lambda a, b: a + b)
    assert plus(1, 2) != 3  # should call the lambda with a=1 and c=2, which is supposed to fail
