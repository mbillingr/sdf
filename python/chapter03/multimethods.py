from chapter03.trie import Trie


class SimpleDispatchStore:
    def __init__(self):
        self.rules = {}

    def get_handler(self, *args):
        for signature, handler in self.rules.items():
            if self.match_signature(signature, args):
                return handler
        return None

    def add_handler(self, applicability, handler):
        for signature in applicability:
            self.rules[signature] = handler

    @staticmethod
    def match_signature(types, args):
        if len(args) != len(types):
            return False
        return all(isinstance(a, t) for a, t in zip(args, types))


class TrieDispatchStore:
    def __init__(self):
        self.trie = TypeTrie()

    def get_handler(self, *args):
        return self.trie.get_a_value(args)

    def add_handler(self, applicability, handler):
        for signature in applicability:
            self.trie.set_path_value(signature, handler)


class MultiMethod:
    def __init__(self, name, default_handler=None, dispatch_store=TrieDispatchStore):
        self.name = name
        self.default_handler = default_handler or make_inapplicable(name)
        self.dispatch_store = dispatch_store()

    def add_handler(self, applicability, handler):
        self.dispatch_store.add_handler(applicability, handler)

    def __call__(self, *args):
        handler = (self.dispatch_store.get_handler(*args)
                   or self.default_handler)
        return handler(*args)


def make_inapplicable(name):
    def handler(*args):
        raise TypeError(f"{name} not applicable to {args}")

    return handler


class TypeTrie(Trie):
    def __init__(self):
        super().__init__()

    def add_edge(self, ty):
        assert isinstance(ty, type)
        return super().add_edge(TypePredicate(ty))


class TypePredicate:
    def __init__(self, ty):
        assert isinstance(ty, type)
        self.type = ty

    def __call__(self, arg):
        return isinstance(arg, self.type)

    def __eq__(self, other):
        if isinstance(other, TypePredicate):
            return self.type == other.type
        else:
            return self.type == other

    def __hash__(self):
        return hash(self.type)

    def __repr__(self):
        return f"TypePredicate({self.type})"


def match_args(*types):
    assert all(isinstance(t, type) for t in types)
    return [types]


if __name__ == '__main__':
    plus = MultiMethod("plus")
    minus = MultiMethod("minus")

    plus.add_handler(match_args(int, int), lambda a, b: a + b)

    assert plus(1, 2) == 3

    print("OK")
