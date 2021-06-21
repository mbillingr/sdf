from chapter03.trie import Trie


def most_specific_generic_procedure(name, default_handler=None):
    return MultiMethod(name, default_handler, MostSpecificDispatchStore)


class DispatchStore:
    def get_handler(self, *args):
        abstract_method()

    def add_handler(self, applicability, handler):
        abstract_method()

    def set_default_handler(self, handler):
        abstract_method()

    def get_default_handler(self):
        abstract_method()


def abstract_method():
    raise NotImplementedError("Subclass Responsibility")


class DefaultDispatchStore(DispatchStore):
    def __init__(self, default_handler=None):
        super().__init__()
        self.default_handler = default_handler

    def get_handler(self):
        return self.default_handler

    def set_default_handler(self, handler):
        self.default_handler = handler

    def get_default_handler(self):
        return self.default_handler


class SimpleDispatchStore(DefaultDispatchStore):
    def __init__(self):
        super().__init__()
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


class TrieDispatchStore(DefaultDispatchStore):
    def __init__(self):
        super().__init__()
        self.trie = TypeTrie()

    def get_handler(self, *args):
        return self.trie.get_a_value(args)

    def add_handler(self, applicability, handler):
        for signature in applicability:
            self.trie.set_path_value(signature, handler)


class RankingDispatchStore(SimpleDispatchStore):
    def __init__(self, order, select):
        super().__init__()
        self.order = order
        self.select = select

    def get_handler(self, *args):
        matches = []
        for signature, handler in self.rules.items():
            if self.match_signature(signature, args):
                matches.append((signature, handler))
        if not matches:
            return None
        matches = sorted(matches, key=lambda m: self.order(m[0]))
        return self.select(matches)[1]


class MostSpecificDispatchStore(RankingDispatchStore):
    def __init__(self):
        super().__init__(order=Order, select=self.select_first)

    @staticmethod
    def select_first(handlers):
        return handlers[0]


class ChainingDispatchStore(RankingDispatchStore):
    def __init__(self):
        super().__init__(order=Order, select=self.chain_handlers)

    def chain_handlers(self, handlers):
        handler_chain = self.get_default_handler()
        for h in handlers[::-1]:
            handler_chain = HandlerChain(h, handler_chain)
        return handler_chain


class HandlerChain:
    def __init__(self, handler, next_handler):
        self.handler = handler
        self.next_handler = next_handler

    def __call__(self, *args):
        return self.handler(self.next_handler, *args)


class Order:
    def __init__(self, signature):
        self.signature = signature

    def __lt__(self, other):
        for t1, t2 in zip(self.signature, other.signature):
            if t1 == t2:
                continue
            elif issubclass(t1, t2):
                return True
            elif issubclass(t2, t1):
                return False
            else:
                continue
        return False


class MultiMethod:
    def __init__(self, name, default_handler=None, dispatch_store: type[DispatchStore] = TrieDispatchStore):
        self.name = name
        self.dispatch_store = dispatch_store()
        self.dispatch_store.set_default_handler(default_handler or make_inapplicable(name))

    def add_handler(self, applicability, handler):
        self.dispatch_store.add_handler(applicability, handler)

    def __call__(self, *args):
        handler = self.dispatch_store.get_handler(*args)
        if handler is None:
            handler = self.dispatch_store.get_default_handler()
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
