def is_number(obj):
    return isinstance(obj, int) or isinstance(obj, float)


def is_boolean(obj):
    return isinstance(obj, bool)


def is_string(obj):
    return isinstance(obj, str)


def is_symbol(obj):
    return isinstance(obj, Symbol)


def boolean(obj):
    return obj is not False and obj is not None


class Symbol:
    def __init__(self, name):
        self.name = name

    def __eq__(self, other):
        return self is other or isinstance(other, Symbol) and self.name == other.name

    def __repr__(self):
        return self.name

    def __hash__(self):
        return hash((Symbol, self.name))


def symbol(name):
    global INTERNED_SYMBOLS
    try:
        return INTERNED_SYMBOLS[name]
    except KeyError:
        s = Symbol(name)
        INTERNED_SYMBOLS[name] = s
    return s


INTERNED_SYMBOLS = {}
