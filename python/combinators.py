import inspect


def compose(f, g):
    get_arity(f).check(1)
    n = get_arity(g)

    def the_composition(*args):
        n.check(len(args))
        tmp = g(*args)
        return f(tmp)

    return restrict_arity(the_composition, n)


def iterate(n):
    def the_iterator(f):
        if n == 0:
            return identity
        return compose(f, iterate(n - 1)(f))

    return the_iterator


def identity(arg):
    return arg


def parallel_combine(h, f, g):
    get_arity(h).check(2)
    n = get_arity(f)
    m = get_arity(g)
    assert n == m

    def the_combination(*args):
        n.check(len(args))
        return h(f(*args), g(*args))

    return restrict_arity(the_combination, n)


def spread_combine(h, f, g):
    n = get_arity(f)
    m = get_arity(g)
    assert n.is_fixed() or m.is_fixed()

    t = n + m
    if n.is_fixed():
        def the_combination(*args):
            t.check(len(args))
            i = n.min
            return h(f(*args[:i]), g(*args[i:]))
    else:
        def the_combination(*args):
            t.check(len(args))
            i = len(args) - m.min
            return h(f(*args[:i]), g(*args[i:]))

    return restrict_arity(the_combination, t)


def restrict_arity(proc, arity):
    if isinstance(arity, int):
        arity = Arity(arity)
    ARITY_TABLE[proc] = arity
    return proc


def get_arity(proc):
    try:
        return ARITY_TABLE[proc]
    except KeyError:
        pass
    sig = inspect.signature(proc)
    n_required = 0
    n_optional = 0
    vararg = False
    for param in sig.parameters.values():
        if (param.kind == inspect.Parameter.POSITIONAL_ONLY
                or param.kind == inspect.Parameter.POSITIONAL_OR_KEYWORD):
            if param.default == inspect.Parameter.empty:
                n_required += 1
            else:
                n_optional += 1
        elif param.kind == inspect.Parameter.VAR_POSITIONAL:
            vararg = True
        elif (param.kind == inspect.Parameter.KEYWORD_ONLY
              and param.default == inspect.Parameter.empty):
            raise TypeError("Arity understands no keyword arguments")

    if vararg:
        return Arity(n_required, False)
    else:
        return Arity(n_required, n_required + n_optional)


def check_arity(func, expected_arity):
    if get_arity(func) != expected_arity:
        raise TypeError(f"Expected arity {expected_arity},"
                        f" got {get_arity(func)}")


def check_args(args, n_expected):
    if len(args) != n_expected:
        raise TypeError(f"Expected {n_expected} positional "
                        f"arguments but got {len(args)}.")


class Arity:
    def __init__(self, min, max=None):
        self.min = min
        if max is None:
            self.max = min
        else:
            self.max = max

    def is_fixed(self):
        return self.min == self.max

    def is_compatible(self, n_args):
        return n_args >= self.min and (self.max is False or n_args <= self.max)

    def check(self, n_args):
        if not self.is_compatible(n_args):
            raise TypeError(f"Expected {self} arguments,"
                            f" got {n_args}")

    def __add__(self, other):
        min = self.min + other.min
        if self.max and other.max:
            max = self.max + other.max
        else:
            max = False
        return Arity(min, max)

    def __eq__(self, other):
        return self.min == other.min and self.max == other.max

    def __repr__(self):
        if self.is_fixed():
            return str(self.min)
        elif not self.max:
            return f'>={self.min}'
        else:
            return f'{self.min}..{self.max}'


ARITY_TABLE = {}
