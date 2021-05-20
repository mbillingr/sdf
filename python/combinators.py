import inspect

from values import apply, Values


def compose(*funcs):
    funcs = list(funcs)
    try:
        f = funcs.pop()
    except IndexError:
        f = Values

    for func in funcs[::-1]:
        f = compose2(func, f)
    return f


def compose2(f, g):
    n = get_arity(g)

    def the_composition(*args, **kwargs):
        n.check(len(args))
        return apply(f, g(*args, **kwargs))

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
    return compose(h, parallel_apply(f, g))


def parallel_apply(f, g):
    n = get_arity(f)
    m = get_arity(g)
    assert n == m

    def the_combination(*args, **kwargs):
        n.check(len(args))
        fv = Values(f(*args, **kwargs))
        gv = Values(g(*args, **kwargs))
        return fv.append(gv)

    return restrict_arity(the_combination, n)


def spread_combine(h, f, g):
    return compose(h, spread_apply(f, g))


def spread_apply(f, g):
    n = get_arity(f)
    m = get_arity(g)
    assert n.fixed() or m.fixed()

    t = n + m
    if n.fixed():
        def the_combination(*args, **kwargs):
            t.check(len(args))
            i = n.min
            fv = Values(f(*args[:i], **kwargs))
            gv = Values(g(*args[i:], **kwargs))
            return fv.append(gv)
    else:
        def the_combination(*args, **kwargs):
            t.check(len(args))
            i = len(args) - m.min
            fv = Values(f(*args[:i], **kwargs))
            gv = Values(g(*args[i:], **kwargs))
            return fv.append(gv)

    return restrict_arity(the_combination, t)


def discard_arguments(*indices):
    def wrapper(f):
        for i in indices:
            if isinstance(i, int):
                f = discard_positional_argument(i)(f)
            elif isinstance(i, str):
                f = discard_keyword_argument(i)(f)
            else:
                raise TypeError("discarded argument must be int or str")
        return f

    return wrapper


def discard_positional_argument(i):
    def wrapper(f):
        m = get_arity(f) + 1

        def the_combination(*args, **kwargs):
            m.check(len(args))
            return Values(*(args[:i] + args[i + 1:]), **kwargs)

        return compose(f, the_combination)

    return wrapper


def discard_keyword_argument(name):
    def wrapper(f):
        def the_combination(*args, **kwargs):
            del kwargs[name]
            return f(*args, **kwargs)

        return the_combination

    return wrapper


def curry_arguments(*indices):
    def curry_wrapper(*args):
        args = list(args)

        def currier(f):
            get_arity(f).check(len(args) + len(indices))

            def inner_wrapper(*values):
                for i, x in zip(indices, values):
                    args.insert(i, x)
                return f(*args)

            return inner_wrapper

        return currier

    return curry_wrapper


def permute_arguments(*permspec):
    permute = make_permutation(permspec)

    def permutation_wrapper(f):
        the_combination = compose(
            f, lambda *args, **kwargs: Values(*permute(args)))

        n = get_arity(f)
        assert n.fixed() == len(permspec)
        return restrict_arity(the_combination, n)

    return permutation_wrapper


def make_permutation(permspec):
    def the_permuter(lst):
        return [lst[i] for i in permspec]

    return the_permuter


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

    def fixed(self):
        return self.min == self.max and self.min

    def is_compatible(self, n_args):
        return n_args >= self.min and (self.max is False or n_args <= self.max)

    def check(self, n_args):
        if not self.is_compatible(n_args):
            raise TypeError(f"Expected {self} arguments,"
                            f" got {n_args}")

    def __add__(self, other):
        if isinstance(other, int):
            return self + Arity(other, other)
        min = self.min + other.min
        if self.max and other.max:
            max = self.max + other.max
        else:
            max = False
        return Arity(min, max)

    def __eq__(self, other):
        return self.min == other.min and self.max == other.max

    def __repr__(self):
        if self.fixed():
            return str(self.min)
        elif not self.max:
            return f'>={self.min}'
        else:
            return f'{self.min}..{self.max}'


ARITY_TABLE = {}
