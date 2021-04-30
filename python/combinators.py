import inspect

from values import apply, Values


def compose(f, g):
    n = get_arity(g)

    def the_composition(*args, **kwargs):
        check_args(args, n)
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
    check_arity(g, n)

    def the_combination(*args, **kwargs):
        check_args(args, n)
        fv = Values(f(*args, **kwargs))
        gv = Values(g(*args, **kwargs))
        return fv.append(gv)

    return restrict_arity(the_combination, n)


def spread_combine(h, f, g):
    return compose(h, spread_apply(f, g))


def spread_apply(f, g):
    n = get_arity(f)
    m = get_arity(g)
    t = n + m

    def the_combination(*args, **kwargs):
        check_args(args, t)
        fv = Values(f(*args[:n], **kwargs))
        gv = Values(g(*args[n:], **kwargs))
        return fv.append(gv)

    return restrict_arity(the_combination, t)


def restrict_arity(proc, n_args):
    ARITY_TABLE[proc] = n_args
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

    if vararg or n_optional > 0:
        raise TypeError("Variable arity not understood yet")

    return n_required


def check_arity(func, expected_arity):
    if get_arity(func) != expected_arity:
        raise TypeError(f"Expected arity {expected_arity},"
                        f" got {get_arity(func)}")


def check_args(args, n_expected):
    if len(args) != n_expected:
        raise TypeError(f"Expected {n_expected} positional "
                        f"arguments but got {len(args)}.")


ARITY_TABLE = {}
