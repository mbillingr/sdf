import inspect


def compose(f, g):
    check_arity(f, 1)
    n = get_arity(g)

    def the_composition(*args):
        check_args(args, n)
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
    check_arity(h, 2)
    n = get_arity(f)
    check_arity(g, n)

    def the_combination(*args):
        check_args(args, n)
        return h(f(*args), g(*args))

    return restrict_arity(the_combination, n)


def spread_combine(h, f, g):
    n = get_arity(f)
    m = get_arity(g)
    t = n + m

    def the_combination(*args):
        check_args(args, t)
        return h(f(*args[:n]), g(*args[n:]))

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
