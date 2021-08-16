from chapter05.common.environment import THE_EMPTY_ENVIRONMENT, extend_environment
from chapter05.common.pairs import cadr, car, cdr, cons, is_null
from chapter05.common.primitive_types import symbol


def make_initial_environment():
    return extend_environment(
        map(car, INITIAL_ENV_BINDINGS.items()),
        map(cadr, INITIAL_ENV_BINDINGS.items()),
        THE_EMPTY_ENVIRONMENT,
    )


def error(msg, *args):
    raise RuntimeError(msg, *args)


def global_hash_set(key, value):
    global GLOBAL_HASH_TABLE
    GLOBAL_HASH_TABLE[key] = value


def global_hash_get(key):
    return GLOBAL_HASH_TABLE.get(key, None)


GLOBAL_HASH_TABLE = {}
INITIAL_ENV_BINDINGS = {
    symbol("foo"): lambda: 42,
    symbol("null?"): is_null,
    symbol("cons"): cons,
    symbol("car"): car,
    symbol("cdr"): cdr,
    symbol("+"): lambda a, b: a + b,
    symbol("-"): lambda a, b: a - b,
    symbol("*"): lambda a, b: a * b,
    symbol("/"): lambda a, b: a / b,
    symbol("="): lambda a, b: a == b,
    symbol("<"): lambda a, b: a < b,
    symbol(">"): lambda a, b: a > b,
    symbol("<="): lambda a, b: a <= b,
    symbol(">="): lambda a, b: a >= b,
    symbol("#t"): True,
    symbol("#f"): False,
    symbol("eq?"): lambda a, b: a is b,
    symbol("error"): error,
    symbol("global-hash-set!"): global_hash_set,
    symbol("global-hash-get"): global_hash_get,
    symbol("display"): lambda *args: print(*args),
}
