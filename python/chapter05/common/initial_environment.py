from chapter05.common.display import display
from chapter05.common.environment import THE_EMPTY_ENVIRONMENT, extend_environment
from chapter05.common.pairs import cadr, car, cdr, cons, is_null, is_pair, length
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
    symbol("pair?"): is_pair,
    symbol("cons"): cons,
    symbol("car"): car,
    symbol("cdr"): cdr,
    symbol("length"): length,
    symbol("+"): lambda *args: sum(args),
    symbol("-"): lambda a, b: a - b,
    symbol("*"): lambda a, b: a * b,
    symbol("/"): lambda a, b: a / b,
    symbol("%"): lambda a, b: a % b,
    symbol("="): lambda a, b: a == b,
    symbol("<"): lambda a, b: a < b,
    symbol(">"): lambda a, b: a > b,
    symbol("<="): lambda a, b: a <= b,
    symbol(">="): lambda a, b: a >= b,
    symbol("#t"): True,
    symbol("#f"): False,
    symbol("eq?"): lambda a, b: a is b,
    symbol("not"): lambda b: not b,
    symbol("error"): error,
    symbol("global-hash-set!"): global_hash_set,
    symbol("global-hash-get"): global_hash_get,
    symbol("display"): lambda *args: display(*args),
    symbol("newline"): lambda: print(),
    symbol("list"): lambda *args: args,
    symbol("the-unspecified-value"): type("_", (), {}),
}
