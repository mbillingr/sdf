"""
Representation of pairs that transparently supports tuples and lists.
For example, the tuple (1, 2, 3) and the list [1, 2, 3] are
equivalent to the nested pairs (1 . (2 . (3 . ()))).
"""

from dataclasses import dataclass


def cons(car, cdr):
    return Pair(car, cdr)


def length(obj):
    if isinstance(obj, Pair):
        return 1 + length(obj.cdr)
    if isinstance(obj, tuple):
        return len(obj)
    if isinstance(obj, list):
        return len(obj)
    return 1 + length(cdr(obj))


def memq(obj, seq):
    while True:
        if is_null(seq):
            return False
        if car(seq) is obj:
            return seq
        seq = cdr(seq)


@dataclass(frozen=True)
class Pair:
    car: object
    cdr: object


def is_pair(obj):
    return isinstance(obj, Pair) or isinstance(obj, tuple) and len(obj) >= 1


def is_null(obj):
    return obj == ()


def car(obj):
    if isinstance(obj, Pair):
        return obj.car
    return obj[0]


def cdr(obj):
    if isinstance(obj, Pair):
        return obj.cdr
    return obj[1:]


def cadr(obj):
    return car(cdr(obj))


def cddr(obj):
    return cdr(cdr(obj))


def caadr(obj):
    return car(car(cdr(obj)))


def caddr(obj):
    return car(cdr(cdr(obj)))


def cdadr(obj):
    return cdr(car(cdr(obj)))


def cdddr(obj):
    return cdr(cdr(cdr(obj)))


def cadddr(obj):
    return car(cdr(cdr(cdr(obj))))


def is_tagged_list(exp, tag):
    return is_pair(exp) and car(exp) == tag
