from chapter03.generic_procedures import (
    simple_generic_procedure,
    define_generic_procedure_handler,
    match_args,
)
from chapter05.common.pairs import car, is_null, cdr, is_pair

display = simple_generic_procedure("display", 1, lambda obj: print(obj, end=""))


def display_pair(p, parens=True):
    if parens:
        print("(", end="")
    display(car(p))
    if is_null(cdr(p)):
        # no more items
        pass
    elif is_pair(cdr(p)):
        print(" ", end="")
        display_pair(cdr(p), parens=False)
    else:
        print(" . ", end="")
        display(cdr(p))
    if parens:
        print(")", end="")


define_generic_procedure_handler(display, match_args(is_pair), display_pair)
