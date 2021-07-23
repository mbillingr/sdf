from chapter03.generic_procedures import (
    any_arg,
    define_generic_procedure_handler,
    is_any,
    simple_generic_procedure,
)

from generic_interpreter import (
    INITIAL_ENV_BINDINGS,
    cons,
    eval_operands,
    g,
    init,
    is_environment,
    is_operands,
    is_pair,
    is_symbol,
    is_variable,
    lookup_variable_value,
    match_args,
    symbol,
)


def evaluate_symbol(variable, environment):
    value = lookup_variable_value(variable, environment)
    return variable if value is None else value


g.define_eval_handler(match_args(is_variable, is_environment), evaluate_symbol)

add = simple_generic_procedure("+", 2, lambda a, b: a + b)
sub = simple_generic_procedure("-", 2, lambda a, b: a - b)
mul = simple_generic_procedure("*", 2, lambda a, b: a * b)
div = simple_generic_procedure("/", 2, lambda a, b: a / b)

INITIAL_ENV_BINDINGS[symbol("+")] = add
INITIAL_ENV_BINDINGS[symbol("-")] = sub
INITIAL_ENV_BINDINGS[symbol("*")] = mul
INITIAL_ENV_BINDINGS[symbol("/")] = div


def is_symbolic(obj):
    return is_symbol(obj) or is_pair(obj)


define_generic_procedure_handler(
    add, any_arg(2, is_symbolic, is_any), lambda a, b: (symbol("+"), a, b)
)
define_generic_procedure_handler(
    sub, any_arg(2, is_symbolic, is_any), lambda a, b: (symbol("-"), a, b)
)
define_generic_procedure_handler(
    mul, any_arg(2, is_symbolic, is_any), lambda a, b: (symbol("*"), a, b)
)
define_generic_procedure_handler(
    div, any_arg(2, is_symbolic, is_any), lambda a, b: (symbol("/"), a, b)
)

g.define_apply_handler(
    match_args(is_symbol, is_operands, is_environment),
    lambda proc, operands, calling_environment: cons(
        proc, eval_operands(operands, calling_environment)
    ),
)

if __name__ == "__main__":
    init()
