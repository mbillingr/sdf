from chapter03.generic_procedures import (
    define_generic_procedure_handler,
    match_args,
    simple_generic_procedure,
)

from chapter05.common.derived_syntax import (
    cond_to_if,
    is_cond,
    is_let,
    let_to_combination,
)
from chapter05.common.display import display
from chapter05.common.environment import (
    define_variable,
    extend_environment,
    is_environment,
    lookup_variable_value,
    set_variable_value,
)
from chapter05.common.initial_environment import make_initial_environment
from chapter05.common.lazy import (
    advanced_value,
    is_advanced_memo,
    is_postponed,
    is_postponed_memo,
    postpone,
    postpone_memo,
)
from chapter05.common.pairs import car, cdr, cons, is_null, is_pair, length
from chapter05.common.parser import read
from chapter05.common.primitive_types import (
    Symbol,
    boolean,
    is_boolean,
    is_number,
    is_string,
    is_symbol,
)
from chapter05.common.procedures import (
    apply_primitive_procedure,
    is_compound_procedure,
    is_operands,
    is_strict_primitive_procedure,
    make_compound_procedure,
    procedure_body,
    procedure_environment,
    procedure_parameter_name,
    procedure_parameters,
)
from chapter05.common.syntax import (
    assignment_value,
    assignment_variable,
    begin_actions,
    definition_value,
    definition_variable,
    if_alternative,
    if_consequent,
    if_predicate,
    is_application,
    is_assignment,
    is_begin,
    is_definition,
    is_if,
    is_lambda,
    is_lazy,
    is_lazy_memo,
    is_quoted,
    is_variable,
    lambda_body,
    lambda_parameters,
    operands,
    operator,
    text_of_quotation,
)


def default_eval(expression, environment):
    if is_application(expression):
        return g.apply(
            g.advance(g.eval(operator(expression), environment)),
            operands(expression),
            environment,
        )
    else:
        raise TypeError(f"Unknown expression type {expression}")


def default_apply(procedure, _operands, _calling_environment):
    raise TypeError(f"Unknown procedure type {procedure}")


class g:
    eval = simple_generic_procedure("g:eval", 2, default_eval)
    advance = simple_generic_procedure("g:advance", 1, lambda x: x)
    apply = simple_generic_procedure("g:apply", 3, default_apply)

    handle_operand = simple_generic_procedure(
        "g:handle-operand",
        3,
        lambda parameter, operand, environment: g.advance(g.eval(operand, environment)),
    )

    @staticmethod
    def define_eval_handler(applicability, handler):
        define_generic_procedure_handler(g.eval, applicability, handler)

    @staticmethod
    def define_advance_handler(applicability, handler):
        define_generic_procedure_handler(g.advance, applicability, handler)

    @staticmethod
    def define_apply_handler(applicability, handler):
        define_generic_procedure_handler(g.apply, applicability, handler)


g.define_eval_handler(match_args(is_number, is_environment), lambda expr, env: expr)
g.define_eval_handler(match_args(is_boolean, is_environment), lambda expr, env: expr)
g.define_eval_handler(match_args(is_string, is_environment), lambda expr, env: expr)

g.define_eval_handler(
    match_args(is_quoted, is_environment),
    lambda expr, env: text_of_quotation(expr),
)

g.define_eval_handler(match_args(is_variable, is_environment), lookup_variable_value)

g.define_eval_handler(
    match_args(is_if, is_environment),
    lambda expression, environment: (
        g.eval(if_consequent(expression), environment)
        if boolean(g.advance(g.eval(if_predicate(expression), environment)))
        else g.eval(if_alternative(expression), environment)
    ),
)


def evaluate_sequence(actions, environment):
    if is_null(actions):
        raise SyntaxError("Empty sequence")
    if is_null(cdr(actions)):
        return g.eval(car(actions), environment)
    g.eval(car(actions), environment)
    return evaluate_sequence(cdr(actions), environment)


g.define_eval_handler(
    match_args(is_begin, is_environment),
    lambda expression, environment: (
        evaluate_sequence(begin_actions(expression), environment)
    ),
)

g.define_eval_handler(
    match_args(is_lambda, is_environment),
    lambda expression, environment: (
        make_compound_procedure(
            lambda_parameters(expression), lambda_body(expression), environment
        )
    ),
)

g.define_eval_handler(
    match_args(is_cond, is_environment),
    lambda expression, environment: (g.eval(cond_to_if(expression), environment)),
)

g.define_eval_handler(
    match_args(is_let, is_environment),
    lambda expression, environment: (
        g.eval(let_to_combination(expression), environment)
    ),
)

g.define_eval_handler(
    match_args(is_assignment, is_environment),
    lambda expression, environment: (
        set_variable_value(
            assignment_variable(expression),
            g.eval(assignment_value(expression), environment),
            environment,
        )
    ),
)

g.define_eval_handler(
    match_args(is_definition, is_environment),
    lambda expression, environment: (
        define_variable(
            definition_variable(expression),
            g.eval(definition_value(expression), environment),
            environment,
        )
    ),
)

# apply handlers


def eval_operands(operands, calling_environment):
    return tuple(
        g.advance(g.eval(operand, calling_environment)) for operand in operands
    )


g.define_apply_handler(
    match_args(is_strict_primitive_procedure, is_operands, is_environment),
    lambda procedure, operands, calling_environment: apply_primitive_procedure(
        procedure, eval_operands(operands, calling_environment)
    ),
)


def is_strict_compound_procedure(obj):
    return is_compound_procedure(obj) and all(map(is_symbol, procedure_parameters(obj)))


def apply_strict_compound_procedure(procedure, operands, calling_environment):
    if length(procedure_parameters(procedure)) != length(operands):
        raise TypeError("Wrong number of arguments supplied")
    return g.eval(
        procedure_body(procedure),
        extend_environment(
            procedure_parameters(procedure),
            eval_operands(operands, calling_environment),
            procedure_environment(procedure),
        ),
    )


g.define_apply_handler(
    match_args(is_strict_compound_procedure, is_operands, is_environment),
    apply_strict_compound_procedure,
)


def is_general_compound_procedure(obj):
    return is_compound_procedure(obj) and any(map(is_pair, procedure_parameters(obj)))


def apply_general_compound_procedure(procedure, operands, calling_environment):
    if length(procedure_parameters(procedure)) != length(operands):
        raise TypeError("Wrong number of arguments supplied")
    params = procedure_parameters(procedure)
    body = procedure_body(procedure)
    names = [procedure_parameter_name(p) for p in params]
    arguments = [
        g.handle_operand(param, operand, calling_environment)
        for param, operand in zip(params, operands)
    ]
    return g.eval(
        body, extend_environment(names, arguments, procedure_environment(procedure))
    )


g.define_apply_handler(
    match_args(is_general_compound_procedure, is_operands, is_environment),
    apply_general_compound_procedure,
)


def is_operand(_exp):
    return True


define_generic_procedure_handler(
    g.handle_operand,
    match_args(is_lazy, is_operand, is_environment),
    lambda parameter, operand, environment: postpone(operand, environment),
)

define_generic_procedure_handler(
    g.handle_operand,
    match_args(is_lazy_memo, is_operand, is_environment),
    lambda parameter, operand, environment: postpone_memo(operand, environment),
)

g.define_advance_handler(
    match_args(is_postponed),
    lambda object: g.advance(g.eval(object.expression, object.environment)),
)


def handle_postponed_memo(object):
    value = g.advance(g.eval(object.expression, object.environment))
    object.set_value(value)
    return value


g.define_advance_handler(
    match_args(is_postponed_memo),
    handle_postponed_memo,
)

g.define_advance_handler(match_args(is_advanced_memo), advanced_value)

##


##


def init():
    initialize_repl()
    print(THE_GLOBAL_ENVIRONMENT)
    repl()


def repl():
    check_repl_initialized()
    while True:
        print("> ", end="")
        display(eval_str(None))
        print()


def eval_str(s):
    ans = "no result"
    for input in read(s):
        ans = g.advance(g.eval(input, THE_GLOBAL_ENVIRONMENT))
    return ans


THE_GLOBAL_ENVIRONMENT = "not initialized"


def initialize_repl():
    global THE_GLOBAL_ENVIRONMENT
    THE_GLOBAL_ENVIRONMENT = make_initial_environment()


def check_repl_initialized():
    if THE_GLOBAL_ENVIRONMENT == "not initialized":
        raise RuntimeError("Interpreter not initialized. Run init() first.")


if __name__ == "__main__":
    a = Symbol("a")
    b = Symbol("b")
    c = Symbol("c")

    print((a, b, c))

    print(car((1, 2, 3)), cdr((1, 2, 3)))
    x = cons(1, (2, 3, 4))
    print(x)
    print(car(x), cdr(x), cdr(cdr(x)))

    quote = Symbol("quote")
    x = Symbol("x")
    begin = Symbol("begin")

    # print(g.eval(((Symbol('lambda'), (x,), x), 42), ()))
    print(g.eval((begin, 1, 2, 3), ()))

    init()
