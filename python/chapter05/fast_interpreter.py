from functools import reduce
from weakref import WeakKeyDictionary


from chapter02.combinators import compose
from chapter03.generic_procedures import (
    define_generic_procedure_handler,
    match_args,
    simple_generic_procedure,
)
from chapter05.common.display import display
import chapter05.common.symbols as S
from chapter05.common.derived_syntax import (
    cond_to_if,
    is_cond,
    is_let,
    let_to_combination,
)
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
from chapter05.common.pairs import is_null, length
from chapter05.common.parser import read
from chapter05.common.primitive_types import is_boolean, is_number, is_string
from chapter05.common.procedures import (
    apply_primitive_procedure,
    is_compound_procedure,
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

THE_GLOBAL_ENVIRONMENT = make_initial_environment()


def eval_str(s, env=None):
    if env is None:
        env = THE_GLOBAL_ENVIRONMENT
    ans = "no result"
    for input in read(s):
        ans = x.advance(x.eval(input, env))
    return ans


def default_analyze(expression):
    if is_application(expression):
        return analyze_application(expression)
    else:
        raise SyntaxError(f"Unknown expression type: {expression}")


def default_apply(procedure, _operand_execs, _calling_environment):
    raise TypeError(f"Unknown procedure type {procedure}")


class x:
    @staticmethod
    def eval(expression, environment):
        exec = analyze(expression)
        return exec(environment)

    analyze = simple_generic_procedure("x:analyze", 1, default_analyze)
    apply = simple_generic_procedure("x:apply", 3, default_apply)
    handle_operand = simple_generic_procedure(
        "x:handle-operand",
        3,
        lambda parameter, operand_exec, environment: operand_exec(environment),
    )

    advance = simple_generic_procedure("x:advance", 1, lambda x: x)


def analyze(expression):
    return Executor(x.analyze(expression))


def is_executor(x):
    return isinstance(x, Executor)


def is_executors(x):
    return True


class Executor:
    def __init__(self, proc):
        self.proc = proc

    def __call__(self, environment):
        print("executing", self.proc.__name__, EXECUTOR_METADATA[self.proc])
        return self.proc(environment)


EXECUTOR_METADATA = WeakKeyDictionary()


def analyze_application(expression):
    operator_exec = analyze(operator(expression))
    operand_execs = tuple(map(analyze, operands(expression)))

    def execute_application(environment):
        return x.apply(
            x.advance(operator_exec(environment)), operand_execs, environment
        )

    EXECUTOR_METADATA[execute_application] = expression
    return execute_application


def analyze_self_evaluating(expression):
    def execute_self_evaluating(_environment):
        return expression

    EXECUTOR_METADATA[execute_self_evaluating] = expression
    return execute_self_evaluating


define_generic_procedure_handler(
    x.analyze, match_args(is_boolean), analyze_self_evaluating
)
define_generic_procedure_handler(
    x.analyze, match_args(is_number), analyze_self_evaluating
)
define_generic_procedure_handler(
    x.analyze, match_args(is_string), analyze_self_evaluating
)


def analyze_quoted(expression):
    qval = text_of_quotation(expression)

    def execute_quotation(_environment):
        return qval

    EXECUTOR_METADATA[execute_quotation] = expression
    return execute_quotation


define_generic_procedure_handler(x.analyze, match_args(is_quoted), analyze_quoted)


def analyze_variable(expression):
    def execute_variable(environment):
        return lookup_variable_value(expression, environment)

    EXECUTOR_METADATA[execute_variable] = expression
    return execute_variable


define_generic_procedure_handler(x.analyze, match_args(is_variable), analyze_variable)


def analyze_lambda(expression):
    variables = lambda_parameters(expression)
    body_exec = analyze(lambda_body(expression))

    def execute_lambda(environment):
        return make_compound_procedure(variables, body_exec, environment)

    EXECUTOR_METADATA[execute_lambda] = expression
    return execute_lambda


define_generic_procedure_handler(x.analyze, match_args(is_lambda), analyze_lambda)


def analyze_if(expression):
    predicate_exec = analyze(if_predicate(expression))
    consequent_exec = analyze(if_consequent(expression))
    alternative_exec = analyze(if_alternative(expression))

    def execute_if(environment):
        if x.advance(predicate_exec(environment)):
            return consequent_exec(environment)
        else:
            alternative_exec(environment)

    EXECUTOR_METADATA[execute_if] = expression
    return execute_if


define_generic_procedure_handler(x.analyze, match_args(is_if), analyze_if)


def analyze_begin(expression):
    def chain_execs(exec1, exec2):
        def sequence2(environment):
            exec1(environment)
            return exec2(environment)

        return sequence2

    exps = begin_actions(expression)

    if is_null(exps):
        raise SyntaxError("Empty sequence")

    execute_sequence = reduce(chain_execs, map(analyze, exps))
    EXECUTOR_METADATA[execute_sequence] = expression
    return execute_sequence


define_generic_procedure_handler(x.analyze, match_args(is_begin), analyze_begin)


def analyze_assignment(expression):
    var = assignment_variable(expression)
    value_exec = analyze(assignment_value(expression))

    def execute_assignment(environment):
        set_variable_value(var, value_exec(environment), environment)
        return S.OK

    EXECUTOR_METADATA[execute_assignment] = expression
    return execute_assignment


define_generic_procedure_handler(
    x.analyze, match_args(is_assignment), analyze_assignment
)


def analyze_definition(expression):
    var = definition_variable(expression)
    value_exec = analyze(definition_value(expression))

    def execute_definition(environment):
        define_variable(var, value_exec(environment), environment)
        return var

    EXECUTOR_METADATA[execute_definition] = expression
    return execute_definition


define_generic_procedure_handler(
    x.analyze, match_args(is_definition), analyze_definition
)

# === derived syntax ===

define_generic_procedure_handler(
    x.analyze, match_args(is_cond), compose(analyze, cond_to_if)
)
define_generic_procedure_handler(
    x.analyze, match_args(is_let), compose(analyze, let_to_combination)
)

# === apply ===

define_generic_procedure_handler(
    x.apply,
    match_args(is_strict_primitive_procedure, is_executors, is_environment),
    lambda procedure, operand_execs, environment: apply_primitive_procedure(
        procedure,
        map(lambda operand_exec: x.advance(operand_exec(environment)), operand_execs),
    ),
)


def apply_compound_procedure(procedure, operand_execs, calling_environment):
    if length(procedure_parameters(procedure)) != length(operand_execs):
        raise TypeError("Wrong number of arguments supplied")
    params = procedure_parameters(procedure)
    body_exec = procedure_body(procedure)
    names = map(procedure_parameter_name, params)
    arguments = map(
        lambda args: x.handle_operand(*args, calling_environment),
        zip(params, operand_execs),
    )
    return body_exec(
        extend_environment(names, arguments, procedure_environment(procedure))
    )


define_generic_procedure_handler(
    x.apply,
    match_args(is_compound_procedure, is_executors, is_environment),
    apply_compound_procedure,
)

define_generic_procedure_handler(
    x.handle_operand,
    match_args(is_lazy, is_executor, is_environment),
    lambda parameter, operand_exec, environment: postpone(operand_exec, environment),
)
define_generic_procedure_handler(
    x.handle_operand,
    match_args(is_lazy_memo, is_executor, is_environment),
    lambda parameter, operand_exec, environment: postpone_memo(
        operand_exec, environment
    ),
)

define_generic_procedure_handler(
    x.advance,
    match_args(is_postponed),
    lambda object: x.advance(object.expression(object.environment)),
)
define_generic_procedure_handler(
    x.advance,
    match_args(is_postponed_memo),
    lambda object: object.set_value(x.advance(object.expression(object.environment))),
)
define_generic_procedure_handler(
    x.advance, match_args(is_advanced_memo), advanced_value
)

if __name__ == "__main__":
    while True:
        print("> ", end="")
        display(eval_str(None))
        print()
