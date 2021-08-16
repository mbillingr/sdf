from functools import reduce

from chapter02.combinators import compose
from chapter03.generic_procedures import (
    define_generic_procedure_handler,
    match_args,
    simple_generic_procedure,
)

import chapter05.common.symbols as S
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
    is_undoable_assignment,
    is_variable,
    lambda_body,
    lambda_parameters,
    operands,
    operator,
    text_of_quotation,
)

THE_GLOBAL_ENVIRONMENT = make_initial_environment()


def continue_with(procedure, *args):
    return procedure, args


def run_execution(proc, *args):
    try:
        while True:
            proc, args = proc(*args)
    except Return as ret:
        return ret.value


class Return(Exception):
    def __init__(self, value):
        self.value = value


def eval_str(s, env=None):
    if env is None:
        env = THE_GLOBAL_ENVIRONMENT
    ans = "no result"
    for input in read(s):
        ans = a.eval(input, env)
    return ans


def default_analyze(expression):
    if is_application(expression):
        return analyze_application(expression)
    else:
        raise SyntaxError(f"Unknown expression type: {expression}")


def default_apply(procedure, _operand_execs, _calling_environment, _succeed, fail):
    return continue_with(fail, "Unknown procedure type", procedure)


class a:
    @staticmethod
    def eval(expression, environment):
        exec = analyze(expression)

        def succeed(value, _fail):
            raise Return(value)

        def fail(*what):
            raise RuntimeError("Unhandled failure:", *what)

        return run_execution(exec, environment, succeed, fail)

    analyze = simple_generic_procedure("x:analyze", 1, default_analyze)
    apply = simple_generic_procedure("x:apply", 5, default_apply)
    handle_operand = simple_generic_procedure(
        "x:handle-operand",
        5,
        lambda parameter, operand_exec, environment, succeed, fail: continue_with(
            operand_exec, environment, succeed, fail
        ),
    )

    advance = simple_generic_procedure(
        "x:advance", 3, lambda x, succeed, fail: continue_with(succeed, x, fail)
    )


def analyze(expression):
    return a.analyze(expression)


def is_executor(x):
    return callable(x)


def is_executors(x):
    return True


def execute_strict(executor, env, succeed, fail):
    return continue_with(
        executor,
        env,
        lambda value, fail1: continue_with(a.advance, value, succeed, fail1),
        fail,
    )


def analyze_application(expression):
    operator_exec = analyze(operator(expression))
    operand_execs = tuple(map(analyze, operands(expression)))

    return lambda env, succeed, fail: execute_strict(
        operator_exec,
        env,
        lambda procedure, fail2: continue_with(
            a.apply, procedure, operand_execs, env, succeed, fail2
        ),
        fail,
    )


def analyze_self_evaluating(expression):
    return lambda env, succeed, fail: continue_with(succeed, expression, fail)


define_generic_procedure_handler(
    a.analyze, match_args(is_boolean), analyze_self_evaluating
)
define_generic_procedure_handler(
    a.analyze, match_args(is_number), analyze_self_evaluating
)
define_generic_procedure_handler(
    a.analyze, match_args(is_string), analyze_self_evaluating
)


def analyze_quoted(expression):
    qval = text_of_quotation(expression)
    return lambda env, succeed, fail: continue_with(succeed, qval, fail)


define_generic_procedure_handler(a.analyze, match_args(is_quoted), analyze_quoted)


def analyze_variable(expression):
    def execute_variable(env, succeed, fail):
        value = lookup_variable_value(expression, env)
        if value is None:
            return continue_with(fail, "unbound variable", expression)
        else:
            return continue_with(succeed, value, fail)

    return execute_variable


define_generic_procedure_handler(a.analyze, match_args(is_variable), analyze_variable)


def analyze_lambda(expression):
    variables = lambda_parameters(expression)
    body_exec = analyze(lambda_body(expression))
    return lambda env, succeed, fail: continue_with(
        succeed, make_compound_procedure(variables, body_exec, env), fail
    )


define_generic_procedure_handler(a.analyze, match_args(is_lambda), analyze_lambda)


def analyze_if(expression):
    predicate_exec = analyze(if_predicate(expression))
    consequent_exec = analyze(if_consequent(expression))
    alternative_exec = analyze(if_alternative(expression))

    return lambda env, succeed, fail: execute_strict(
        predicate_exec,
        env,
        lambda pred_value, pred_fail: continue_with(
            consequent_exec if pred_value else alternative_exec, env, succeed, pred_fail
        ),
        fail,
    )


define_generic_procedure_handler(a.analyze, match_args(is_if), analyze_if)


def analyze_begin(expression):
    def chain_execs(exec1, exec2):
        def sequence2(env, succeed, fail):
            return continue_with(
                exec1,
                env,
                lambda value, fail2: continue_with(exec2, env, succeed, fail2),
                fail,
            )

        return sequence2

    exps = begin_actions(expression)

    if is_null(exps):
        raise SyntaxError("Empty sequence")

    return reduce(chain_execs, map(analyze, exps))


define_generic_procedure_handler(a.analyze, match_args(is_begin), analyze_begin)


def analyze_assignment(expression):
    var = assignment_variable(expression)
    value_exec = analyze(assignment_value(expression))

    def execute_assignment(env, succeed, fail):
        def the_assignment(new_val, val_fail):
            set_variable_value(var, new_val, env)
            return continue_with(succeed, S.OK, val_fail)

        return continue_with(value_exec, env, the_assignment, fail)

    return execute_assignment


define_generic_procedure_handler(
    a.analyze, match_args(is_assignment), analyze_assignment
)


def analyze_undoable_assignment(expression):
    var = assignment_variable(expression)
    value_exec = analyze(assignment_value(expression))

    def execute_assignment(env, succeed, fail):
        def the_assignment(new_val, val_fail):
            old_val = lookup_variable_value(var, env)
            set_variable_value(var, new_val, env)

            def restore_old_value(*args):
                set_variable_value(var, old_val, env)
                return continue_with(val_fail, *args)

            return continue_with(succeed, S.OK, restore_old_value)

        return continue_with(value_exec, env, the_assignment, fail)

    return execute_assignment


define_generic_procedure_handler(
    a.analyze, match_args(is_undoable_assignment), analyze_undoable_assignment
)


def analyze_definition(expression):
    var = definition_variable(expression)
    value_exec = analyze(definition_value(expression))

    def execute_definition(env, succeed, fail):
        def the_definition(new_val, val_fail):
            define_variable(var, new_val, env)
            return continue_with(succeed, var, val_fail)

        return continue_with(value_exec, env, the_definition, fail)

    return execute_definition


define_generic_procedure_handler(
    a.analyze, match_args(is_definition), analyze_definition
)

# === derived syntax ===

define_generic_procedure_handler(
    a.analyze, match_args(is_cond), compose(analyze, cond_to_if)
)
define_generic_procedure_handler(
    a.analyze, match_args(is_let), compose(analyze, let_to_combination)
)

# === apply ===


def apply_strict_primitive_procedure(procedure, operand_execs, env, succeed, fail):
    execute_proc = lambda args, fail0: continue_with(
        succeed, apply_primitive_procedure(procedure, args), fail0
    )

    for operand_exec in operand_execs:
        execute_proc = lambda args, f, operand_exec=operand_exec, execute_proc=execute_proc: execute_strict(
            operand_exec,
            env,
            lambda arg, farg: continue_with(execute_proc, (arg,) + args, farg),
            f,
        )

    return continue_with(execute_proc, (), fail)


define_generic_procedure_handler(
    a.apply,
    match_args(
        is_strict_primitive_procedure, is_executors, is_environment, callable, callable
    ),
    apply_strict_primitive_procedure,
)


def apply_compound_procedure(
    procedure, operand_execs, calling_environment, succeed, fail
):
    if length(procedure_parameters(procedure)) != length(operand_execs):
        return continue_with(fail, "Wrong number of arguments supplied")
    params = procedure_parameters(procedure)
    body_exec = procedure_body(procedure)
    names = map(procedure_parameter_name, params)

    execute_proc = lambda arguments, fail0: continue_with(
        body_exec,
        extend_environment(names, arguments, procedure_environment(procedure)),
        succeed,
        fail0,
    )

    for param, operand_exec in zip(params, operand_execs):
        execute_proc = lambda args, f, operand_exec=operand_exec, execute_proc=execute_proc: continue_with(
            a.handle_operand,
            param,
            operand_exec,
            calling_environment,
            lambda arg, farg: continue_with(execute_proc, (arg,) + args, farg),
            f,
        )

    return continue_with(execute_proc, (), fail)


define_generic_procedure_handler(
    a.apply,
    match_args(is_compound_procedure, is_executors, is_environment),
    apply_compound_procedure,
)

define_generic_procedure_handler(
    a.handle_operand,
    match_args(is_lazy, is_executor, is_environment, callable, callable),
    lambda parameter, operand_exec, environment, succeed, fail: continue_with(
        succeed, postpone(operand_exec, environment), fail
    ),
)
define_generic_procedure_handler(
    a.handle_operand,
    match_args(is_lazy_memo, is_executor, is_environment, callable, callable),
    lambda parameter, operand_exec, environment, succeed, fail: continue_with(
        succeed, postpone_memo(operand_exec, environment), fail
    ),
)

define_generic_procedure_handler(
    a.advance,
    match_args(is_postponed),
    lambda object: a.advance(object.expression(object.environment)),
)
define_generic_procedure_handler(
    a.advance,
    match_args(is_postponed_memo),
    lambda object: object.set_value(a.advance(object.expression(object.environment))),
)
define_generic_procedure_handler(
    a.advance, match_args(is_advanced_memo), advanced_value
)

if __name__ == "__main__":
    while True:
        print("> ", end="")
        display(eval_str(None))
        print()