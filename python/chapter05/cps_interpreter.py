from functools import reduce
from time import time

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
from chapter05.common.pairs import car, cdr, cons, is_null, is_pair, length
from chapter05.common.parser import read
from chapter05.common.primitive_types import is_boolean, is_number, is_string, symbol
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
    amb_alternatives,
    assignment_value,
    assignment_variable,
    begin_actions,
    callcc_callee,
    definition_value,
    definition_variable,
    if_alternative,
    if_consequent,
    if_predicate,
    is_amb,
    is_application,
    is_assignment,
    is_begin,
    is_callcc,
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


def run_execution_light(proc, *args):
    while True:
        proc, args = proc(*args)


class Return(Exception):
    def __init__(self, value):
        self.value = value


def eval_str(s, env=None):
    if env is None:
        env = THE_GLOBAL_ENVIRONMENT
    inputs = tuple(read(s))
    expression = inputs[0] if len(inputs) == 1 else (S.BEGIN,) + inputs

    def fail_k(*args):
        raise Return("no value")

    def success_k(value, _fail):
        raise Return(value)

    return run_execution(a.eval, expression, env, success_k, fail_k)


def default_analyze(expression):
    if is_application(expression):
        return analyze_application(expression)
    else:
        raise SyntaxError(f"Unknown expression type: {expression}")


def default_apply(procedure, _operand_execs, _calling_environment, _succeed, fail):
    raise TypeError(f"Unknown procedure type {procedure}")


class a:
    @staticmethod
    def eval(expression, environment, succeed, fail):
        exec = analyze(expression)
        return continue_with(exec, environment, succeed, fail)

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
            raise NameError("unbound variable", expression)
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
            PROCEDURE_NAMES[new_val] = var
            return continue_with(succeed, var, val_fail)

        return continue_with(value_exec, env, the_definition, fail)

    return execute_definition


define_generic_procedure_handler(
    a.analyze, match_args(is_definition), analyze_definition
)


def analyze_amb(expression):
    alternative_execs = tuple(map(analyze, amb_alternatives(expression)))

    def execute_amb(env, succeed, fail):
        def loop(alts=alternative_execs):
            if is_pair(alts):
                return continue_with(
                    car(alts),
                    env,
                    succeed,
                    lambda *args: continue_with(loop, cdr(alts)),
                )
            else:
                return continue_with(fail)

        return continue_with(loop)

    return execute_amb


define_generic_procedure_handler(a.analyze, match_args(is_amb), analyze_amb)


def is_continuation(obj):
    return isinstance(obj, Continuation)


class Continuation:
    def __init__(self, succeed, fail):
        self.succeed = succeed
        self.fail = fail


def analyze_callcc(expression):
    callee_exec = analyze(callcc_callee(expression))

    def execute_callcc(env, succeed, fail):
        reified_continuation = Continuation(succeed, fail)

        return execute_strict(
            callee_exec,
            env,
            lambda callee, cfail: continue_with(
                a.apply,
                callee,
                [
                    lambda env, succeed, fail: continue_with(
                        succeed, reified_continuation, fail
                    )
                ],
                env,
                succeed,
                cfail,
            ),
            fail,
        )

    return execute_callcc


define_generic_procedure_handler(a.analyze, match_args(is_callcc), analyze_callcc)

# === derived syntax ===

define_generic_procedure_handler(
    a.analyze, match_args(is_cond), compose(analyze, cond_to_if)
)
define_generic_procedure_handler(
    a.analyze, match_args(is_let), compose(analyze, let_to_combination)
)

# === apply ===


def apply_continuation(continuation, operand_execs, env, succeed, fail):
    def execute_proc(args, fail0):
        if TRACE_APPLICATIONS:
            display(cons(symbol("<continuation>"), args))
            print()
        return continue_with(continuation.succeed, *args, continuation.fail)

    for operand_exec in operand_execs[::-1]:
        execute_proc = lambda args, f, operand_exec=operand_exec, execute_proc=execute_proc: execute_strict(
            operand_exec,
            env,
            lambda arg, farg: continue_with(execute_proc, args + (arg,), farg),
            f,
        )

    return continue_with(execute_proc, (), fail)


define_generic_procedure_handler(
    a.apply,
    match_args(is_continuation, is_executors, is_environment, callable, callable),
    apply_continuation,
)


def apply_strict_primitive_procedure(procedure, operand_execs, env, succeed, fail):
    def execute_proc(args, fail0):
        if TRACE_APPLICATIONS:
            display(cons(symbol("<primitive>"), args))
            print()
        return continue_with(succeed, apply_primitive_procedure(procedure, args), fail0)

    for operand_exec in operand_execs[::-1]:
        execute_proc = lambda args, f, operand_exec=operand_exec, execute_proc=execute_proc: execute_strict(
            operand_exec,
            env,
            lambda arg, farg: continue_with(execute_proc, args + (arg,), farg),
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
        raise TypeError("Wrong number of arguments supplied")
    params = procedure_parameters(procedure)
    body_exec = procedure_body(procedure)
    names = tuple(map(procedure_parameter_name, params))

    def execute_proc(arguments, fail0):
        body_env = extend_environment(
            names, arguments, procedure_environment(procedure)
        )
        if TRACE_APPLICATIONS:
            display(cons(procedure_name(procedure), arguments))
            display(body_env)
            print()
        return continue_with(
            body_exec,
            body_env,
            succeed,
            fail0,
        )

    for param, operand_exec in zip(params[::-1], operand_execs[::-1]):
        execute_proc = lambda args, f, operand_exec=operand_exec, execute_proc=execute_proc, param=param: continue_with(
            a.handle_operand,
            param,
            operand_exec,
            calling_environment,
            lambda arg, farg: continue_with(execute_proc, args + (arg,), farg),
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
    match_args(is_postponed, callable, callable),
    lambda object, succeed, fail: execute_strict(
        object.expression, object.environment, succeed, fail
    ),
)

define_generic_procedure_handler(
    a.advance,
    match_args(is_postponed_memo, callable, callable),
    lambda object, succeed, fail: execute_strict(
        object.expression,
        object.environment,
        lambda value, fail0: continue_with(succeed, object.set_value(value), fail0),
        fail,
    ),
)
define_generic_procedure_handler(
    a.advance,
    match_args(is_advanced_memo),
    lambda object, succeed, fail: continue_with(succeed, advanced_value(object), fail),
)


def repl():
    timer = time()

    def internal_loop(succeed, fail):
        nonlocal timer

        print("> ", end="")
        inputs = tuple(read(input()))

        expression = inputs[0] if len(inputs) == 1 else (S.BEGIN,) + inputs

        def fail_k(*args):
            print(f";;; {time() - timer} seconds elapsed")
            print(";;; There are no more values of", expression)
            return continue_with(internal_loop, success_k, no_problem)

        timer = time()
        if expression == symbol("try-again"):
            return continue_with(fail)
        else:
            print(";;; Starting a new problem")
            return continue_with(
                a.eval, expression, THE_GLOBAL_ENVIRONMENT, succeed, fail_k
            )

    def success_k(value, fail):
        print(f";;; {time() - timer} seconds elapsed")
        display(value)
        print()
        return continue_with(internal_loop, success_k, fail)

    def no_problem():
        print(";;; There is no current problem")
        return continue_with(internal_loop, success_k, no_problem)

    run_execution(internal_loop, success_k, no_problem)


def procedure_name(procedure):
    return PROCEDURE_NAMES.get(procedure, "<unknown>")


TRACE_APPLICATIONS = False
PROCEDURE_NAMES = {}


if __name__ == "__main__":
    repl()
