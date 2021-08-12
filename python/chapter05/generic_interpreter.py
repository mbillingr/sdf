import re
from dataclasses import dataclass
from typing import Any

from chapter03.generic_procedures import (
    simple_generic_procedure,
    define_generic_procedure_handler,
    match_args,
)


class TcEnable:
    """Use as decorator to make functions tail-call enabled.
    This is contagious:
    Functions that can be the target tail calls must obviously be decorated.
    Functions that do tail-calls must be decorated too.
    So basically every function must be decorated...
    """

    def __init__(self, func, simple=False):
        self.raw_func = func
        if simple:
            self.func = func
        else:
            self.func = wrap_trampoline(func)

    def __call__(self, *args, **kwargs):
        return self.func(*args, **kwargs)

    def tailcall(self, *args, **kwargs):
        raise TailCall(self.raw_func, args, kwargs)


def tail_call(func, *args, **kwargs):
    raise TailCall(func, args, kwargs)


class TailCall(BaseException):
    def __init__(self, func, args=(), kwargs={}):
        self.func = func
        self.args = args
        self.kwargs = kwargs


def tc_enable(simple=False):
    return lambda func: TcEnable(func, simple)


def wrap_trampoline(function):
    def trampoline(*args, **kwargs):
        func = function
        while True:
            try:
                return func(*args, **kwargs)
            except TailCall as tc:
                func = tc.func
                args = tc.args
                kwargs = tc.kwargs

    return trampoline


@tc_enable(simple=True)
def boolean(obj):
    return obj is not False and obj is not None


class Symbol:
    def __init__(self, name):
        self.name = name

    def __eq__(self, other):
        return self is other or isinstance(other, Symbol) and self.name == other.name

    def __repr__(self):
        return self.name

    def __hash__(self):
        return hash((Symbol, self.name))


@tc_enable(simple=True)
def symbol(name):
    global INTERNED_SYMBOLS
    try:
        return INTERNED_SYMBOLS[name]
    except KeyError:
        s = Symbol(name)
        INTERNED_SYMBOLS[name] = s
    return s


INTERNED_SYMBOLS = {}


@tc_enable(simple=True)
def is_symbol(obj):
    return isinstance(obj, Symbol)


# representation of pairs that transparently supports tuples and lists.
# For example, the tuple (1, 2, 3) and the list [1, 2, 3] are
# equivalent to the nested pairs (1 . (2 . (3 . ()))).


@tc_enable(simple=True)
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


@tc_enable(simple=True)
def is_pair(obj):
    return isinstance(obj, Pair) or isinstance(obj, tuple) and len(obj) >= 1


@tc_enable(simple=True)
def is_null(obj):
    return obj == ()


@tc_enable(simple=True)
def car(obj):
    if isinstance(obj, Pair):
        return obj.car
    return obj[0]


@tc_enable(simple=True)
def cdr(obj):
    if isinstance(obj, Pair):
        return obj.cdr
    return obj[1:]


@tc_enable()
def cadr(obj):
    return car.tailcall(cdr(obj))


@tc_enable()
def cddr(obj):
    return cdr.tailcall(cdr(obj))


@tc_enable()
def caadr(obj):
    return car.tailcall(car(cdr(obj)))


@tc_enable()
def caddr(obj):
    return car.tailcall(cdr(cdr(obj)))


@tc_enable()
def cdadr(obj):
    return cdr.tailcall(car(cdr(obj)))


@tc_enable()
def cdddr(obj):
    return cdr.tailcall(cdr(cdr(obj)))


@tc_enable()
def cadddr(obj):
    return car.tailcall(cdr(cdr(cdr(obj))))


@tc_enable(simple=True)
def is_tagged_list(exp, tag):
    return is_pair(exp) and car(exp) == tag


@tc_enable()
def is_environment(obj):
    return True


THE_EMPTY_ENVIRONMENT = ()


@tc_enable()
def lookup_variable_value(variable, environment):
    if not environment:
        return None
    current, parent = environment
    try:
        return current[variable]
    except KeyError:
        pass
    return lookup_variable_value(variable, parent)


@tc_enable()
def set_variable_value(variable, value, environment):
    if not environment:
        return None
    current, parent = environment
    if variable in current:
        current[variable] = value
    else:
        set_variable_value(variable, value, parent)


@tc_enable()
def define_variable(variable, value, environment):
    current, parent = environment
    current[variable] = value


@tc_enable()
def extend_environment(variables, values, base_environment):
    return {var: val for var, val in zip(variables, values)}, base_environment


@tc_enable()
def default_eval(expression, environment):
    if is_application(expression):
        return g.apply.tailcall(
            g.advance(g.eval(operator(expression), environment)),
            operands(expression),
            environment,
        )
    else:
        raise TypeError(f"Unknown expression type {expression}")


@tc_enable()
def is_application(exp):
    return is_pair.tailcall(exp)


@tc_enable()
def operator(exp):
    return car.tailcall(exp)


@tc_enable()
def operands(exp):
    return cdr.tailcall(exp)


def default_apply(procedure, _operands, _calling_environment):
    raise TypeError(f"Unknown procedure type {procedure}")


class g:
    raw_eval = simple_generic_procedure("g:eval", 2, default_eval)
    raw_advance = simple_generic_procedure("g:advance", 1, lambda x: x)
    raw_apply = simple_generic_procedure("g:apply", 3, default_apply)
    eval = tc_enable()(raw_eval)
    advance = tc_enable()(raw_advance)
    apply = tc_enable()(raw_apply)

    handle_operand = simple_generic_procedure(
        "g:handle-operand",
        3,
        lambda parameter, operand, environment: g.advance(g.eval(operand, environment)),
    )

    @staticmethod
    def define_eval_handler(applicability, handler):
        define_generic_procedure_handler(g.raw_eval, applicability, handler)

    @staticmethod
    def define_advance_handler(applicability, handler):
        define_generic_procedure_handler(g.raw_advance, applicability, handler)

    @staticmethod
    def define_apply_handler(applicability, handler):
        define_generic_procedure_handler(g.raw_apply, applicability, handler)

    TOKENIZE = re.compile(r'(\'|\(|\)|\s+|".*?")')

    @staticmethod
    def read(raw_string=None):
        raw_string = input() if raw_string is None else raw_string
        tokens = g.TOKENIZE.split(raw_string)
        tokens = [
            token
            for token in tokens
            if token and not (token.isspace() or token == "\n")
        ]
        yield from Parser(tokens).parse()


class Parser:
    def __init__(self, tokens):
        self.tokens = iter(tokens)
        self.current_token = next(self.tokens)

    def advance(self):
        self.current_token = next(self.tokens)

    def parse(self):
        while True:
            yield self.parse_item()
            try:
                self.advance()
            except StopIteration:
                return

    def parse_item(self):
        token = self.current_token

        if token == ")":
            raise ValueError(f"unexpected token {token}")
        if token == "(":
            return tuple(self.parse_list())

        if token == "'":
            self.advance()
            return Symbol("quote"), self.parse_item()

        if token.startswith('"') and token.endswith('"'):
            return token[1:-1]

        try:
            return int(token)
        except ValueError:
            pass

        try:
            return float(token)
        except ValueError:
            pass

        return symbol(token)

    def parse_list(self):
        end_delimiter = {"(": ")", "[": "]", "{": "}", "<": ">"}[self.current_token]
        self.advance()
        items = []
        while True:
            if self.current_token == end_delimiter:
                return items
            items.append(self.parse_item())
            self.advance()


@tc_enable(simple=True)
def is_number(obj):
    return isinstance(obj, int) or isinstance(obj, float)


@tc_enable(simple=True)
def is_boolean(obj):
    return isinstance(obj, bool)


@tc_enable(simple=True)
def is_string(obj):
    return isinstance(obj, str)


g.define_eval_handler(match_args(is_number, is_environment), lambda expr, env: expr)
g.define_eval_handler(match_args(is_boolean, is_environment), lambda expr, env: expr)
g.define_eval_handler(match_args(is_string, is_environment), lambda expr, env: expr)


@tc_enable(simple=True)
def is_quoted(exp):
    return is_tagged_list(exp, symbol("quote"))


@tc_enable()
def text_of_quotation(quot):
    return cadr.tailcall(quot)


g.define_eval_handler(
    match_args(is_quoted, is_environment),
    lambda expr, env: text_of_quotation(expr),
)


@tc_enable(simple=True)
def is_variable(exp):
    return is_symbol(exp)


g.define_eval_handler(match_args(is_variable, is_environment), lookup_variable_value)


@tc_enable(simple=True)
def is_if(exp):
    return is_tagged_list(exp, Symbol("if"))


@tc_enable()
def if_predicate(exp):
    return cadr.tailcall(exp)


@tc_enable()
def if_consequent(exp):
    return caddr.tailcall(exp)


@tc_enable()
def if_alternative(exp):
    if is_null(cdddr(exp)):
        return Symbol("the-unspecified-value")
    else:
        return cadddr.tailcall(exp)


@tc_enable(simple=True)
def make_if(pred, conseq, alternative):
    return Symbol("if"), pred, conseq, alternative


g.define_eval_handler(
    match_args(is_if, is_environment),
    lambda expression, environment: (
        g.eval.tailcall(if_consequent(expression), environment)
        if boolean(g.advance(g.eval(if_predicate(expression), environment)))
        else g.eval.tailcall(if_alternative(expression), environment)
    ),
)


@tc_enable(simple=True)
def is_begin(exp):
    return is_tagged_list(exp, Symbol("begin"))


@tc_enable()
def begin_actions(begin_exp):
    return cdr.tailcall(begin_exp)


@tc_enable()
def make_begin(actions):
    return cons.tailcall(Symbol("begin"), actions)


@tc_enable()
def sequence_begin(seq):
    if is_null(seq):
        return seq
    if is_null(cdr(seq)):
        return car.tailcall(seq)
    actions = tuple(
        map(lambda exp: begin_actions(exp) if is_begin(exp) else (exp,), seq)
    )
    return make_begin.tailcall(tuple(item for sublist in actions for item in sublist))


@tc_enable()
def evaluate_sequence(actions, environment):
    if is_null(actions):
        raise SyntaxError("Empty sequence")
    if is_null(cdr(actions)):
        return g.eval.tailcall(car(actions), environment)
    g.eval(car(actions), environment)
    return evaluate_sequence.tailcall(cdr(actions), environment)


g.define_eval_handler(
    match_args(is_begin, is_environment),
    lambda expression, environment: (
        evaluate_sequence.tailcall(begin_actions(expression), environment)
    ),
)


@tc_enable(simple=True)
def is_lambda(exp):
    return is_tagged_list(exp, Symbol("lambda"))


@tc_enable()
def lambda_parameters(lambda_exp):
    return cadr.tailcall(lambda_exp)


@tc_enable()
def lambda_body(lambda_exp):
    full_body = cddr(lambda_exp)
    return sequence_begin.tailcall(full_body)


@tc_enable(simple=True)
def make_lambda(parameters, body):
    return cons.tailcall(
        Symbol("lambda"),
        cons(parameters, begin_actions(body) if is_begin(body) else (body,)),
    )


class CompoundProcedure:
    def __init__(self, vars, bproc, env):
        self.vars = vars
        self.bproc = bproc
        self.env = env


@tc_enable(simple=True)
def is_compound_procedure(obj):
    return isinstance(obj, CompoundProcedure)


@tc_enable(simple=True)
def make_compound_procedure(vars, bproc, env):
    return CompoundProcedure(vars, bproc, env)


@tc_enable(simple=True)
def procedure_parameters(cproc):
    return cproc.vars


@tc_enable(simple=True)
def procedure_body(cproc):
    return cproc.bproc


@tc_enable(simple=True)
def procedure_environment(cproc):
    return cproc.env


g.define_eval_handler(
    match_args(is_lambda, is_environment),
    lambda expression, environment: (
        make_compound_procedure.tailcall(
            lambda_parameters(expression), lambda_body(expression), environment
        )
    ),
)


@tc_enable(simple=True)
def is_cond(exp):
    return is_tagged_list(exp, Symbol("cond"))


@tc_enable(simple=True)
def cond_clauses(exp):
    return cdr(exp)


@tc_enable(simple=True)
def cond_clause_predicate(clause):
    return car(clause)


@tc_enable()
def cond_clause_consequent(clause):
    return sequence_begin.tailcall(cdr(clause))


@tc_enable(simple=True)
def is_else_clause(clause):
    return cond_clause_predicate(clause) == Symbol("else")


@tc_enable()
def cond_to_if(cond_exp):
    @tc_enable()
    def expand(clauses):
        if is_null(clauses):
            raise ValueError("COND: no values matched")
        if is_else_clause(car(clauses)):
            if is_null(cdr(clauses)):
                return cond_clause_consequent.tailcall(car(clauses))
            else:
                raise SyntaxError(f"COND: ELSE not last {cond_exp}")
        return make_if.tailcall(
            cond_clause_predicate(car(clauses)),
            cond_clause_consequent(car(clauses)),
            expand(cdr(clauses)),
        )

    return expand.tailcall(cond_clauses(cond_exp))


g.define_eval_handler(
    match_args(is_cond, is_environment),
    lambda expression, environment: (
        g.eval.tailcall(cond_to_if(expression), environment)
    ),
)


@tc_enable(simple=True)
def is_let(exp):
    return is_tagged_list(exp, Symbol("let"))


@tc_enable(simple=True)
def let_bound_variables(let_exp):
    return tuple(map(car, cadr(let_exp)))


@tc_enable(simple=True)
def let_bound_values(let_exp):
    return tuple(map(cadr, cadr(let_exp)))


@tc_enable()
def let_body(let_exp):
    return sequence_begin.tailcall(cddr(let_exp))


@tc_enable()
def let_to_combination(let_exp):
    names = let_bound_variables(let_exp)
    values = let_bound_values(let_exp)
    body = let_body(let_exp)
    return cons.tailcall(make_lambda(names, body), values)


g.define_eval_handler(
    match_args(is_let, is_environment),
    lambda expression, environment: (
        g.eval.tailcall(let_to_combination(expression), environment)
    ),
)


@tc_enable(simple=True)
def is_assignment(exp):
    return is_tagged_list(exp, Symbol("set!"))


assignment_variable = cadr
assignment_value = caddr

g.define_eval_handler(
    match_args(is_assignment, is_environment),
    lambda expression, environment: (
        set_variable_value.tailcall(
            assignment_variable(expression),
            g.eval(assignment_value(expression), environment),
            environment,
        )
    ),
)


@tc_enable(simple=True)
def is_definition(exp):
    return is_tagged_list(exp, Symbol("define"))


def definition_variable(defn):
    if is_variable(cadr(defn)):
        return cadr(defn)
    else:
        return caadr(defn)


def definition_value(defn):
    if is_variable(cadr(defn)):
        return caddr(defn)
    else:
        return cons(symbol("lambda"), cons(cdadr(defn), cddr(defn)))


assignment_value = caddr

g.define_eval_handler(
    match_args(is_definition, is_environment),
    lambda expression, environment: (
        define_variable.tailcall(
            definition_variable(expression),
            g.eval(definition_value(expression), environment),
            environment,
        )
    ),
)

# apply handlers


@tc_enable(simple=True)
def is_strict_primitive_procedure(obj):
    return callable(obj)


@tc_enable()
def is_operands(obj):
    return is_null(obj) or is_pair(obj)


def eval_operands(operands, calling_environment):
    return tuple(
        g.advance(g.eval(operand, calling_environment)) for operand in operands
    )


@tc_enable()
def apply_primitive_procedure(procedure, operands):
    return tail_call(procedure, *operands)


g.define_apply_handler(
    match_args(is_strict_primitive_procedure, is_operands, is_environment),
    lambda procedure, operands, calling_environment: apply_primitive_procedure(
        procedure, eval_operands(operands, calling_environment)
    ),
)


@tc_enable(simple=True)
def is_strict_compound_procedure(obj):
    return is_compound_procedure(obj) and all(map(is_symbol, procedure_parameters(obj)))


def apply_strict_compound_procedure(procedure, operands, calling_environment):
    if length(procedure_parameters(procedure)) != length(operands):
        raise TypeError("Wrong number of arguments supplied")
    return g.eval.tailcall(
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

procedure_parameter_name = simple_generic_procedure("parameter-name", 1, lambda x: x)

define_generic_procedure_handler(procedure_parameter_name, match_args(is_pair), car)


@tc_enable(simple=True)
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


def is_lazy(var_decl):
    return (
        is_pair(var_decl)
        and memq(symbol("lazy"), cdr(var_decl))
        and not memq(symbol("memo"), cdr(var_decl))
    )


def is_lazy_memo(var_decl):
    return (
        is_pair(var_decl)
        and memq(symbol("lazy"), cdr(var_decl))
        and memq(symbol("memo"), cdr(var_decl))
    )


def is_operand(_exp):
    return True


def is_postponed(obj):
    return isinstance(obj, Postponed)


@dataclass
class Postponed:
    expression: Any
    environment: Any


def postpone(expression, environment):
    return Postponed(expression, environment)


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
    lambda object: g.advance(
        g.eval(postponed_expression(object), postponed_environment(object))
    ),
)


def is_postponed_memo(obj):
    return isinstance(obj, PostponedMemo) and obj.value is None


def is_advanced_memo(obj):
    return isinstance(obj, PostponedMemo) and obj.value is not None


def postpone_memo(expression, environment):
    return PostponedMemo(expression, environment)


def advanced_value(x):
    return x.value


@dataclass
class PostponedMemo:
    expression: Any
    environment: Any
    value: Any = None

    def set_value(self, value):
        self.value = value
        self.expression = None
        self.environment = THE_EMPTY_ENVIRONMENT


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
    for input in g.read(s):
        ans = g.advance(g.eval(input, THE_GLOBAL_ENVIRONMENT))
    return ans


THE_GLOBAL_ENVIRONMENT = "not initialized"


def initialize_repl():
    global THE_GLOBAL_ENVIRONMENT
    THE_GLOBAL_ENVIRONMENT = make_global_environment()


def check_repl_initialized():
    if THE_GLOBAL_ENVIRONMENT == "not initialized":
        raise RuntimeError("Interpreter not initialized. Run init() first.")


def make_global_environment():
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
}

##

display = simple_generic_procedure("display", 1, lambda obj: print(obj, end=""))


def display_pair(p, parens=True):
    if parens:
        print("(", end="")
    display(car(p))
    if is_null(cdr(p)):
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

##

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
