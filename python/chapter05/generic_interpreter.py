from dataclasses import dataclass

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
    return obj is not False


class Symbol:
    def __init__(self, name):
        self.name = name

    def __eq__(self, other):
        return self is other or isinstance(other, Symbol) and self.name == other.name

    def __repr__(self):
        return self.name


@tc_enable(simple=True)
def symbol(name):
    return Symbol(name)


@tc_enable(simple=True)
def is_symbol(obj):
    return isinstance(obj, Symbol)


# representation of pairs that transparently supports tuples and lists.
# For example, the tuple (1, 2, 3) and the list [1, 2, 3] are
# equivalent to the nested pairs (1 . (2 . (3 . ()))).


@tc_enable(simple=True)
def cons(car, cdr):
    return Pair(car, cdr)


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


@tc_enable()
def lookup_variable_value(variable, environment):
    raise NotImplementedError()


@tc_enable()
def set_variable_value(variable, value, environment):
    raise NotImplementedError()


@tc_enable()
def default_eval(expression, environment):
    if is_application(expression):
        g.apply.tail_call(
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
    eval = simple_generic_procedure("g:eval", 2, default_eval)
    advance = simple_generic_procedure("g:advance", 1, lambda x: x)
    apply = simple_generic_procedure("g:apply", 3, default_apply)


@tc_enable(simple=True)
def is_number(obj):
    return isinstance(obj, int) or isinstance(obj, float)


@tc_enable(simple=True)
def is_boolean(obj):
    return isinstance(obj, bool)


@tc_enable(simple=True)
def is_string(obj):
    return isinstance(obj, str)


define_generic_procedure_handler(
    g.eval, match_args(is_number, is_environment), lambda expr, env: expr
)
define_generic_procedure_handler(
    g.eval, match_args(is_boolean, is_environment), lambda expr, env: expr
)
define_generic_procedure_handler(
    g.eval, match_args(is_string, is_environment), lambda expr, env: expr
)


@tc_enable(simple=True)
def is_quoted(exp):
    return is_tagged_list(exp, symbol("quote"))


@tc_enable()
def text_of_quotation(quot):
    return cadr.tailcall(quot)


define_generic_procedure_handler(
    g.eval,
    match_args(is_quoted, is_environment),
    lambda expr, env: text_of_quotation(expr),
)


@tc_enable(simple=True)
def is_variable(exp):
    return is_symbol(exp)


define_generic_procedure_handler(
    g.eval, match_args(is_variable, is_environment), lookup_variable_value
)


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


define_generic_procedure_handler(
    g.eval,
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
    actions = map(lambda exp: begin_actions(exp) if is_begin(exp) else (exp,), seq)
    return make_begin.tailcall(tuple(item for sublist in actions for item in sublist))


@tc_enable()
def evaluate_sequence(actions, environment):
    if is_null(actions):
        raise SyntaxError("Empty sequence")
    if is_null(cdr(actions)):
        return g.eval.tailcall(car(actions), environment)
    g.eval(car(actions), environment)
    return evaluate_sequence.tailcall(cdr(actions), environment)


define_generic_procedure_handler(
    g.eval,
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


define_generic_procedure_handler(
    g.eval,
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


define_generic_procedure_handler(
    g.eval,
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
    return list(map(car, cadr(let_exp)))


@tc_enable(simple=True)
def let_bound_values(let_exp):
    return list(map(cadr, cadr(let_exp)))


@tc_enable()
def let_body(let_exp):
    return sequence_begin.tailcall(cddr(let_exp))


@tc_enable()
def let_to_combination(let_exp):
    names = let_bound_variables(let_exp)
    values = let_bound_values(let_exp)
    body = let_body(let_exp)
    return cons.tailcall(make_lambda(names, body), values)


define_generic_procedure_handler(
    g.eval,
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

define_generic_procedure_handler(
    g.eval,
    match_args(is_assignment, is_environment),
    lambda expression, environment: (
        set_variable_value.tailcall(
            assignment_variable(expression),
            g.eval(assignment_value(expression)),
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

define_generic_procedure_handler(
    g.eval,
    match_args(is_definition, is_environment),
    lambda expression, environment: (
        define_variable.tailcall(
            definition_variable(expression),
            g.eval(definition_value(expression)),
            environment,
        )
    ),
)


##

g.eval = TcEnable(g.eval)
g.advance = TcEnable(g.advance)
g.apply = TcEnable(g.apply)

##

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
