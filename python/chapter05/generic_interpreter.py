from dataclasses import dataclass

from chapter03.generic_procedures import simple_generic_procedure, define_generic_procedure_handler, match_args


class TcEnable:
    """Use as decorator to make functions tail-call enabled.
    This is contagious:
    Functions that can be the target tail calls must obviously be decorated.
    Functions that do tail-calls must be decorated too.
    So basically every function must be decorated...
    """

    def __init__(self, func, simple=False):
        if simple:
            self.func = func
        else:
            self.func = wrap_trampoline(func)

    def __call__(self, *args, **kwargs):
        return self.func(*args, **kwargs)

    def tailcall(self, *args, **kwargs):
        raise TailCall(self.func, args, kwargs)


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
def default_eval(expression, environment):
    if is_application(expression):
        tail_call(g.apply,
                  g.advance(g.eval(operator(expression),
                                   environment)),
                  operands(expression),
                  environment)
    else:
        raise TypeError(f"Unknown expression type {expression}")


@tc_enable(simple=False)
def is_application(exp): return is_pair(exp)


def operator(exp): return car(exp)


def operands(exp): return cdr(exp)


def default_apply(procedure, _operands, _calling_environment):
    raise TypeError(f"Unknown procedure type {procedure}")


class g:
    eval = simple_generic_procedure("g:eval", 2, default_eval)
    advance = simple_generic_procedure("g:advance", 1, lambda x: x)
    apply = simple_generic_procedure("g:apply", 3, default_apply)


def is_number(obj):
    return isinstance(obj, int) or isinstance(obj, float)


def is_boolean(obj):
    return isinstance(obj, bool)


def is_string(obj):
    return isinstance(obj, str)


define_generic_procedure_handler(g.eval, match_args(is_number, is_environment), lambda expr, env: expr)
define_generic_procedure_handler(g.eval, match_args(is_boolean, is_environment), lambda expr, env: expr)
define_generic_procedure_handler(g.eval, match_args(is_string, is_environment), lambda expr, env: expr)


def is_quoted(exp):
    return is_tagged_list(exp, symbol('quote'))


def text_of_quotation(quot):
    return cadr(quot)


define_generic_procedure_handler(g.eval, match_args(is_quoted, is_environment),
                                 lambda expr, env: text_of_quotation(expr))


def is_variable(exp):
    return is_symbol(exp)


define_generic_procedure_handler(g.eval, match_args(is_variable, is_environment), lookup_variable_value)


def is_if(exp):
    return is_tagged_list(exp, Symbol('if'))


def if_predicate(exp): return cadr(exp)


def if_consequent(exp): return caddr(exp)


def if_alternative(exp):
    if is_null(cdddr(exp)):
        return Symbol('the-unspecified-value')
    else:
        return cadddr(exp)


def make_if(pred, conseq, alternative):
    return Symbol('if'), pred, conseq, alternative


define_generic_procedure_handler(g.eval, match_args(is_if, is_environment),
                                 lambda expression, environment: (
                                     g.eval(if_consequent(expression), environment)
                                     if boolean(g.advance(g.eval(if_predicate(expression), environment)))
                                     else g.eval(if_alternative(expression), environment)))


def is_begin(exp):
    return is_tagged_list(exp, Symbol('begin'))


def begin_actions(begin_exp):
    return cdr(begin_exp)


def make_begin(actions):
    return cons(Symbol('begin'), actions)


def sequence_begin(seq):
    if is_null(seq):
        return seq
    if is_null(cdr(seq)):
        return car(seq)
    actions = map(lambda exp: begin_actions(exp) if is_begin(exp) else (exp,), seq)
    return make_begin(tuple(item for sublist in actions for item in sublist))


@tc_enable()
def evaluate_sequence(actions, environment):
    if is_null(actions):
        raise SyntaxError("Empty sequence")
    if is_null(cdr(actions)):
        return g.eval(car(actions), environment)
    g.eval(car(actions), environment)
    return evaluate_sequence.tailcall(cdr(actions), environment)


define_generic_procedure_handler(g.eval, match_args(is_begin, is_environment),
                                 lambda expression, environment: (
                                     evaluate_sequence(begin_actions(expression), environment)))


def is_lambda(exp):
    return is_tagged_list(exp, Symbol('lambda'))


def lambda_parameters(lambda_exp):
    return cadr(lambda_exp)


def lambda_body(lambda_exp):
    full_body = cddr(lambda_exp)
    return sequence_begin(full_body)


def make_lambda(parameters, body):
    return cons(Symbol('lambda'),
                cons(parameters,
                     begin_actions(body) if is_begin(body) else (body,)))


class CompoundProcedure:
    def __init__(self, vars, bproc, env):
        self.vars = vars
        self.bproc = bproc
        self.env = env


def is_compound_procedure(obj):
    return isinstance(obj, CompoundProcedure)


def make_compound_procedure(vars, bproc, env):
    return CompoundProcedure(vars, bproc, env)


def procedure_parameters(cproc):
    return cproc.vars


def procedure_body(cproc):
    return cproc.bproc


def procedure_environment(cproc):
    return cproc.env


define_generic_procedure_handler(g.eval, match_args(is_lambda, is_environment),
                                 lambda expression, environment: (
                                     make_compound_procedure(lambda_parameters(expression),
                                                             lambda_body(expression),
                                                             environment)))


def is_cond(exp):
    return is_tagged_list(exp, Symbol('cond'))


def cond_clauses(exp): return cdr(exp)


def cond_clause_predicate(clause): return car(clause)


def cond_clause_consequent(clause): return sequence_begin(cdr(clause))


def is_else_clause(clause): return cond_clause_predicate(clause) == Symbol('else')


def cond_to_if(cond_exp):
    def expand(clauses):
        if is_null(clauses):
            raise ValueError("COND: no values matched")
        if is_else_clause(car(clauses)):
            if is_null(cdr(clauses)):
                return cond_clause_consequent(car(clauses))
            else:
                raise SyntaxError(f"COND: ELSE not last {cond_exp}")
        return make_if(cond_clause_predicate(car(clauses)),
                       cond_clause_consequent(car(clauses)),
                       expand(cdr(clauses)))

    return expand(cond_clauses(cond_exp))


define_generic_procedure_handler(g.eval, match_args(is_cond, is_environment),
                                 lambda expression, environment: (
                                     g.eval(cond_to_if(expression), environment)))


def is_let(exp):
    return is_tagged_list(exp, Symbol('let'))


def let_bound_variables(let_exp):
    return list(map(car, cadr(let_exp)))


def let_bound_values(let_exp):
    return list(map(cadr, cadr(let_exp)))


def let_body(let_exp):
    return sequence_begin(cddr(let_exp))


def let_to_combination(let_exp):
    names = let_bound_variables(let_exp)
    values = let_bound_values(let_exp)
    body = let_body(let_exp)
    return cons(make_lambda(names, body), values)


define_generic_procedure_handler(g.eval, match_args(is_let, is_environment),
                                 lambda expression, environment: (
                                     g.eval(let_to_combination(expression), environment)))

a = Symbol('a')
b = Symbol('b')
c = Symbol('c')

print((a, b, c))

print(car((1, 2, 3)), cdr((1, 2, 3)))
x = cons(1, (2, 3, 4))
print(x)
print(car(x), cdr(x), cdr(cdr(x)))

quote = Symbol('quote')
x = Symbol('x')
begin = Symbol('begin')

#print(g.eval(((Symbol('lambda'), (x,), x), 42), ()))
print(g.eval((begin, 1, 2, 3), ()))
