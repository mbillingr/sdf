import re
from dataclasses import dataclass

from chapter03.generic_procedures import (
    simple_generic_procedure,
    define_generic_procedure_handler,
    match_args,
)


def tail_call(func, *args, **kwargs):
    raise TailCall(func, args, kwargs)


class TailCall(Exception):
    def __init__(self, func, args=(), kwargs=None):
        self.func = func
        self.args = args
        self.kwargs = kwargs or {}


def trampoline(func, *args, **kwargs):
    while True:
        try:
            return func(*args, **kwargs)
        except TailCall as tc:
            func = tc.func
            args = tc.args
            kwargs = tc.kwargs


def boolean(obj):
    return obj is not False


class Symbol:
    def __init__(self, name):
        self.name = name

    def __eq__(self, other):
        return self is other or isinstance(other, Symbol) and self.name == other.name

    def __repr__(self):
        return self.name

    def __hash__(self):
        return hash((Symbol, self.name))


def symbol(name):
    return Symbol(name)


def is_symbol(obj):
    return isinstance(obj, Symbol)


# representation of pairs that transparently supports tuples and lists.
# For example, the tuple (1, 2, 3) and the list [1, 2, 3] are
# equivalent to the nested pairs (1 . (2 . (3 . ()))).


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


def is_environment(obj):
    return True


THE_EMPTY_ENVIRONMENT = ()


def lookup_variable_value(variable, environment):
    if not environment:
        return None
    current, parent = environment
    try:
        return current[variable]
    except KeyError:
        pass
    return lookup_variable_value(variable, parent)


def set_variable_value(variable, value, environment):
    if not environment:
        return None
    current, parent = environment
    if variable in current:
        current[variable] = value
    else:
        set_variable_value(variable, value, parent)


def define_variable(variable, value, environment):
    current, parent = environment
    current[variable] = value


def extend_environment(variables, values, base_environment):
    return {var: val for var, val in zip(variables, values)}, base_environment


def default_eval(expression, environment):
    if is_application(expression):
        return g.apply(
            g.advance(g.eval(operator(expression), environment)),
            operands(expression),
            environment,
        )
    else:
        raise TypeError(f"Unknown expression type {expression}")


def is_application(exp):
    return is_pair(exp)


def operator(exp):
    return car(exp)


def operands(exp):
    return cdr(exp)


def default_apply(procedure, _operands, _calling_environment):
    raise TypeError(f"Unknown procedure type {procedure}")


class g:
    eval = simple_generic_procedure("g:eval", 2, default_eval)
    advance = simple_generic_procedure("g:advance", 1, lambda x: x)
    apply = simple_generic_procedure("g:apply", 3, default_apply)

    @staticmethod
    def define_eval_handler(applicability, handler):
        define_generic_procedure_handler(g.eval, applicability, handler)

    @staticmethod
    def define_advance_handler(applicability, handler):
        define_generic_procedure_handler(g.raw_advance, applicability, handler)

    @staticmethod
    def define_apply_handler(applicability, handler):
        define_generic_procedure_handler(g.apply, applicability, handler)

    TOKENIZE = re.compile(r'(\'|\(|\)|\[|]|\s+|".*?")')

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

        if token in (")", "]"):
            raise ValueError(f"unexpected token {token}")
        if token == "(":
            return tuple(self.parse_list())
        if token == "[":
            return list(self.parse_list())

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


def is_number(obj):
    return isinstance(obj, int) or isinstance(obj, float)


def is_boolean(obj):
    return isinstance(obj, bool)


def is_string(obj):
    return isinstance(obj, str)


def is_python_list(obj):
    return isinstance(obj, list)


g.define_eval_handler(
    match_args(is_python_list, is_environment), lambda expr, env: expr
)
g.define_eval_handler(match_args(is_number, is_environment), lambda expr, env: expr)
g.define_eval_handler(match_args(is_boolean, is_environment), lambda expr, env: expr)
g.define_eval_handler(match_args(is_string, is_environment), lambda expr, env: expr)


def is_quoted(exp):
    return is_tagged_list(exp, symbol("quote"))


def text_of_quotation(quot):
    return cadr(quot)


g.define_eval_handler(
    match_args(is_quoted, is_environment),
    lambda expr, env: text_of_quotation(expr),
)


def is_variable(exp):
    return is_symbol(exp)


g.define_eval_handler(match_args(is_variable, is_environment), lookup_variable_value)


def is_if(exp):
    return is_tagged_list(exp, Symbol("if"))


def if_predicate(exp):
    return cadr(exp)


def if_consequent(exp):
    return caddr(exp)


def if_alternative(exp):
    if is_null(cdddr(exp)):
        return Symbol("the-unspecified-value")
    else:
        return cadddr(exp)


def make_if(pred, conseq, alternative):
    return Symbol("if"), pred, conseq, alternative


g.define_eval_handler(
    match_args(is_if, is_environment),
    lambda expression, environment: (
        g.eval(if_consequent(expression), environment)
        if boolean(g.advance(g.eval(if_predicate(expression), environment)))
        else g.eval(if_alternative(expression), environment)
    ),
)


def is_begin(exp):
    return is_tagged_list(exp, Symbol("begin"))


def begin_actions(begin_exp):
    return cdr(begin_exp)


def make_begin(actions):
    return cons(Symbol("begin"), actions)


def sequence_begin(seq):
    if is_null(seq):
        return seq
    if is_null(cdr(seq)):
        return car(seq)
    actions = tuple(
        map(lambda exp: begin_actions(exp) if is_begin(exp) else (exp,), seq)
    )
    return make_begin(tuple(item for sublist in actions for item in sublist))


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


def is_lambda(exp):
    return is_tagged_list(exp, Symbol("lambda"))


def lambda_parameters(lambda_exp):
    return cadr(lambda_exp)


def lambda_body(lambda_exp):
    full_body = cddr(lambda_exp)
    return sequence_begin(full_body)


def make_lambda(parameters, body):
    return cons(
        Symbol("lambda"),
        cons(parameters, begin_actions(body) if is_begin(body) else (body,)),
    )


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


g.define_eval_handler(
    match_args(is_lambda, is_environment),
    lambda expression, environment: (
        make_compound_procedure(
            lambda_parameters(expression), lambda_body(expression), environment
        )
    ),
)


def is_cond(exp):
    return is_tagged_list(exp, Symbol("cond"))


def cond_clauses(exp):
    return cdr(exp)


def cond_clause_predicate(clause):
    return car(clause)


def cond_clause_consequent(clause):
    return sequence_begin(cdr(clause))


def is_else_clause(clause):
    return cond_clause_predicate(clause) == Symbol("else")


def cond_to_if(cond_exp):
    def expand(clauses):
        if is_null(clauses):
            raise ValueError("COND: no values matched")
        if is_else_clause(car(clauses)):
            if is_null(cdr(clauses)):
                return cond_clause_consequent(car(clauses))
            else:
                raise SyntaxError(f"COND: ELSE not last {cond_exp}")
        return make_if(
            cond_clause_predicate(car(clauses)),
            cond_clause_consequent(car(clauses)),
            expand(cdr(clauses)),
        )

    return expand(cond_clauses(cond_exp))


g.define_eval_handler(
    match_args(is_cond, is_environment),
    lambda expression, environment: (g.eval(cond_to_if(expression), environment)),
)


def is_let(exp):
    return is_tagged_list(exp, Symbol("let"))


def let_bound_variables(let_exp):
    return tuple(map(car, cadr(let_exp)))


def let_bound_values(let_exp):
    return tuple(map(cadr, cadr(let_exp)))


def let_body(let_exp):
    return sequence_begin(cddr(let_exp))


def let_to_combination(let_exp):
    names = let_bound_variables(let_exp)
    values = let_bound_values(let_exp)
    body = let_body(let_exp)
    return cons(make_lambda(names, body), values)


g.define_eval_handler(
    match_args(is_let, is_environment),
    lambda expression, environment: (
        g.eval(let_to_combination(expression), environment)
    ),
)


def is_assignment(exp):
    return is_tagged_list(exp, Symbol("set!"))


assignment_variable = cadr
assignment_value = caddr

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
        define_variable(
            definition_variable(expression),
            g.eval(definition_value(expression), environment),
            environment,
        )
    ),
)

# apply handlers

fntype = type(lambda: 0)


def is_strict_primitive_procedure(obj):
    return isinstance(obj, fntype)


def is_operands(obj):
    return is_null(obj) or is_pair(obj)


def eval_operands(operands, calling_environment):
    return tuple(
        g.advance(g.eval(operand, calling_environment)) for operand in operands
    )


def apply_primitive_procedure(procedure, operands):
    return procedure(*operands)


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


def apply_list_of_procedures(procedures, operands, calling_environment):
    return list(
        map(
            lambda proc: g.apply(
                g.eval(proc, calling_environment), operands, calling_environment
            ),
            procedures,
        )
    )


g.define_apply_handler(
    match_args(is_python_list, is_operands, is_environment),
    apply_list_of_procedures,
)

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
        ans = g.eval(input, THE_GLOBAL_ENVIRONMENT)
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


INITIAL_ENV_BINDINGS = {
    Symbol("+"): lambda a, b: a + b,
    Symbol("-"): lambda a, b: a - b,
    Symbol("*"): lambda a, b: a * b,
    Symbol("/"): lambda a, b: a / b,
    Symbol("="): lambda a, b: a == b,
    Symbol("<"): lambda a, b: a < b,
    Symbol(">"): lambda a, b: a > b,
    Symbol("<="): lambda a, b: a <= b,
    Symbol(">="): lambda a, b: a >= b,
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


def test_apply_many():
    initialize_repl()
    assert eval_str("([+ - * / (lambda (a b) a) (lambda (a b) b)] 9 3)") == [
        12,
        6,
        27,
        3,
        9,
        3,
    ]
