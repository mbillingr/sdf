from chapter05.common.pairs import cadr, car, cddr, cdr, cons, is_null, is_tagged_list
from chapter05.common.primitive_types import Symbol
from chapter05.common.syntax import make_if, make_lambda, sequence_begin


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
