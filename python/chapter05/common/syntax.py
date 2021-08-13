from chapter05.common import symbols as S
from chapter05.common.pairs import (
    caadr,
    cadddr,
    caddr,
    cadr,
    car,
    cdadr,
    cdddr,
    cddr,
    cdr,
    cons,
    is_null,
    is_pair,
    is_tagged_list,
    memq,
)
from chapter05.common.primitive_types import Symbol, is_symbol, symbol


def is_application(exp):
    return is_pair(exp)


def operator(exp):
    return car(exp)


def operands(exp):
    return cdr(exp)


def is_quoted(exp):
    return is_tagged_list(exp, S.QUOTE)


def text_of_quotation(quot):
    return cadr(quot)


def is_variable(exp):
    return is_symbol(exp)


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


def is_assignment(exp):
    return is_tagged_list(exp, Symbol("set!"))


assignment_variable = cadr
assignment_value = caddr


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
