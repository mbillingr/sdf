from chapter03.generic_procedures import (
    define_generic_procedure_handler,
    match_args,
    simple_generic_procedure,
)

from chapter05.common.pairs import car, is_null, is_pair


def is_strict_primitive_procedure(obj):
    return callable(obj)


def is_operands(obj):
    return is_null(obj) or is_pair(obj)


def apply_primitive_procedure(procedure, operands):
    return procedure(*operands)


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


procedure_parameter_name = simple_generic_procedure("parameter-name", 1, lambda x: x)
define_generic_procedure_handler(procedure_parameter_name, match_args(is_pair), car)
