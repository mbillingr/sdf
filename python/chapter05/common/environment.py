THE_EMPTY_ENVIRONMENT = ()


def is_environment(obj):
    return True


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
