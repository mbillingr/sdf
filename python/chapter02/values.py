"""Positional and Keyword return values

In Python we can pass positional and keyword arguments to functions
but we are restricted to a single return value.
Although it's possible to return multiple values in a compound structure
(usually a tuple), the caller cannot know if the function meant to return
multiple values or a single structured value. This can be a problem in
generic function composition.

This module attempts to establish symmetry between function arguments
and return values by introducing multiple positional and keyword return
values.

The function `apply` passes either a single value to a callable, or if
its second argument represents multiple values it passes these as
positional and keyword arguments to the callable.
"""


def apply(proc, arg_val):
    """Apply `proc` to `arg_val`.

    :param proc: a callable
    :param arg_val: values to pass to `proc`
    :return: return value of `proc`
    """
    try:
        f = arg_val.__apply__
    except AttributeError:
        return proc(arg_val)
    return f(proc)


class Values:
    """Representation of multiple positional and keyword values.
    """

    def __init__(self, *args, **kwargs):
        if len(args) == 1 and isinstance(args[0], Values):
            assert not kwargs
            kwargs = args[0].by_keyword
            args = args[0].positional
        self.positional = args
        self.by_keyword = kwargs

    def __apply__(self, proc):
        """apply these arguments to `proc`"""
        return proc(*self.positional, **self.by_keyword)

    def append(self, other):
        """append positional values and merge keyword values."""
        return Values(*self.positional,
                      *other.positional,
                      **self.by_keyword,
                      **other.by_keyword)

    def __repr__(self):
        pos = [repr(p) for p in self.positional]
        kw = [f'{k}={repr(v)}' for k, v in self.by_keyword.items()]
        args = ', '.join(pos + kw)
        return f'Values({args})'

    def __eq__(self, other):
        return (isinstance(other, Values)
                and self.positional == other.positional
                and self.by_keyword == other.by_keyword)
