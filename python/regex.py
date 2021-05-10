"""Regular Expression Combinator Library

This library builds a domain-specific language embedded in Python to build
regular expressions.
In contrast to the Scheme implementation from the book which supports Posix
BRE, it produces regex strings compatible with Python's `re` module.
"""


def dot():
    """Match any one character"""
    return "."


def bol():
    """Match the beginning of the line"""
    return "^"


def eol():
    """Match the end of the line"""
    return "$"


def quote(string):
    """Match a literal string"""
    return seq(''.join(_escape(ch) for ch in string))


def _escape(char):
    if char in CHARS_NEEDING_ESCAPE:
        return f"\\{char}"
    return char


CHARS_NEEDING_ESCAPE = r".\^$*+?|()[]"


def seq(*exprs):
    """Match a sequence of regular expressions"""
    return "(" + ''.join(exprs) + ")"


def opt(expr):
    """Match this pattern optionally"""
    return alt(expr, "")


def alt(*exprs):
    """Match the first matching regular expression in sequence"""
    return seq("|".join(exprs))


def repeat(min, max, expr):
    """Match repeated expression"""
    if max is None:
        return seq(f"{expr}{{{min},}}")
    elif max <= min:
        return seq(f"{expr}{{{min}}}")
    else:
        return seq(f"{expr}{{{min},{max}}}")


def char_from(string):
    """Match any one character in string"""
    if not string:
        return seq()
    elif len(string) == 1:
        return quote(string)
    elif set(string) == {'-', '^'}:
        raise NotImplementedError()

    return "[" + _escape_bracketed_contents(string) + "]"


def char_not_from(string):
    """Match any one character not in string"""
    if not string:
        return dot()
    return "[^" + _escape_bracketed_contents(string) + "]"


def _escape_bracketed_contents(members):
    def optional(ch):
        return ch if ch in members else ""

    return (optional(']')
            + ''.join(m for m in members if m not in ']^-')
            + optional('^')
            + optional('-'))
