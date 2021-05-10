import re
import regex as r


# integration tests

def test_match_any_character():
    assert re.fullmatch(r.dot(), 'x')
    assert not re.fullmatch(r.dot(), 'xx')


def test_match_literal_strings():
    assert re.match(r.quote("foo"), 'foo')
    assert re.match(r.quote("foo"), 'foobar')
    assert not re.match(r.quote("(f..)"), '(foo)')
    assert re.match(r.quote("(f..)"), '(f..)')


def test_match_start_and_end():
    pattern = r.seq(r.bol(), r.quote("foo"), r.eol())
    assert re.fullmatch(pattern, 'foo')
    assert not re.fullmatch(pattern, 'foobar')


def test_match_alternative():
    pattern = r.alt(r.quote("cat"), r.quote("dog"))
    assert not re.fullmatch(pattern, 'foo')
    assert re.fullmatch(pattern, 'cat')
    assert re.fullmatch(pattern, 'dog')
    assert not re.fullmatch(pattern, 'catdog')


def test_repetition():
    pattern = r.repeat(2, 3, r.alt(r.quote("cat"), r.quote("dog")))
    assert not re.fullmatch(pattern, 'cat')
    assert re.fullmatch(pattern, 'catdog')
    assert re.fullmatch(pattern, 'dogcatcat')
    assert not re.fullmatch(pattern, 'dogcatcatdog')


def test_unlimited_repetition():
    pattern = r.repeat(2, None, r.alt(r.quote("cat"), r.quote("dog")))
    assert not re.fullmatch(pattern, 'cat')
    assert re.fullmatch(pattern, 'catdog')
    assert re.fullmatch(pattern, 'dogcatcat')
    assert re.fullmatch(pattern, 'dogcatcatdogdogcat')


def test_char_from():
    pattern = r.char_from("x]")
    assert re.fullmatch(pattern, "x")
    assert re.fullmatch(pattern, "]")
    assert not re.fullmatch(pattern, "foo")


def test_char_not_from():
    assert not re.fullmatch(r.char_not_from("y"), "y")
    assert re.fullmatch(r.char_not_from("y"), "x")
    assert re.fullmatch(r.char_not_from("y"), "X")
    assert re.fullmatch(r.char_not_from("y"), "+")
    assert re.fullmatch(r.char_not_from(""), "_")
    assert re.fullmatch(r.char_not_from("^x-z]"), "_")
    assert re.fullmatch(r.char_not_from("^x-z]"), "y")
    assert not re.fullmatch(r.char_not_from("^x-z]"), "x")
    assert not re.fullmatch(r.char_not_from("^x-z]"), "z")
    assert not re.fullmatch(r.char_not_from("^x-z]"), "^")
    assert not re.fullmatch(r.char_not_from("^x-z]"), "-")
    assert not re.fullmatch(r.char_not_from("^x-z]"), "]")


def test_number():
    int_pattern = r.seq(r.opt(r.alt(r.quote("+"), r.quote("-"))),
                        r.repeat(1, None, r.char_from("0123456789")))
    num_pattern = r.seq(int_pattern,
                        r.opt(r.seq(r.quote("."),
                                    r.opt(int_pattern))))
    number = re.compile(num_pattern)

    assert not number.fullmatch("foo")
    assert not number.fullmatch("+")
    assert number.fullmatch("0")
    assert number.fullmatch("-42")
    assert number.fullmatch("3.")
    assert number.fullmatch("3.14")


# unit tests


def test_primitives():
    assert r.dot() == '.'
    assert r.bol() == '^'
    assert r.eol() == '$'


def test_empty_sequence():
    assert r.seq() == "()"


def test_singleton_sequence():
    assert r.seq('.') == "(.)"


def test_arbitrary_sequence():
    assert r.seq('a', 'b', 'c') == "(abc)"


def test_quoted_empty_string():
    assert r.quote("") == "()"


def test_quoted_simple_string():
    assert r.quote("abc") == "(abc)"


def test_quoted_string_escapes_certain_characters():
    assert r.quote("a.b") == r"(a\.b)"
    assert r.quote(r"a\b") == r"(a\\b)"
    assert r.quote("a^b") == r"(a\^b)"
    assert r.quote("a$b") == r"(a\$b)"
    assert r.quote("a*b") == r"(a\*b)"
    assert r.quote("a+b") == r"(a\+b)"
    assert r.quote("a?b") == r"(a\?b)"
    assert r.quote("a|b") == r"(a\|b)"
    assert r.quote("a(b") == r"(a\(b)"
    assert r.quote("a)b") == r"(a\)b)"
    assert r.quote("a[b") == r"(a\[b)"
    assert r.quote("a]b") == r"(a\]b)"


def test_empty_alternative():
    assert r.alt() == "()"


def test_single_alternative():
    assert r.alt("foo") == "(foo)"


def test_multiple_alternatives():
    assert r.alt("foo", "bar", "baz") == "(foo|bar|baz)"


def test_repetition_min_same_as_max():
    assert r.repeat(3, 3, "foo") == "(foo{3})"


def test_repetition_no_max():
    assert r.repeat(2, None, r.quote("foo")) == "((foo){2,})"


def test_repetition_min_max():
    assert r.repeat(2, 4, r.quote("foo")) == "((foo){2,4})"


def test_repetition_max_less_than_min_is_ignored():
    assert r.repeat(2, 1, "foo") == "(foo{2})"


def test_char_from_empty_string():
    assert r.char_from("") == "()"


def test_char_from_single_char():
    assert r.char_from("X") == "(X)"
    assert r.char_from(".") == r"(\.)"


def test_char_from_simple_string():
    assert r.char_from("abcABC") == "[abcABC]"


def test_char_from_string_needing_escape():
    assert r.char_from("^x-z]") == r"[]xz^-]"


def test_char_not_from_empty_string():
    assert r.char_not_from("") == "."


def test_char_not_from_simple_string():
    assert r.char_not_from("xyz") == "[^xyz]"


def test_char_not_from_string_needing_escape():
    assert r.char_not_from("^x-z]") == r"[^]xz^-]"


def test_opt():
    assert r.opt("foo") == "(foo|)"
