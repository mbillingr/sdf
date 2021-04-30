from unittest.mock import Mock

from values import Values, apply


def test_normal_return_value_passed_on_as_single_argument():
    def foo(a): return a

    def bar(): return 42

    assert apply(foo, bar()) == 42


def test_return_multiple_values_and_pass_to_another_function():
    def foo(a, b, c): return [a, b, c]

    def bar(): return Values(1, 2, c=3)

    assert apply(foo, bar()) == [1, 2, 3]


def test_repr_of_values_is():
    values = Values(1, 'two', c='cee')
    assert repr(values) == "Values(1, 'two', c='cee')"


def test_apply_passes_value_without_apply_method_to_callable():
    value = list()
    proc = Mock()
    apply(proc, value)

    proc.assert_called_once_with(value)


def test_apply_returns_result_of_callable():
    proc = Mock()
    result = apply(proc, 0)

    assert result == proc.return_value


def test_apply_invokes_apply_method_of_argument_with_proc():
    proc = Mock()

    class values: __apply__ = Mock()

    apply(proc, values)

    values.__apply__.assert_called_once_with(proc)


def test_apply_returns_result_of_arguments_apply_method():
    proc = Mock()

    class values: __apply__ = Mock()

    result = apply(proc, values)

    assert result == values.__apply__.return_value


def test_values_application_returns_result_of_proc():
    proc = Mock()
    result = Values().__apply__(proc)

    proc.assert_called_once()
    assert result == proc.return_value


def test_values_application_passes_positional_arguments_to_proc():
    proc = Mock()
    values = Values(1, 2, 3)

    values.__apply__(proc)

    proc.assert_called_once_with(1, 2, 3)


def test_can_combine_values_sequentially():
    first = Values(1, 2, 3)
    second = Values('x', 'y', 'z')

    proc = Mock()
    first.append(second).__apply__(proc)
    proc.assert_called_once_with(1, 2, 3, 'x', 'y', 'z')


def test_values_application_passes_keyword_arguments_to_proc():
    proc = Mock()
    values = Values(a=1, b=2, c=3)

    values.__apply__(proc)

    proc.assert_called_once_with(a=1, b=2, c=3)


def test_can_combine_keywordvalues_sequentially():
    first = Values(a=1, b=2, c=3)
    second = Values(x=1, y=2, z=3)

    proc = Mock()
    first.append(second).__apply__(proc)
    proc.assert_called_once_with(a=1, b=2, c=3, x=1, y=2, z=3)


def test_values_with_values_argument_returns_same_values():
    val1 = Values(1, 2)
    val2 = Values(val1)
    assert apply(make_list, val1) == apply(make_list, val2)


def make_list(*args):
    return list(args)
