from unittest.mock import Mock, call

import pytest

from combinators import (compose, iterate, parallel_combine, spread_combine,
                         restrict_arity, get_arity)


def test_compose():
    f = Mock()
    g = Mock()
    h = compose(f, g)

    result = h(1, 2, 3)

    g.assert_called_once_with(1, 2, 3)
    f.assert_called_once_with(g.return_value)
    assert result == f.return_value


def test_iterate_zero_never_calls_function():
    func = Mock()
    iterate(0)(func)(42)
    func.assert_not_called()


def test_iterate_zero_is_identity():
    assert iterate(0)(Mock())(42) == 42


def test_iterate_once_calls_the_function():
    func = Mock()
    result = iterate(1)(func)(42)
    func.assert_called_once_with(42)
    assert result == func.return_value


def test_iterate_calls_function_n_times():
    func = Mock()
    iterate(3)(func)("input")
    assert func.call_count == 3


def test_iterate_feeds_output_back_to_function():
    func = Mock()
    iterate(3)(func)("input")
    func.assert_has_calls([call("input"),
                           call(func.return_value),
                           call(func.return_value)])


def test_parallel_combine():
    h, f, g = Mock(), Mock(), Mock()
    c = parallel_combine(h, f, g)

    result = c(1, 2, 3)

    f.assert_called_once_with(1, 2, 3)
    g.assert_called_once_with(1, 2, 3)
    h.assert_called_once_with(f.return_value, g.return_value)
    assert result == h.return_value


def test_spread_combine():
    h, f, g = Mock(), Mock(), Mock()
    restrict_arity(f, 2)
    restrict_arity(g, 3)
    c = spread_combine(h, f, g)

    result = c(1, 2, 3, 4, 5)

    f.assert_called_once_with(1, 2)
    g.assert_called_once_with(3, 4, 5)
    h.assert_called_once_with(f.return_value, g.return_value)
    assert result == h.return_value
    assert get_arity(c) == 5


def test_get_arity_of_arbitrary_function():
    assert get_arity(lambda: None) == 0
    assert get_arity(lambda x: None) == 1
    assert get_arity(lambda x, y, z: None) == 3

    with pytest.raises(TypeError, match="Variable arity"):
        assert get_arity(lambda x, y, z=0: None)

    with pytest.raises(TypeError, match="Variable arity"):
        assert get_arity(lambda *args: None)

    assert get_arity(lambda **kwargs: None) == 0
