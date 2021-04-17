from unittest.mock import Mock, call

import pytest

from combinators import (compose, iterate, parallel_combine, spread_combine,
                         restrict_arity, get_arity)


def test_compose():
    f = restrict_arity(Mock(), 1)
    g = restrict_arity(Mock(), 3)
    h = compose(f, g)

    result = h(1, 2, 3)

    g.assert_called_once_with(1, 2, 3)
    f.assert_called_once_with(g.return_value)
    assert result == f.return_value


def test_compose_f_must_be_unary():
    with pytest.raises(TypeError, match="arity"):
        compose(lambda x, y: 0, Mock())


def test_composition_arity_equals_arity_of_g():
    f = restrict_arity(Mock(), 1)
    h = compose(f, lambda x, y: 0)
    assert get_arity(h) == 2


def test_composition_checks_arity():
    f = restrict_arity(Mock(), 1)
    h = compose(f, lambda x, y: 0)
    with pytest.raises(TypeError, match="argument"):
        compose(f, h)()


def test_iterate_zero_never_calls_function():
    func = Mock()
    iterate(0)(func)(42)
    func.assert_not_called()


def test_iterate_zero_is_identity():
    assert iterate(0)(Mock())(42) == 42


def test_iterate_once_calls_the_function():
    func = restrict_arity(Mock(), 1)
    result = iterate(1)(func)(42)
    func.assert_called_once_with(42)
    assert result == func.return_value


def test_iterate_calls_function_n_times():
    func = restrict_arity(Mock(), 1)
    iterate(3)(func)("input")
    assert func.call_count == 3


def test_iterate_feeds_output_back_to_function():
    func = restrict_arity(Mock(), 1)
    iterate(3)(func)("input")
    func.assert_has_calls([call("input"),
                           call(func.return_value),
                           call(func.return_value)])


def test_parallel_combine():
    h = restrict_arity(Mock(), 2)
    f = restrict_arity(Mock(), 3)
    g = restrict_arity(Mock(), 3)
    c = parallel_combine(h, f, g)

    result = c(1, 2, 3)

    f.assert_called_once_with(1, 2, 3)
    g.assert_called_once_with(1, 2, 3)
    h.assert_called_once_with(f.return_value, g.return_value)
    assert result == h.return_value


def test_parallel_combine_h_must_be_binary():
    with pytest.raises(TypeError, match="arity"):
        parallel_combine(lambda x, y, z: 0, Mock(), Mock())


def test_parallel_combine_f_and_g_must_have_same_arity():
    with pytest.raises(TypeError, match="arity"):
        parallel_combine(lambda x, y: 0, lambda a: 0, lambda a, b: 0)


def test_parallel_combine_arity_equals_arity_of_f_and_g():
    h = restrict_arity(Mock(), 2)
    p = parallel_combine(h, lambda x, y, z: 0, lambda x, y, z: 0)
    assert get_arity(p) == 3


def test_parallel_combination_checks_arity():
    p = parallel_combine(lambda x, y: (x, y), lambda x: x, lambda x: x)
    with pytest.raises(TypeError, match="argument"):
        p()


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
