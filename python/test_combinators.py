from unittest.mock import Mock, call

import pytest

from combinators import (compose, iterate, parallel_combine, spread_combine,
                         restrict_arity, get_arity, Arity)


def test_compose():
    f = restrict_arity(Mock(), 1)
    g = restrict_arity(Mock(), 3)
    h = compose(f, g)

    result = h(1, 2, 3)

    g.assert_called_once_with(1, 2, 3)
    f.assert_called_once_with(g.return_value)
    assert result == f.return_value


def test_compose_f_must_must_take_at_least_one_argument():
    with pytest.raises(TypeError, match="arguments"):
        compose(lambda: 0, Mock())


def test_compose_f_must_must_take_at_most_one_required_argument():
    with pytest.raises(TypeError, match="arguments"):
        compose(lambda x, y: 0, Mock())


def test_composition_arity_equals_arity_of_g():
    f = restrict_arity(Mock(), 1)
    h = compose(f, lambda x, y: 0)
    assert get_arity(h) == Arity(2)


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
    with pytest.raises(TypeError, match="arguments"):
        parallel_combine(lambda x, y, z: 0, Mock(), Mock())


def test_parallel_combine_f_and_g_must_have_same_arity():
    with pytest.raises(AssertionError):
        parallel_combine(lambda x, y: 0, lambda a: 0, lambda a, b: 0)


def test_parallel_combine_arity_equals_arity_of_f_and_g():
    h = restrict_arity(Mock(), 2)
    p = parallel_combine(h, lambda x, y, z: 0, lambda x, y, z: 0)
    assert get_arity(p) == Arity(3)


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
    assert get_arity(c) == Arity(5)


def test_spread_combine_f_and_g_cant_both_be_vararg():
    h, f, g = Mock(), Mock(), Mock()
    restrict_arity(f, Arity(2, 3))
    restrict_arity(g, Arity(2, 3))
    with pytest.raises(AssertionError):
        spread_combine(h, f, g)


def test_spread_combine_with_g_vararg():
    h, f, g = Mock(), Mock(), Mock()
    restrict_arity(f, 2)
    restrict_arity(g, Arity(0, False))
    c = spread_combine(h, f, g)

    result = c(1, 2, 3, 4, 5)

    f.assert_called_once_with(1, 2)
    g.assert_called_once_with(3, 4, 5)
    h.assert_called_once_with(f.return_value, g.return_value)
    assert result == h.return_value
    assert get_arity(c) == Arity(2, False)


def test_spread_combine_with_f_vararg():
    h, f, g = Mock(), Mock(), Mock()
    restrict_arity(f, Arity(1, 2))
    restrict_arity(g, 3)
    c = spread_combine(h, f, g)

    result = c(1, 2, 3, 4, 5)

    f.assert_called_once_with(1, 2)
    g.assert_called_once_with(3, 4, 5)
    h.assert_called_once_with(f.return_value, g.return_value)
    assert result == h.return_value
    assert get_arity(c) == Arity(4, 5)


def test_get_arity_of_arbitrary_function():
    assert get_arity(lambda: None) == Arity(0)
    assert get_arity(lambda x: None) == Arity(1)
    assert get_arity(lambda x, y, z: None) == Arity(3)

    assert get_arity(lambda x, y, z=0: None) == Arity(2, 3)
    assert get_arity(lambda *args: None) == Arity(0, False)

    assert get_arity(lambda **kwargs: None) == Arity(0)


def test_compatibility_with_fixed_arity():
    arity = Arity(2)
    assert not arity.is_compatible(1)
    assert arity.is_compatible(2)
    assert not arity.is_compatible(3)


def test_compatibility_with_maximum_arity():
    arity = Arity(2, 3)
    assert not arity.is_compatible(1)
    assert arity.is_compatible(2)
    assert arity.is_compatible(3)
    assert not arity.is_compatible(4)


def test_compatibility_with_unbounded_arity():
    arity = Arity(2, False)
    assert not arity.is_compatible(1)
    assert arity.is_compatible(2)
    assert arity.is_compatible(3)
    assert arity.is_compatible(4)
