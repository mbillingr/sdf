from unittest.mock import Mock
from chapter03.multimethods import MultiMethod, match_args, SimpleDispatchStore, TypeTrie, TypePredicate, \
    TrieDispatchStore
import pytest


class NullDispatchStore:
    def get_handler(self, *_):
        return None


class TrivialDispatchStore:
    def __init__(self, handler):
        self.handler = handler

    def get_handler(self, *_):
        return self.handler


def make_trivial_dispatch_store(handler):
    return lambda: TrivialDispatchStore(handler)


def test_multimethod_returns_handler_result():
    handler = Mock()
    foo = MultiMethod("foo", dispatch_store=make_trivial_dispatch_store(handler))

    result = foo()

    assert result == handler.return_value


def test_multimethod_default_handler():
    default_handler = Mock()
    foo = MultiMethod("foo", default_handler=default_handler, dispatch_store=NullDispatchStore)

    foo()

    default_handler.assert_called_once()


def test_multimethod_forwards_args_to_handler():
    handler = Mock()
    foo = MultiMethod("foo", dispatch_store=make_trivial_dispatch_store(handler))

    foo(1, 2)

    handler.assert_called_with(1, 2)


def test_simple_dispatch_store_selects_handler_based_on_all_arguments():
    dispatch_store = SimpleDispatchStore()

    int_float_handler = Mock()
    dispatch_store.add_handler(match_args(int, float), int_float_handler)

    float_int_handler = Mock()
    dispatch_store.add_handler(match_args(float, int), float_int_handler)

    assert dispatch_store.get_handler(1.0, 2) == float_int_handler
    assert dispatch_store.get_handler(3, 4.0) == int_float_handler


def test_simple_dispatch_store_is_strict_with_types():
    dispatch_store = SimpleDispatchStore()
    assert not dispatch_store.match_signature((int, int), (1.0, 2.0))


def test_multimethod_matches_on_subtypes():
    dispatch_store = SimpleDispatchStore()
    assert dispatch_store.match_signature((object, object), (None, []))  # EVERY type inherits object


def test_typetrie_dispatches_on_types():
    root = TypeTrie()
    leaf = root.add_edge(int)

    int_result = root.get_matching_tries([42])
    assert int_result == [leaf]

    float_result = root.get_matching_tries([3.1])
    assert float_result == []


def test_typetrie_works_with_paths():
    root = TypeTrie()
    first_node = root.add_edge(int)
    leaf = root.intern_path([int, float, int])

    assert root.get_matching_tries([1, 2.0, 3]) == [leaf]

    assert root.get_matching_tries([1]) == [first_node]

    assert root.get_matching_tries([3.1]) == []


def test_type_predicates_compare_on_type():
    a = TypePredicate(int)
    b = TypePredicate(int)
    c = TypePredicate(float)

    assert a == int
    assert a == b
    assert a != c

    assert hash(a) == hash(int)
    assert hash(a) == hash(b)
    assert hash(a) != hash(c)


def test_trie_dispatch_store_selects_handler_based_on_all_arguments():
    dispatch_store = TrieDispatchStore()

    int_float_handler = Mock()
    dispatch_store.add_handler(match_args(int, float), int_float_handler)

    float_int_handler = Mock()
    dispatch_store.add_handler(match_args(float, int), float_int_handler)

    assert dispatch_store.get_handler(1.0, 2) == float_int_handler
    assert dispatch_store.get_handler(3, 4.0) == int_float_handler
