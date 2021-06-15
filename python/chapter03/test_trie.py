from chapter03.trie import Trie
import pytest


def test_add_edge_to_trie():
    a_trie = Trie()
    s = a_trie.add_edge(is_symbol)
    assert isinstance(s, Trie)


def test_intern_path():
    a_trie = Trie()
    s = a_trie.intern_path([is_symbol, is_symbol])
    assert isinstance(s, Trie)


def test_new_trie_has_no_value():
    assert not Trie().has_value()


def test_set_value_on_trie():
    a_trie = Trie()
    a_trie.value = 42
    assert a_trie.has_value()
    assert a_trie.value == 42


def test_set_path_value():
    a_trie = Trie()
    ss = a_trie.intern_path([is_symbol, is_symbol])
    a_trie.set_path_value([is_symbol, is_symbol], ["symbol", "symbol"])
    assert ss.value == ["symbol", "symbol"]


def test_intern_path_reuses_existing_nodes():
    a_trie = Trie()
    ss1 = a_trie.intern_path([is_symbol, is_symbol])
    ss2 = a_trie.intern_path([is_symbol, is_symbol])
    assert ss1 is ss2


def test_set_path_value_creates_path_if_necessary():
    a_trie = Trie()
    ss = a_trie.set_path_value([is_symbol, is_symbol], ["symbol", "symbol"])
    assert ss.value == ["symbol", "symbol"]


def test_match_single_feature_that_does_not_match():
    assert Trie().get_matching_tries(["a"]) == []


def test_match_single_feature_that_matches():
    a_trie = Trie()
    s = a_trie.add_edge(is_symbol)
    r = a_trie.get_matching_tries(["a"])
    assert len(r) == 1
    assert s in r


def test_match_sequence_of_features():
    a_trie = Trie()
    ss = a_trie.intern_path([is_symbol, is_symbol])
    r = a_trie.get_matching_tries(["b", "c"])
    assert len(r) == 1
    assert ss in r


def test_get_a_value():
    a_trie = Trie()
    a_trie.set_path_value([is_symbol, is_symbol], ["symbol", "symbol"])
    assert a_trie.get_a_value(["a", "b"]) == ["symbol", "symbol"]


def test_get_multiple_values():
    a_trie = Trie()
    a_trie.set_path_value([is_negative_number], "negative number")
    a_trie.set_path_value([is_even_number], "even number")
    r = a_trie.get_all_values([-4])
    assert len(r) == 2
    assert "negative number" in r
    assert "even number" in r


def is_symbol(obj):
    return isinstance(obj, str) and obj.isalnum()


def is_negative_number(obj):
    try:
        return obj < 0
    except Exception:
        return False


def is_even_number(obj):
    try:
        return obj % 2 == 0
    except Exception:
        return False
