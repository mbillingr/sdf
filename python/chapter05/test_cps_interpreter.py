import pytest

from chapter05.common.primitive_types import symbol
from chapter05.common.procedures import is_compound_procedure
from cps_interpreter import a, eval_str, make_initial_environment


def test_application():
    assert eval_str("(+ 1 2)", make_initial_environment()) == 3


def test_self_evaluating_number():
    assert eval_str("42", make_initial_environment()) == 42


def test_quotation():
    assert eval_str("'(1 2 3)") == (1, 2, 3)


def test_variable():
    assert eval_str("+") is not None
    with pytest.raises(NameError, match="foobar"):
        eval_str("foobar")


def test_lambda():
    assert is_compound_procedure(eval_str("(lambda (x) x)"))


def test_if():
    assert eval_str("(if #t 2 3)") == 2
    assert eval_str("(if #f 2 3)") == 3


def test_sequence():
    assert eval_str("(begin 1 2 3)") == 3
    with pytest.raises(SyntaxError):
        eval_str("(begin)")


def test_definition():
    assert eval_str("(define x 42)") == symbol("x")
    assert eval_str("(define x 42) x") == 42
    assert is_compound_procedure(eval_str("(define (x) 0) x"))


def test_assignment():
    assert eval_str("(define x 0) (set! x 1)") == symbol("ok")
    assert eval_str("(define x 0) (set! x 1) x") == 1


def test_cond():
    assert eval_str("(cond (else 0))") == 0
    assert eval_str("(cond (#f 1) (#t 2) (else 3))") == 2


def test_let():
    assert eval_str("(let ((x 1) (y 2)) (+ x y))") == 3


def test_maybeset_behaves_like_set():
    assert eval_str("(define x 0) (maybe-set! x 1) x") == 1


def test_amb():
    assert eval_str("(amb 1 2 3)") == 1
    assert eval_str("(amb)") == "no value"


def test_amb_preserves_side_effects():
    eval_str(
        "(begin (define x '()) " 
        "       (set! x (cons (amb 1 2 3) x)) " 
        "       (amb))"
    )
    assert eval_str("x") == (3, 2, 1)


def test_argument_evaluation_order():
    assert eval_str(
        "(define x '())"
        "(define (stored value) (set! x (cons value x)) value)"
        "(define (ternary a b c) 'ignore)"
        "(ternary (stored 1) (stored 2) (stored 3))"
        "x"
    ) == (3, 2, 1)


@pytest.mark.skip
def test_bench_fib():
    from time import time

    start = time()
    res = eval_str(
        "(define (fib n) "
        "   (cond ((< n 2) 1) "
        "         (else (+ (fib (- n 1)) "
        "                  (fib (- n 2))))))"
        "(fib 25)"
    )
    stop = time()
    print("TIME:", stop - start)
    assert res == 121393
