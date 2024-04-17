:- begin_tests(arithmetic).

:- use_module('src/pro/terms/arithmetic.pro', [
    simplify/2,
    linear_expression/4,
    rewrite_without_arithmetic/4
]).
:- use_module('src/pro/terms/expressions.pro', [identifier/1]).

% swipl -g run_tests -t halt .\src\pro\terms\arithmetic_test.pro

test(simplify) :-
    simplify(luna_ref(["X", 1]), luna_ref(["X", 1])).
test(simplify) :-
    simplify(["+", 1, ["*", 2, ["-", 8, 5]]], 7).
test(simplify) :-
    simplify(["+", "X", 0], "X").
test(simplify) :-
    simplify(["+", "X", ["*", ["+", 55, ["*", 5, -11]], "Z"]], "X").
test(simplify) :-
    simplify(["-", "X", ["+", 34, ["*", 17, -2]]], "X").
test(simplify) :-
    simplify(["-", 0, "X"], ["-", "X"]).
test(simplify) :-
    simplify(["*", "X", 0], 0).
test(simplify) :-
    simplify(["*", "X", 1], "X").
test(simplify) :-
    simplify(["+", ["+", ["+", "X", 20], 10], 5], ["+", "X", 35]).
test(simplify) :-
    simplify(["+", ["-", "X", 5], 10], ["+", "X", 5]).
test(simplify) :-
    simplify(["-", ["+", "X", 10], 5], ["+", "X", 5]).
test(simplify) :-
    simplify(["-", ["+", 10, "X"], 5], ["+", "X", 5]).
test(simplify) :-
    simplify(["+", ["*", 0, "X"], ["-", 10, 5]], 5).
test(simplify) :-
    simplify(["+", ["-", "X", "X"], 1], 1).


test(linear_expression) :-
    linear_expression(Expr, "X", 1, 0),
    Expr = "X".
test(linear_expression) :-
    linear_expression(Expr, "X", 2, 0),
    Expr = ["*", 2, "X"].
test(linear_expression) :-
    linear_expression(Expr, "X", 1, 42),
    Expr = ["+", "X", 42].
test(linear_expression) :-
    linear_expression(Expr, "X", 2, 3),
    Expr = ["+", ["*", 2, "X"], 3].

test(linear_expression) :-
    linear_expression("X", Var, Step, Offset),
    [Var, Step, Offset] = ["X", 1, 0].
test(linear_expression) :-
    linear_expression(42, Var, Step, Offset),
    [Var, Step, Offset] = [_, 0, 42].
test(linear_expression) :-
    linear_expression(["+", "X", 42], Var, Step, Offset),
    [Var, Step, Offset] = ["X", 1, 42].
test(linear_expression) :-
    linear_expression(["*", "X", 3], Var, Step, Offset),
    [Var, Step, Offset] = ["X", 3, 0].
test(linear_expression) :-
    linear_expression(["+", ["*", 42, "X"], -5], Var, Step, Offset),
    [Var, Step, Offset] = ["X", 42, -5].
test(linear_expression) :-
    linear_expression(["-", ["+", ["*", 42, "X"], -5], 10], Var, Step, Offset),
    [Var, Step, Offset] = ["X", 42, -15].

test(rewrite_without_arithmetic, [throws(_)]) :-
    rewrite_without_arithmetic(luna_ref(["X", 0]), _{0:0}, _, _).
test(rewrite_without_arithmetic, [throws(_)]) :-
    rewrite_without_arithmetic(["&&", 1, luna_ref(["X", 0])], _{0:0}, _, _).
test(rewrite_without_arithmetic, [throws(_)]) :-
    rewrite_without_arithmetic(["&&", 1, luna_ref(["X", 0])], _{0:0}, _, _).
test(rewrite_without_arithmetic) :-
    rewrite_without_arithmetic(
        ["||", ["+", luna_ref(["X", 0]), 1], ["+", 1, luna_ref(["X", 0])]],
        _{0:0}, _,
        ["||", A, A]
    ),
    expressions:identifier(A).
test(rewrite_without_arithmetic) :-
    rewrite_without_arithmetic(0, _{0:0}, _, 0).
test(rewrite_without_arithmetic) :-
    rewrite_without_arithmetic("X", _{0:0}, _, "X").
test(rewrite_without_arithmetic) :-
    rewrite_without_arithmetic(["||", "X", 1], _{0:0}, _, ["||", "X", 1]).
test(rewrite_without_arithmetic) :-
    rewrite_without_arithmetic(["*", "X", 1], _{0:0}, _, A),
    expressions:identifier(A).
test(rewrite_without_arithmetic) :-
    rewrite_without_arithmetic(["&&", ["+", "X", 1], ["-", ["+", 42, "X"], 41]], _{0:0}, _, ["&&", A, A]),
    expressions:identifier(A).
test(rewrite_without_arithmetic) :-
    rewrite_without_arithmetic(["&&", ["+", "X", 1], "Y"], _{0:0}, _, ["&&", A, B]),
    expressions:identifier(A),
    expressions:identifier(B),
    A \== B.

:- end_tests(arithmetic).
