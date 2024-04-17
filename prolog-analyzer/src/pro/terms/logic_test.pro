:- begin_tests(logic).

:- use_module('src/pro/terms/logic.pro', [
    condition_normalize/2,
    condition_formula/4,
    bool_used_as_number/3,
    tautology/2,
    negation/2,
    equvalent/2,
    simultaneous/2,
    implies/2
]).

% swipl -g run_tests -t halt .\src\pro\terms\logic_test.pro

test(tautology) :-
    tautology(1, true).
test(tautology) :-
    tautology(42, true).
test(tautology) :-
    tautology(["+", 7, -4], true).
test(tautology) :-
    tautology(0, false).
test(tautology) :-
    tautology(["&&", "X", 0], false).
test(tautology) :-
    tautology(["&&", 0, "X"], false).
test(tautology) :-
    tautology(["||", "X", 1], true).
test(tautology, [fail]) :-
    tautology("X", _).
test(tautology, [fail]) :-
    tautology(["||", "X", "Y"], _).

test(tautology, [fail]) :-
    tautology(["<", ["<", "Y", "Z"], 2], _).

test(tautology) :-
    tautology(["==", "X", "X"], true).
test(tautology) :-
    tautology(["<=", ["+", 2, "X"], ["-", "X", ["+", ["*", "Z", 0], -2]]], true).
test(tautology) :-
    tautology(["||", [">", "X", "Y"], ["<=", "X", "Y"]], true).
test(tautology) :-
    tautology(["&&", [">", "X", "Y"], ["<=", "X", "Y"]], false).
test(tautology) :-
    tautology(["||", ["||", [">", "X", 0], [">", 0, "X"]], ["==", "X", 0]], true).

test(bool_used_as_number) :-
    Expr = ["<", "X", ["<", "Y", "Z"]],
    findall(
        [BadExpr, ArgIndex],
        bool_used_as_number(Expr, BadExpr, ArgIndex),
        [[Expr, 1]]
    ).
test(bool_used_as_number) :-
    Expr = ["<", ["<", "Y", "Z"], "X"],
    findall(
        [BadExpr, ArgIndex],
        bool_used_as_number(Expr, BadExpr, ArgIndex),
        [[Expr, 0]]
    ).
test(bool_used_as_number) :-
    Expr = ["==", ["==", "Y", "Z"], "X"],
    findall(
        [BadExpr, ArgIndex],
        bool_used_as_number(Expr, BadExpr, ArgIndex),
        [[Expr, 0]]
    ).
test(bool_used_as_number) :-
    Expr = ["+", ["==", "Y", "Z"], ["<", "X", "Y"]],
    findall(
        [BadExpr, ArgIndex],
        bool_used_as_number(Expr, BadExpr, ArgIndex),
        [[Expr, 0], [Expr, 1]]
    ).
test(bool_used_as_number) :-
    ChainedCmp1 = ["==", ["==", "X", "Y"], "Z"],
    ChainedCmp2 = ["<", "Z", ["<", "X", "Y"]],
    Expr = ["||", ChainedCmp1, ChainedCmp2],
    findall(
        [BadExpr, ArgIndex],
        bool_used_as_number(Expr, BadExpr, ArgIndex),
        [[ChainedCmp1, 0], [ChainedCmp2, 1]]
    ).
test(bool_used_as_number) :-
    ChainedCmp = [">", "X", [">", "Y", 0]],
    Expr = luna_ref(["X", ChainedCmp]),
    findall(
        [BadExpr, ArgIndex],
        bool_used_as_number(Expr, BadExpr, ArgIndex),
        [[ChainedCmp, 1], [Expr, 0]]
    ).
test(bool_used_as_number) :-
    Expr = luna_ref(["X", ["==", "Count", 0]]),
    findall(
        [BadExpr, ArgIndex],
        bool_used_as_number(Expr, BadExpr, ArgIndex),
        [[Expr, 0]]
    ).
test(bool_used_as_number) :-
    Expr = luna_ref(["X", "Index1", ["==", "Count", 0]]),
    findall(
        [BadExpr, ArgIndex],
        bool_used_as_number(Expr, BadExpr, ArgIndex),
        [[Expr, 1]]
    ).
test(bool_used_as_number) :-
    SubRef = luna_ref(["BOUNDS", 0, ["<", "A", "B"], 2]),
    Expr = ["*", 2, ["==", luna_ref(["X", "Index1", ["+", SubRef, 42]]), "Z"]],
    findall(
        [BadExpr, ArgIndex],
        bool_used_as_number(Expr, BadExpr, ArgIndex),
        [[SubRef, 1], [Expr, 1]]
    ).

test(equvalent, [fail]) :-
    equvalent(["<", "X", ["<", "Y", "Z"]], ["<", "X", ["<", "Y", "Z"]]).
test(equvalent) :-
    equvalent(1, ["||", ["<=", "X", "Y"], [">", "X", "Y"]]).
test(equvalent) :-
    equvalent(["==", "X", "Y"], ["==", "Y", "X"]).
test(equvalent) :-
    equvalent(["==", "X", "Y"], ["&&", ["<=", "X", "Y"], [">=", "X", "Y"]]).
test(equvalent) :-
    equvalent([">", "X", 0], ["<", 0, "X"]).
test(equvalent) :-
    equvalent([">", "X", ["*", ["-", -1, -1], "A"]], ["<", ["-", 5, 5], "X"]).
test(equvalent) :-
    equvalent([">", "X", 0], ["<", ["-", 5, 5], "X"]).
test(equvalent) :-
    equvalent([">", "X", 0], ["<", ["*", 0, "N"], "X"]).
test(equvalent) :-
    equvalent([">", "X", "N"], ["<", ["-", "N", ["+", 7, ["*", -1, 7]]], "X"]).

test(negation) :-
    negation(1, 0).
test(negation) :-
    negation(["==", "X", "Y"], ["!=", "Y", "X"]).
test(negation) :-
    negation(["==", "X", "Y"], ["||", [">", "Y", "X"], [">", "X", "Y"]]).
test(negation) :-
    negation(["&&", "X", "Y"], ["||", ["!", "Y"], ["!", "X"]]).
test(negation) :-
    negation([">", "N", 1], ["<=", "N", 1]).
test(negation, [fail]) :-
    negation([">","Var1__LUNA_N",7], ["<=","Var1__LUNA_N",15]).

test(simultaneous, [fail]) :-
    simultaneous("X", ["!", "X"]).
test(simultaneous, [fail]) :-
    simultaneous(["<", "X", "Y"], ["<=", "Y", "X"]).
test(simultaneous, [fail]) :-
    simultaneous(["==", "X", "Y"], ["||", [">", "Y", "X"], [">", "X", "Y"]]).
test(simultaneous, [fail]) :-
    simultaneous(["||", ["&&", "X", "Z"], "Y"], ["&&", ["!", "X"], ["!", "Y"]]).

test(simultaneous) :-
    simultaneous("X", "Y").
test(simultaneous) :-
    simultaneous("X", 1).
test(simultaneous) :-
    simultaneous(["||", "X", "Y"], ["!", "X"]).
test(simultaneous) :-
    simultaneous(["||", "X", "Y"], ["&&", ["!", "X"], "Z"]).
test(simultaneous) :-
    simultaneous([">", "X", 0], ["<", "Y", 0]).

test(implies, [fail]) :-
    implies("X", "Y").
test(implies, [fail]) :-
    implies([">", "X", 0], [">", "Y", 0]).
test(implies, [fail]) :-
    implies(["&&", "X", [">", luna_ref(["Z", 0]), 0]], ["&&", "X", ["&&", "Y", [">", luna_ref(["Z", 0]), 0]]]).

test(implies) :-
    implies(0, 0).
test(implies) :-
    implies(0, 1).
test(implies) :-
    implies(42, 13).
test(implies) :-
    implies([">", "N", 0], [">", "N", 0]).
test(implies) :-
    implies(["&&", [">", "N", 0], ["<=", "K", "Y"]], [">", "N", 0]).
test(implies) :-
    implies(["&&", [">", "N", 0], ["<=", "K", "Y"]], ["<=", "K", "Y"]).
test(implies) :-
    implies(["&&", [">", "N", 0], ["<=", "K", "Y"]], ["||", "ExtraCondtition", ["&&", [">", "N", 0], ["<=", "K", "Y"]]]).

:- end_tests(logic).