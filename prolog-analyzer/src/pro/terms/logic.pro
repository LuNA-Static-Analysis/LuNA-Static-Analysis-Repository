:- module(logic, [
    tautology/2,
    equvalent/2,
    negation/2,
    satisfiable/1,
    simultaneous/2,
    implies/2,
    bool_used_as_number/3,
    condition_normalize/2,
    condition_formula/4,
    flatten/3,
    and/2
]).

:- use_module(library(clpb), [taut/2]).
:- reexport([library(clpb)]).

:- use_module('src/pro/terms/expressions.pro', [
    identifier/1,
    expression_compare/3
]).
:- use_module('src/pro/terms/arithmetic.pro', [rewrite_without_arithmetic/4]).
:- use_module('src/pro/terms/ref.pro', [rewrite_without_refs/4, expressions_equivalent/2]).
:- use_module('src/pro/dict_utils.pro').

% consult("src/pro/terms/logic.pro").

logic_operator(Op) :- member(Op, ["&&", "||", "!"]).
comparison_operator(Op) :- member(Op, ["<", "<=", ">", ">=", "==", "!="]).

evaluates_to_bool([Op, _, _]) :- logic_operator(Op), !.
evaluates_to_bool([Op, _, _]) :- comparison_operator(Op), !.

% bool_used_as_number(["+", ["+", ["&&", 1, 2], 42], [">=", 1, 2]], Expr, ArgIdx).

%! bool_used_as_number(+Expr, -BadExpr, -ArgIndex:int) is nondet

bool_used_as_number([_, Arg], BadExpr, ArgIndex) :- bool_used_as_number(Arg, BadExpr, ArgIndex).
bool_used_as_number([_, Lhs, _], BadExpr, ArgIndex) :- bool_used_as_number(Lhs, BadExpr, ArgIndex).
bool_used_as_number([_, _, Rhs], BadExpr, ArgIndex) :- bool_used_as_number(Rhs, BadExpr, ArgIndex).

bool_used_as_number(luna_ref([_|Indices]), BadExpr, ArgIndex) :-
    member(Index, Indices),
    bool_used_as_number(Index, BadExpr, ArgIndex).

bool_used_as_number(Expr, Expr, ArgIndex) :-
    Expr = luna_ref([_|Indices]),
    nth0(ArgIndex, Indices, IndexExpr),
    evaluates_to_bool(IndexExpr).

bool_used_as_number(Expr, Expr, 0) :-
    Expr = [Op, Arg],
    \+ logic_operator(Op),
    evaluates_to_bool(Arg).

bool_used_as_number(Expr, Expr, 0) :-
    Expr = [Op, Lhs, _],
    \+ logic_operator(Op),
    evaluates_to_bool(Lhs).

bool_used_as_number(Expr, Expr, 1) :-
    Expr = [Op, _, Rhs],
    \+ logic_operator(Op),
    evaluates_to_bool(Rhs).


operator_negation(Op, NegOp) :-
    atom_string(OpAtom, Op),
    get_dict(
        OpAtom,
        _{
            '>': "<=",
            '>=': "<",
            '<': ">=",
            '<=': ">",
            '!=': "=="
        },
        NegOp
    ),
    !.
operator_negation(Op, _) :-
    atomics_to_string(["operator_negation/2: no negation for ", Op, " is registered"], "", Msg),
    throw(Msg).


condition_normalize(0, 0) :- true, !.
condition_normalize(N, 1) :- number(N), !.

% TODO maybe use expressions_compare to evaluate constant expressions?
condition_normalize([Op, Lhs, Rhs], Normalized) :-
    member(Op, ["<", ">", "!="]),
    ref:expressions_equivalent(Lhs, Rhs),
    Normalized = 0,
    !.
condition_normalize([Op, Lhs, Rhs], Normalized) :-
    member(Op, [">=", "<=", "=="]),
    ref:expressions_equivalent(Lhs, Rhs),
    Normalized = 1,
    !.
condition_normalize([">", Lhs, Rhs], Normalized) :-
    condition_normalize(["<", Rhs, Lhs], Normalized),
    !.
condition_normalize(["==", Lhs, Rhs], Normalized) :-
    condition_normalize(["!", ["||", ["<", Lhs, Rhs], [">", Lhs, Rhs]]], Normalized),
    !.
condition_normalize([Op, Lhs, Rhs], Normalized) :-
    member(Op, ["<=", ">=", "!="]),
    operator_negation(Op, NegOp),
    condition_normalize([NegOp, Lhs, Rhs], Inner),
    Normalized = ["!", Inner],
    !.
condition_normalize([Op, Lhs, Rhs], Normalized) :-
    logic_operator(Op),
    condition_normalize(Lhs, LhsNorm),
    condition_normalize(Rhs, RhsNorm),
    Normalized = [Op, LhsNorm, RhsNorm],
    !.
condition_normalize([Op, Arg], Normalized) :-
    logic_operator(Op),
    condition_normalize(Arg, ArgNorm),
    Normalized = [Op, ArgNorm],
    !.
condition_normalize(Normalized, Normalized) :- true.

cond_make_alias(Key, Alias) :-
    atomics_to_string(["Cond_", Key], "", Alias).

condition_formula(0, A, A, 0) :- true, !.
condition_formula(1, A, A, 1) :- true, !.

condition_formula(Cond, AliasesIn, AliasesOut, Formula) :-
    (
        Cond = [Op, _, _], comparison_operator(Op)
        ; expressions:identifier(Cond)
    ),
    (
        get_dict(CondKey, AliasesIn, Cond)
        ;   (
            Cond = [Op, Lhs, Rhs],
            get_dict(CondKey, AliasesIn, [Op, Lhs1, Rhs1]),
            ref:expressions_equivalent(Lhs, Lhs1),
            ref:expressions_equivalent(Rhs, Rhs1)
        )
        ;   (
            Cond = [Op, Arg],
            get_dict(CondKey, AliasesIn, [Op, Arg1]),
            ref:expressions_equivalent(Arg, Arg1)
        )
    ),
    AliasesOut = AliasesIn,
    cond_make_alias(CondKey, Formula),
    !.
condition_formula(Cond, AliasesIn, AliasesOut, Formula) :-
    (
        Cond = [Op, _, _], comparison_operator(Op)
        ; expressions:identifier(Cond)
    ),
    dict_max_key(AliasesIn, MaxKey),
    CondKey is MaxKey + 1,
    put_dict([CondKey=Cond], AliasesIn, AliasesOut),
    cond_make_alias(CondKey, Formula),
    !.
condition_formula([Op, Arg], AliasesIn, AliasesOut, Formula) :-
    logic_operator(Op),
    condition_formula(Arg, AliasesIn, AliasesOut, ArgFormula),
    Formula = [Op, ArgFormula],
    !.
condition_formula([Op, Lhs, Rhs], AliasesIn, AliasesOut, Formula) :-
    logic_operator(Op),
    condition_formula(Lhs, AliasesIn, AliasesOutLhs, LhsFormula),
    condition_formula(Rhs, AliasesOutLhs, AliasesOut, RhsFormula),
    Formula = [Op, LhsFormula, RhsFormula],
    !.
condition_formula(Cond, _, _, _) :-
    term_string(Cond, CondStr),
    atomics_to_string(["condition_formula/4: Unexpected expression format: ", CondStr], "", Message),
    throw(Message).

operator_clpb(Op, ClpbOperator) :-
    atom_string(OpAtom, Op),
    get_dict(
        OpAtom,
        _{
            '&&': "*",
            '||': "+",
            '!': "~",
            '==': "=:="
        },
        ClpbOperator
    ),
    !.

formula_to_clpb_str(0, "0") :- true, !.
formula_to_clpb_str(1, "1") :- true, !.
formula_to_clpb_str(Formula, ClpbTermStr) :-
    identifier(Formula),
    ClpbTermStr = Formula,
    !.
formula_to_clpb_str([Op, Arg], ClpbTermStr) :-
    operator_clpb(Op, ClpbOp),
    formula_to_clpb_str(Arg, ArgStr),
    atomics_to_string([ClpbOp, "(", ArgStr, ")"], "", ClpbTermStr),
    !.
formula_to_clpb_str([Op, Lhs, Rhs], ClpbTermStr) :-
    operator_clpb(Op, ClpbOp),
    formula_to_clpb_str(Lhs, LhsStr),
    formula_to_clpb_str(Rhs, RhsStr),
    atomics_to_string([ClpbOp, "(", LhsStr, ", ", RhsStr, ")"], "", ClpbTermStr),
    !.
formula_to_clpb_str(Expr, _) :-
    term_string(Expr, ExprInStr),
    atomics_to_string(["formula_to_clpb_str/2: Unexpected expression format: ", ExprInStr], "", Message),
    throw(Message).

formula_to_clpb(Formula, ClpbTerm) :-
    formula_to_clpb_str(Formula, ClpbTermStr),
    term_string(ClpbTerm, ClpbTermStr).


% Expr = ["&&", ["&&", [">=", luna_ref(["LUNA_x", "LUNA_N"]), ["+", "LUNA_y", 1]], ["<", luna_ref(["LUNA_x", ["+", "LUNA_N", ["*", "LUNA_y", 0]]]), ["+", 1, "LUNA_y"]]], [">", "LUNA_y", 5]], condition_clpb(Expr, CondWithoutRefs, CondWithoutArith, CondNormalized, Formula, TermStr, Term).
condition_clpb(Cond, CondWithoutRefs, CondWithoutArith, CondNormalized, Formula, ClpbTermStr, ClpbTerm) :-
    ref:rewrite_without_refs(Cond, _{0:0}, _, CondWithoutRefs),
%    writef("CondWithoutRefs=%t\n", [CondWithoutRefs]),
    arithmetic:rewrite_without_arithmetic(CondWithoutRefs, _{0:0}, _, CondWithoutArith),
%    writef("CondWithoutArith=%t\n", [CondWithoutArith]),
    condition_normalize(CondWithoutArith, CondNormalized),
%    writef("CondNormalized=%t\n", [CondNormalized]),
    condition_formula(CondNormalized, _{0:0}, _, Formula),
%    writef("Formula=%t\n", [Formula]),
    formula_to_clpb_str(Formula, ClpbTermStr),
    term_string(ClpbTerm, ClpbTermStr).


tautology(Cond, true) :-
    \+ bool_used_as_number(Cond, _, _),
    condition_clpb(Cond, _, _, _, _, _, ClpbTerm),
    clpb:taut(ClpbTerm, 1),
    !.
tautology(Cond, false) :-
    \+ bool_used_as_number(Cond, _, _),
    condition_clpb(Cond, _, _, _, _, _, ClpbTerm),
    clpb:taut(ClpbTerm, 0),
    !.

equvalent(Cond1, Cond2) :-
    tautology(["||", ["&&", Cond1, Cond2], ["&&", ["!", Cond1], ["!", Cond2]]], true).

negation(Cond1, Cond2) :-
    equvalent(Cond1, ["!", Cond2]).


satisfiable(Cond) :-
    condition_clpb(Cond, _, _, _, _, _, ClpbTerm),
%    writef("%t\n", [ClpbTerm]),
    clpb:sat(ClpbTerm),
    !.

flatten([Op|Args], Op, Result) :-
    length(Args, ArgsCount),
    length(Ops, ArgsCount),
    maplist(=(Op), Ops),
    maplist(flatten, Args, Ops, FlatArgs),
    append(FlatArgs, Result),
    !.
flatten(Cond, _, [Cond]) :- true, !.

and([], 1) :- true, !.
and([Cond], Cond) :- true, !.
and([Cond|Rest], Conjunction) :-
    and(Rest, RestConjunction),
    Conjunction = ["&&", Cond, RestConjunction].

% TODO rename to maybe_simultaneous/not_exclusive
simultaneous(Cond1, Cond2) :-
    Cond = ["&&", Cond1, Cond2],
    satisfiable(Cond),
    !.

implies(Cond1, Cond2) :-
    Cond = ["||", ["!", Cond1], Cond2],
    tautology(Cond, true),
    !.
