:- module(
    ref,
    [
        expressions_equivalent/2,
        rewrite_without_refs/4
    ]
).

:- use_module('src/pro/terms/expressions.pro', [identifier/1, expression_compare/3]).
:- use_module('src/pro/dict_utils.pro').

% consult("src/pro/terms/ref.pro").

% Handle expressions containing refs (i.e. x[i + 1])

expressions_equivalent(Lhs, Rhs) :-
    dict_create(RefsEmpty, _, []),
    rewrite_without_refs(Lhs, RefsEmpty, RefsLhs, LhsWithoutRefs),
    rewrite_without_refs(Rhs, RefsLhs, _, RhsWithoutRefs),
    \+ expression_compare("#\\=", LhsWithoutRefs, RhsWithoutRefs).

refs_equivalent(luna_ref([BaseName|Indices1]), luna_ref([BaseName|Indices2])) :-
    maplist(expressions_equivalent, Indices1, Indices2).

ref_make_alias(Key, Alias) :-
    atomics_to_string(["Ref_", Key], "", Alias).

% Про систему переписывания термов: https://core.ac.uk/download/pdf/38468694.pdf

% Examples using top-level refs:
% E1 = luna_ref(["LUNA_x", "LUNA_i"])
% E2 = luna_ref(["LUNA_x", [+, [-, "LUNA_i", 2], 2]])
% E3 = luna_ref(["LUNA_x", "LUNA_i", "LUNA_j"])
% E1 = luna_ref(["LUNA_x", "LUNA_i"]), E2 = luna_ref(["LUNA_x", [+, [-, "LUNA_i", 2], 2]]), E3 = luna_ref(["LUNA_x", "LUNA_i", "LUNA_j"]), In = [+, E1, [-, [*, "LUNA_y", E2], E3]], expressions:rewrite_without_refs(In, _{}, Refs, Out).

% Examples using refs as indices:
% rewrite_without_refs(["+", 1, ["*", luna_ref(["X", luna_ref(["N", 0])]), luna_ref(["X", luna_ref(["N", ["-", 42, 42]])])]], _{}, RefsOut, ExprOut).
% rewrite_without_refs(["+", 1, ["*", luna_ref(["X", luna_ref(["N", 0])]), luna_ref(["X", luna_ref(["N", ["-", 42, 43]])])]], _{}, RefsOut, ExprOut).
% rewrite_without_refs(["+", 1, ["*", luna_ref(["X", luna_ref(["N", 0])]), luna_ref(["X", luna_ref(["M", ["-", 42, 42]])])]], _{}, RefsOut, ExprOut).
% rewrite_without_refs(["+", 1, ["*", luna_ref(["X", luna_ref(["N", "K"])]), luna_ref(["X", luna_ref(["N", "K"])])]], _{}, RefsOut, ExprOut).
rewrite_without_refs(ExprIn, RefsIn, RefsOut, ExprOut) :-
    number(ExprIn),
    RefsOut = RefsIn,
    ExprOut = ExprIn,
    !.
rewrite_without_refs(ExprIn, RefsIn, RefsOut, ExprOut) :-
    identifier(ExprIn),
    RefsOut = RefsIn,
    ExprOut = ExprIn,
    !.
rewrite_without_refs([Op, Arg], RefsIn, RefsOut, ExprOut) :-
    rewrite_without_refs(Arg, RefsIn, RefsOut, ArgOut),
    ExprOut = [Op, ArgOut],
    !.
rewrite_without_refs([Op, Lhs, Rhs], RefsIn, RefsOut, ExprOut) :-
    rewrite_without_refs(Lhs, RefsIn, RefsOutLhs, LhsOut),
    rewrite_without_refs(Rhs, RefsOutLhs, RefsOut, RhsOut),
    ExprOut = [Op, LhsOut, RhsOut],
    !.
rewrite_without_refs(ExprIn, RefsIn, RefsOut, ExprOut) :-
    ExprIn = luna_ref(_),
    dict_find_key(ref:refs_equivalent, RefsIn, ExprIn, ExprKey),
    RefsOut = RefsIn,
    ref_make_alias(ExprKey, ExprOut),
    !.
rewrite_without_refs(ExprIn, RefsIn, RefsOut, ExprOut) :-
    ExprIn = luna_ref(_),
    dict_max_key(RefsIn, MaxKey),
    ExprKey is MaxKey +1,
    put_dict([ExprKey=ExprIn], RefsIn, RefsOut),
    ref_make_alias(ExprKey, ExprOut),
    !.
rewrite_without_refs(ExprIn, _, _, _) :-
    term_string(ExprIn, ExprInStr),
    atomics_to_string(["rewrite_without_refs/4: Unexpected expression format: ", ExprInStr], "", Message),
    throw(Message).
