:- module(
    arithmetic,
    [
        indexed_name/5,
        rewrite_without_arithmetic/4,
        simplify/2,
        linear_expression/4
    ]
).

:- use_module('src/pro/terms/expressions.pro', [identifier/1, expression_string/2]).
:- use_module('src/pro/terms/ref.pro', [expressions_equivalent/2]).
:- use_module('src/pro/dict_utils.pro').

% consult("src/pro/terms/arithmetic.pro").

indexed_name(Expression, Name, Index, Step, Offset) :-
    Expression = luna_ref([Name, IndexExpression]),
    linear_expression(IndexExpression, Index, Step, Offset).

arithmetic_operator(Op) :-
    member(Op, [
        "+", "-",
        "*", "//"
    ]),
    !.

arithmetic_expression([Op|_]) :- arithmetic_operator(Op).
%arithmetic_expression(Expr) :- number(Expr).

arith_alias(Key, Alias) :-
    var(Alias), number(Key),
    atomics_to_string(["Arith_", Key], "", Alias),
    !.
arith_alias(Key, Alias) :-
    string(Alias), var(Key),
    split_string(Alias, "_", "", [_, KeyStr]),
    number_codes(Key, KeyStr),
    !.

% Example:    (X < (0 + Y[7 - 2])) || (Y[5] <= (X + 0))
%          == (X < Y[5]) || (Y[5] <= X)
% rewrite_without_arithmetic(["||", ["<", "X", ["+", 0, luna_ref(["Y", ["-", 7, 2]])]], ["<=", luna_ref(["Y", 5]), ["+", 0, "X"]]], _{}, ArithOut, ExprOut).

%! rewrite_without_arithmetic(+ExprIn, +ArithExprsIn:dict, -ArithExprsOut:dict, -ExprOut) is det
rewrite_without_arithmetic(Expr, Aliases, Aliases, Expr) :- number(Expr), !.
rewrite_without_arithmetic(Expr, Aliases, Aliases, Expr) :- expressions:identifier(Expr), !.
rewrite_without_arithmetic(ExprIn, Aliases, Aliases, ExprOut) :-
    arithmetic_expression(ExprIn),
    simplify(ExprIn, ExprSimple),
    ExprIn \= ExprSimple,
    rewrite_without_arithmetic(ExprSimple, Aliases, Aliases, ExprOut),
    !.

rewrite_without_arithmetic(ExprIn, ArithExprsIn, ArithExprsOut, ExprOut) :-
    arithmetic_expression(ExprIn),
    dict_find_key(ref:expressions_equivalent, ArithExprsIn, ExprIn, ExprKey),
    ArithExprsOut = ArithExprsIn,
    arith_alias(ExprKey, ExprOut),
    !.
rewrite_without_arithmetic(ExprIn, ArithExprsIn, ArithExprsOut, ExprOut) :-
    arithmetic_expression(ExprIn),
    dict_max_key(ArithExprsIn, MaxKey),
    ExprKey is MaxKey + 1,
    put_dict([ExprKey=ExprIn], ArithExprsIn, ArithExprsOut),
    arith_alias(ExprKey, ExprOut),
    !.
rewrite_without_arithmetic(ExprIn, ArithExprsIn, ArithExprsOut, ExprOut) :-
    ExprIn = [Op, Arg],
    rewrite_without_arithmetic(Arg, ArithExprsIn, ArithExprsOut, ArgWithoutArith),
    ExprOut = [Op, ArgWithoutArith],
    !.
rewrite_without_arithmetic(ExprIn, ArithExprsIn, ArithExprsOut, ExprOut) :-
    ExprIn = [Op, Lhs, Rhs],
    rewrite_without_arithmetic(Lhs, ArithExprsIn, ArithExprsLhs, LhsWithoutArith),
    rewrite_without_arithmetic(Rhs, ArithExprsLhs, ArithExprsOut, RhsWithoutArith),
    ExprOut = [Op, LhsWithoutArith, RhsWithoutArith],
    !.
rewrite_without_arithmetic(ExprIn, _, _, _) :-
    term_string(ExprIn, ExprInStr),
    atomics_to_string(["rewrite_without_arithmetic/4: Unexpected expression format: ", ExprInStr], "", Message),
    throw(Message).

%! simplify(+ExprIn, -ExprOut) is det
simplify(ExprIn, ExprOut) :-
    identifier(ExprIn),
    ExprOut = ExprIn,
    !.
simplify(ExprIn, ExprOut) :-
    number(ExprIn),
    ExprOut = ExprIn,
    !.
simplify(ExprIn, ExprOut) :-
    ExprIn = [_, A, B],
    number(A),
    number(B),
    expressions:expression_string(ExprIn, ExprStr),
    term_string(ExprTerm, ExprStr),
    ExprOut is ExprTerm,
    !.
simplify(ExprIn, ExprOut) :-
    ExprIn = [_, A],
    number(A),
    expressions:expression_string(ExprIn, ExprStr),
    term_string(ExprTerm, ExprStr),
    ExprOut is ExprTerm,
    !.
simplify(ExprIn, ExprOut) :-
    ExprIn = [Op, A, B],
    simplify(A, SimplifiedA),
    simplify(B, SimplifiedB),
    NewExpression = [Op, SimplifiedA, SimplifiedB],
    NewExpression \= ExprIn,
    simplify(NewExpression, ExprOut),
    !.
simplify(ExprIn, ExprOut) :-
    ExprIn = [Op, A],
    simplify(A, SimplifiedA),
    NewExpression = [Op, SimplifiedA],
    NewExpression \= ExprIn,
    simplify(NewExpression, ExprOut),
    !.
simplify(ExprIn, ExprOut) :-
    ExprIn = ["+", A, B],
    (
        A is 0
        ->  ExprOut = B
        ;
        B is 0
        ->  ExprOut = A
    ),
    !.
simplify(ExprIn, ExprOut) :-
    ExprIn = ["-", A, B],
    (
        B is 0
        ->  ExprOut = A
        ;
        A is 0
        ->  ExprOut = ["-", B]
    ),
    !.
simplify(ExprIn, ExprOut) :-
    ExprIn = ["*", A, B],
    (
        A is 1
        ->  ExprOut = B
        ;
        B is 1
        ->  ExprOut = A
    ),
    !.
simplify(ExprIn, ExprOut) :-
    ExprIn = ["*", A, B],
    ((A is 0; B is 0)
    ->  ExprOut = 0
    ),
    !.
simplify(ExprIn, ExprOut) :-
    ExprIn = [_, A, B],
    (
        identifier(A), number(B)
        ; number(A), identifier(B)
    ),
    ExprOut = ExprIn,
    !.
simplify(ExprIn, ExprOut) :-
    (
        ExprIn = ["+", ["+", A, X1], X2]
        ; ExprIn = ["+", ["+", X1, A], X2]
        ; ExprIn = ["+", X2, ["+", A, X1]]
        ; ExprIn = ["+", X2, ["+", X1, A]]
    ),
    simplify(X1, SimplifiedX1), number(SimplifiedX1),
    simplify(X2, SimplifiedX2), number(SimplifiedX2),
    Offset is SimplifiedX1 + SimplifiedX2,
    simplify(A, SimplifiedA),
    simplify(["+", SimplifiedA, Offset], ExprOut),
    !.
simplify(ExprIn, ExprOut) :-
    ExprIn = ["-", Lhs, Rhs],
    simplify(Lhs, LhsSimple),
    simplify(Rhs, RhsSimple),
    LhsSimple = RhsSimple,
    ExprOut = 0,
    !.
simplify(ExprIn, ExprOut) :-
    (
        ExprIn = ["-", ["+", A, X1], X2]
        ; ExprIn = ["-", ["+", X1, A], X2]
        ; ExprIn = ["+", ["-", A, X2], X1]
    ),
    simplify(X1, SimplifiedX1), number(SimplifiedX1),
    simplify(X2, SimplifiedX2), number(SimplifiedX2),
    Offset is SimplifiedX1 - SimplifiedX2,
    simplify(A, SimplifiedA),
    simplify(["+", SimplifiedA, Offset], ExprOut),
    !.
simplify(A, A) :- true.


%! linear_expression(?Expression, ?Var, ?Step, ?Offset) is det
linear_expression(Expression, Var, Step, Offset) :-
    var(Expression),
    RawExpression = ["+", ["*", Step, Var], Offset],
    simplify(RawExpression, Expression),
    !.
linear_expression(Expression, "", 0, ExpressionSimple) :-
    simplify(Expression, ExpressionSimple),
    number(ExpressionSimple),
    !.
linear_expression(Expression, Var, 1, 0) :-
    \+ var(Expression),
    simplify(Expression, Var),
    identifier(Var),
    !.
linear_expression(RawExpression, Var, Step, 0) :-
    \+ var(RawExpression),
    simplify(RawExpression, SimplifiedExpression),
    (
        SimplifiedExpression = ["*", Step, Var], number(Step)
        ; SimplifiedExpression = ["*", Var, Step], number(Step)
    ),
    identifier(Var),
    !.
linear_expression(RawExpression, Var, Step, Offset) :-
    \+ var(RawExpression),
    simplify(RawExpression, SimplifiedExpression),
    (
        SimplifiedExpression = ["+", Offset, InnerExpression], number(Offset)
        ; SimplifiedExpression = ["+", InnerExpression, Offset], number(Offset)
        ; SimplifiedExpression = ["-", InnerExpression, NegOffset] , number(NegOffset), Offset is -NegOffset
    ),
    linear_expression(InnerExpression, Var, Step, 0),
    !.
