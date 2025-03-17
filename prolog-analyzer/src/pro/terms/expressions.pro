:- module(
    expressions,
    [
        identifier/1,
        repr/2,
        repr_chars/2,
        expression_identifier/2,
        expression_compare/3,
        expression_string/2,
        expression_dict/2
    ]).

:- use_module('src/pro/clpfd_utility.pro').

identifier(Expression) :- \+ var(Expression), string(Expression).

expression_identifier(Expression, Identifier) :- identifier(Expression), Identifier = Expression.
expression_identifier(Expression, Identifier) :- Expression = luna_ref([Identifier|_]).
expression_identifier(Expression, Identifier) :-
    Expression = luna_ref([_|Indices]),
    member(IndexExpression, Indices),
    expression_identifier(IndexExpression, Identifier).
expression_identifier(Expression, Identifier) :-
    Expression = [_|Operands],
    member(OperandExpression, Operands),
    expression_identifier(OperandExpression, Identifier).

repr(Expression, Expression) :-
    identifier(Expression),
    !.
repr(Expression, Repr) :-
    number(Expression),
    number_string(Expression, Repr),
    !.
repr([Op, Arg], Repr) :-
    repr(Arg, ArgRepr),
    atomics_to_string([Op, ArgRepr], "", Repr),
    !.
repr([Op, Lhs, Rhs], Repr) :-
    repr(Lhs, LshRepr),
    repr(Rhs, RhsRepr),
    atomics_to_string(["(", LshRepr, " ", Op, " ", RhsRepr, ")"], "", Repr),
    !.
repr(luna_ref(Items), Repr) :-
    maplist(repr, Items, [BaseNameRepr|IndicesReprs]),
    atomics_to_string([BaseNameRepr, "["], "", ReprHead),
    atomics_to_string(IndicesReprs, "][", ReprBody),
    atomics_to_string([ReprHead, ReprBody, "]"], "", Repr),
    !.
repr(Expression, _) :-
    term_string(Expression, ExpressionStr),
    string_concat("Unexpected expression format: ", ExpressionStr, Message),
    throw(Message).

repr_chars(Expr, Chars) :-
    repr(Expr, Str),
    string_chars(Str, Chars).

expression_string(Expression, Expression) :-
    string(Expression),
    !.
expression_string(Expression, S) :-
    number(Expression),
    number_string(Expression, S),
    !.
expression_string([BinOp, Lhs, Rhs], S) :-
    expression_string(Lhs, LhsStr),
    expression_string(Rhs, RhsStr),
    atomics_to_string(["(", LhsStr, " ", BinOp, " ", RhsStr, ")"], S),
    !.
expression_string([Op, Arg], S) :-
    expression_string(Arg, ArgStr),
    atomics_to_string(["(", Op, ArgStr, ")"], S),
    !.
expression_string(Expression, _) :-
    term_string(Expression, ExpressionStr),
    atomics_to_string(["expression_string/2: Unexpected expression format: ", ExpressionStr], "", Message),
    throw(Message).

% TODO make private, add an implementation in ref that would rewrite without refs first 
% Comparing expressions using CLP(FD)
expression_compare(Sign, Lhs, Rhs) :-
    expression_string(Lhs, LhsStr),
    expression_string(Rhs, RhsStr),
    clpfd_utility:build_term(Sign, [LhsStr, RhsStr], Cond),
    Cond.

expression_dict(Expression, Dict) :-
    Dict = expression{
        'type': "ref",
        'name': Expression
    },
    identifier(Expression),
    !.
expression_dict(Expression, Dict) :-
    Dict = expression{
        'type': "const",
        'value': Expression
    },
    number(Expression),
    !.
expression_dict(luna_ref([Name|Indices]), Dict) :-
    Dict = expression{
        'type': "ref",
        'name': Name,
        'indices': IndicesAsDicts
    },
    maplist(expression_dict, Indices, IndicesAsDicts),
    !.
expression_dict([Op|Args], Dict) :-
    Dict = expression{
        'type': Op,
        'args': ArgsAsDicts
    },
    maplist(expression_dict, Args, ArgsAsDicts),
    !.
expression_dict(Expression, _) :-
    term_string(Expression, ExpressionStr),
    atomics_to_string(["expression_dict/2: Unexpected expression format: ", ExpressionStr], "", Message),
    throw(Message).
