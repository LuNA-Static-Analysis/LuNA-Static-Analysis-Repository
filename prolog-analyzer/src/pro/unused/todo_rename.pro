:- use_module(library(clpfd)).
:- use_module(string_utility:'src/pro/string-utility.pro').

% luna_statement(Id, Pos, Type, Data)
:- multifile(luna_statement/4).




% luna_subroutine(Pos, Name, Type, Args, Body)
:- multifile(luna_subroutine/5).


pair(A, B, [A, B]) :- true.

zip(L1, L2, Result) :-
    maplist(pair, L1, L2, Result).

compare_expressions(ComparisonSignStr, LeftExpr, RightExpr) :-
    string_utility:concat3(LeftExpr, ComparisonSignStr, RightExpr, CondString),
    term_string(Cond, CondString),
    Cond.  % clpfd is required here

% TODO specific versions of compare_expressions, i.e. expr_less, expr_cannot_compare etc.

is_df_name(MaybeName) :-
    Prefix = "Var__",
    string_length(Prefix, PrefixLength),
    string(MaybeName),
    sub_string(MaybeName, 0, PrefixLength, _, Prefix).

referenced(Name, Expressions) :-
    member(Name, Expressions),
    is_df_name(Name).

referenced(Name, Expressions) :-
    member(Ref, Expressions),
    Ref = luna_ref([Name, _]).

referenced(Name, Body, StatementId) :-
    member(StatementId, Body),
    luna_statement(StatementId, _, exec, [Callee, CallArgs]),
    luna_subroutine(_, Callee, extern, _, _),
    referenced(Name, CallArgs).

referenced(Name, Body, StatementId) :-
    member(InnerId, Body),
    luna_statement(InnerId, _, for, [_, _, _, InnerBody]),
    referenced(Name, InnerBody, StatementId).

find(Name, StatementId) :-
    luna_subroutine(_, _, struct, _, Body),
    member(DfsId, Body),
    luna_statement(DfsId, _, dfs, [Names]),
    member(Name, Names),
    referenced(Name, Body, StatementId).

df_referenced_indexed(Name, Body, Index, Type) :-
    member(ExecId, Body),
    luna_statement(ExecId, _, exec, [Callee, Arguments]),
    luna_subroutine(_, Callee, extern, ArgumentTypes, _),
    zip(ArgumentTypes, Arguments, ArgumentsWithTypes),
    member([[Type], luna_ref([Name, Index])], ArgumentsWithTypes).


df_indexed(Name, Type, DeclaredIn, From, To) :-
    luna_subroutine(_, DeclaredIn, struct, _, Body),
    member(DfsId, Body),
    luna_statement(DfsId, _, dfs, [Dfs]),
    member(Name, Dfs),
    member(ForId, Body),
    luna_statement(ForId, _, for, [Counter, From, To, ForBody]),
    df_referenced_indexed(Name, ForBody, Counter, Type).
