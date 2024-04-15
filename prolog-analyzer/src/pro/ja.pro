:- module(ja, [
    position/3,
    statement/2,
    subroutine/3,
    if/2,
    for/4,
    exec/3,
    dfs/2,
    body/2,
    parent/2,
    parent_body/2,
    arity/2,
    input_parameter/2,
    output_parameter/3
]).

:- use_module('src/pro/utility.pro').

position(StatementId, File, Line) :-
    luna_statement(StatementId, File, Line, _, _, _).
position(StatementId, File, Line) :-
    luna_subroutine(StatementId, File, Line, _, _, _, _).

statement(StatementId, Type) :-
    luna_statement(StatementId, _, _, Type, _, _).
subroutine(Id, Name, Type) :-
    luna_subroutine(Id, _, _, Name, Type, _, _).

if(StatementId, Condition) :-
    luna_statement(StatementId, _, _, if, [Condition|_], _).
for(StatementId, Var, First, Last) :-
    luna_statement(StatementId, _, _, for, [Var, First, Last|_], _).
exec(StatementId, Callee, Args) :-
    luna_statement(StatementId, _, _, exec, [Callee, Args], _).
dfs(StatementId, Names) :-
    luna_statement(StatementId, _, _, dfs, [Names], _).

body(StatementId, Body) :-
    luna_statement(StatementId, _, _, for, [_, _, _, Body], _).
body(StatementId, Body) :-
    luna_statement(StatementId, _, _, if, [_, Body], _).
body(SubroutineId, Body) :-
    luna_subroutine(SubroutineId, _, _, _, _, _, Body).

parent(StatementId, ParentId) :-
    body(ParentId, ParentBody),
    member(StatementId, ParentBody).

parent_body(StatementId, Body) :-
    body(_, Body),
    member(StatementId, Body).
parent_body(StatementId, Body) :-
    luna_subroutine(_, _, _, _, _, _, Body),
    member(StatementId, Body).

arity(SubName, Arity) :-
    luna_subroutine(_, _, _, SubName, _, Args, _),
    length(Args, Arity).

arg_names(SubName, ArgNames) :-
    luna_subroutine(_, _, _, SubName, struct, Args, _),
    maplist(nth0(1), Args, ArgNames).

input_parameter(StatementId, LocalName) :-
    luna_statement(StatementId, _, _, _, _, Requests),
    member(LocalName, Requests).

output_parameter(StatementId, LocalName, Index) :-
    luna_statement(StatementId, _, _, exec, [Callee, LocalArgs], _),
    luna_subroutine(_, _, _, Callee, extern, ArgTypes, _),
    maplist(utility:pair, LocalArgs, ArgTypes, Args),
    nth0(Index, Args, [LocalName, [name]]).