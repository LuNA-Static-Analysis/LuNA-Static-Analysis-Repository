:- module(execution_sequence, [
    item/5,
    for/2,
    if/1,
    exec/2,
    conditions/2,
    to_chars/2,
    to_call_stack/2,
    walk/2,
    main_root_ctx/1
]).

:- use_module('src/pro/call_stack.pro', [from_ids/2, to_string/2]).
:- use_module('src/pro/ja.pro').
:- use_module('src/pro/aliases.pro').


to_call_stack(Es, Cs) :-
    maplist(nth0(0), Es, StatementIds),
    call_stack:from_ids(StatementIds, Cs).

to_chars(Es, Chars) :-
    to_call_stack(Es, Cs),
    call_stack:to_string(Cs, Str),
    string_chars(Str, Chars).

item([StatementId, Type, Aliases, Conditions|_], StatementId, Type, Aliases, Conditions) :- true.

exec([_, exec, _, _, Callee], Callee) :- true.
for([_, for, _, _, VarEncoded], VarEncoded) :- true.
if([_, if|_]) :- true.

conditions([], []) :- true.
conditions([Item|_], Conditions) :-
    item(Item, _, _, _, Conditions),
    !.

% extern sub
%! walk(+Body:list, +Aliases:dict, +CallStack:list, +ExecutionSequenceAcc:list, -ExecutionSequence:list) is nondet
walk(Body, Aliases, CallStack, ExecutionSequenceAcc, ExecutionSequence) :-
    aliases:extend_from_body(Aliases, Body, CallStack, CurrentAliases),
    member(StatementId, Body),

    ja:exec(StatementId, Callee, _),
    ja:subroutine(_, Callee, extern),

    conditions(ExecutionSequenceAcc, Conditions),

    NewExecutionSequenceAcc = [[StatementId, exec, CurrentAliases, Conditions, Callee] | ExecutionSequenceAcc],
    ExecutionSequence = NewExecutionSequenceAcc.

% struct sub
%! walk(+Body:list, +Aliases:dict, +CallStack:list, +ExecutionSequenceAcc:list, -ExecutionSequence:list) is nondet
walk(Body, Aliases, CallStack, ExecutionSequenceAcc,  ExecutionSequence) :-
    aliases:extend_from_body(Aliases, Body, CallStack, CurrentAliases),
    member(StatementId, Body),
    InnerCallStack = [StatementId|CallStack],

    ja:exec(StatementId, Callee, ActualArgs),
    ja:subroutine(CalleeSubId, Callee, struct),
    ja:body(CalleeSubId, CalleeBody),
    ja:arg_names(Callee, FormalArgNames),

    aliases:resolve_in_all(CurrentAliases, ActualArgs, TrueActualArgs),
    utility:zip_to_dict(FormalArgNames, TrueActualArgs, 'aliases', TrueActualArgsByName),

    conditions(ExecutionSequenceAcc, Conditions),

    NewExecutionSequenceAcc = [[StatementId, exec, CurrentAliases, Conditions, Callee] | ExecutionSequenceAcc],

    walk(CalleeBody, TrueActualArgsByName, InnerCallStack, NewExecutionSequenceAcc, ExecutionSequence).

% if
%! walk(+Body:list, +Aliases:dict, +CallStack:list, +EsAcc:list, -Es:list) is nondet
walk(Body, Aliases, CallStack, EsAcc, Es) :-
    aliases:extend_from_body(Aliases, Body, CallStack, CurrentAliases),
    member(StatementId, Body),
    InnerCallStack = [StatementId|CallStack],

    ja:if(StatementId, LocalCondition), ja:body(StatementId, IfBody),

    aliases:resolve(CurrentAliases, LocalCondition, TrueCondition),
    conditions(EsAcc, Conditions),

    NewEsAcc = [[StatementId, if, CurrentAliases, [TrueCondition|Conditions]] | EsAcc],
    walk(IfBody, CurrentAliases, InnerCallStack, NewEsAcc, Es).

% for
%! walk(+Body:list, +Aliases:dict, +CallStack:list, +ExecutionSequenceAcc:list, -ExecutionSequence:list) is nondet
walk(Body, Aliases, CallStack, ExecutionSequenceAcc, ExecutionSequence) :-
    aliases:extend_from_body(Aliases, Body, CallStack, CurrentAliases),
    member(StatementId, Body),
    InnerCallStack = [StatementId|CallStack],

    ja:for(StatementId, Var, _, _), ja:body(StatementId, ForBody),

    aliases:extend(CurrentAliases, [Var], InnerCallStack, InnerAliases),
    aliases:resolve(InnerAliases, Var, VarEncoded),

    conditions(ExecutionSequenceAcc, Conditions),

    NewExecutionSequenceAcc = [
        [StatementId, for, CurrentAliases, Conditions, VarEncoded]
        | ExecutionSequenceAcc
    ],

    walk(ForBody, InnerAliases, InnerCallStack, NewExecutionSequenceAcc, ExecutionSequence).

main_root_ctx(RootCtx) :-
    ja:subroutine(MainId, "main", _), ja:body(MainId, MainBody),
    MainCallStack = [MainId],
    dict_create(MainAliases, aliases, []),
    RootCtx = [MainBody, MainAliases, MainCallStack].

walk(RootCtx, Es) :-
    [Body, Aliases, CallStack] = RootCtx,
    walk(Body, Aliases, CallStack, [], Es).
