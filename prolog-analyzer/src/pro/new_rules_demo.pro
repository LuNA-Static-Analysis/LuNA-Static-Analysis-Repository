:- use_module('src/pro/ja.pro', [
    parent/2,
    output_parameter/3,
    arity/2,
    if/2,
    exec/3
]).
:- use_module('src/pro/encode.pro', [decode_names_in_all/2]).
:- use_module('src/pro/ref.pro', [expressions_equivalent/2]).
:- use_module('src/pro/terms/logic.pro', [negation/2]).
:- use_module(library(clpb)).
:- use_module('src/pro/expressions.pro', [repr_chars/2]).
:- use_module('src/pro/utility.pro').

:- consult("src/pro/find-usages.pro").
:- consult("generated/out.pro").

% consult("src/pro/new_rules_demo.pro").

if_can_be_replaced_with_ternary(Name, Callee, File, CondPos, InitPosLine, CondNeg, InitNegLine) :-
    ja:if(IfPosId, CondPos), ja:parent(IfPosId, ParentId),
    ja:if(IfNegId, CondNeg), ja:parent(IfNegId, ParentId),
    IfPosId < IfNegId,

    ja:exec(InitPosId, Callee, _), ja:parent(InitPosId, IfPosId),
    ja:output_parameter(InitPosId, NamePos, _),

    ja:exec(InitNegId, Callee, _), ja:parent(InitNegId, IfNegId),
    ja:output_parameter(InitNegId, NameNeg, _),

    ja:arity(Callee, CalleeArity), CalleeArity > 1,
    ref:expressions_equivalent(NamePos, NameNeg),
    logic:negation(CondPos, CondNeg),

    Name = NamePos,
    ja:position(InitPosId, File, InitPosLine),
    ja:position(InitNegId, File, InitNegLine).

if_can_be_replaced_with_ternary() :-
    if_can_be_replaced_with_ternary(Name, Callee, File, CondPos, InitPosLine, CondNeg, InitNegLine),
    expressions:repr_chars(Name, NameChars),
    string_chars(File, FileChars),
    expressions:repr_chars(CondPos, CondPosChars),
    expressions:repr_chars(CondNeg, CondNegChars),
    writef:writef(
        "%s is initialized in exclusive if statements\nIf %s is true in %s:%d\nIf %s is true in %s:%d\n",
        [NameChars, CondPosChars, FileChars, InitPosLine, CondNegChars, FileChars, InitNegLine]
    ),
    writef:writef("Consider replacing with single call to %t and using ternary operator for other parameters.", [Callee]).

list_subset(_, []).
list_subset([X|XSS], [X|XS]) :- list_subset(XSS, XS).
list_subset([_|XSS], [X|XS]) :- list_subset(XSS, [X|XS]).

permutatation(L1, L2) :-
    msort(L1, Sorted),
    msort(L2, Sorted).

possible_wrong_arguments_order(SubEs, FormalArgNames, TrueActualArgs) :-
    main_root_ctx(RootCtx), walk(RootCtx, Es),

    append(_, SubEs, Es), SubEs = [[StmtId, Callee, Aliases|_]|_],

    ja:exec(StmtId, Callee, ActualArgs),
    resolve_aliases_in_all(Aliases, ActualArgs, EncodedTrueActualArgs),
    encode:decode_names_in_all(EncodedTrueActualArgs, TrueActualArgs),

    ja:arg_names(Callee, FormalArgNames),

    maplist(utility:pair, FormalArgNames, TrueActualArgs, ArgPairs),
    once((
        list_subset(ArgPairs, SomeArgPairs),
        maplist(utility:pair, ParamNames, ActualParams, SomeArgPairs),
        ParamNames \= ActualParams,
        permutatation(ParamNames, ActualParams)
    )).

possible_wrong_arguments_order() :-
    possible_wrong_arguments_order(SubEs, ParamNames, ActualParams),
    execution_sequence_chars(SubEs, SubEsChars),
    atomics_to_string(ParamNames, ", ", ParamNamesStr), string_chars(ParamNamesStr, ParamNamesChars),
    atomics_to_string(ActualParams, ", ", ActualParamsStr), string_chars(ActualParamsStr, ActualParamsChars),
    writef:writef("Possibly wrong order of arguments:\nIn %s\n", [SubEsChars]),
    writef:writef("Formal params declared as (%s)\n", [ParamNamesChars]),
    writef:writef("But actual parameters are (%s)\n", [ActualParamsChars]).