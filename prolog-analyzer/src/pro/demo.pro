:- use_module(library(clpb)).

:- use_module('src/pro/terms/expressions.pro', [
    expression_identifier/2,
    repr/2,
    repr_chars/2
]).
:- use_module('src/pro/encode.pro', [decode_name/3, decode_names/2]).
:- use_module('src/pro/execution_sequence.pro', [
    main_root_ctx/1,
    walk/2,
    to_chars/2
]).
:- use_module('src/pro/call_stack.pro', [to_string/2]).

:- use_module('src/pro/checks/ranges.pro', [
    index_range_missmatch_error_json/1,
    index_range_not_initialized_error_json/1,
    index_range_overlap_error_json/1,

    index_range_missmatch/2,
    index_range_not_initialized/2,
    index_range_overlap/2,
    input_df/2,
    output_df/2
]).
:- use_module('src/pro/checks/conditions.pro', [
    bool_used_as_number_error_json/1,
    tautology_error_json/1,

    bool_used_as_number/1,
    tautology/1
]).

:- use_module(library(http/json)).

%:- use_module('src/pro/reporting.pro').

:- consult("generated/out.pro").

% consult("src/pro/demo.pro").

errors_json(FileName) :-
    open(FileName, write, Out),
    findall(Error, tautology_error_json(Error), Errors1),
    findall(Error, bool_used_as_number_error_json(Error), Errors2),
    findall(Error, index_range_overlap_error_json(Error), Errors3),
    findall(Error, index_range_missmatch_error_json(Error), Errors4),
    findall(Error, index_range_not_initialized_error_json(Error), Errors5),
    append([Errors1, Errors2, Errors3, Errors4, Errors5], Errors),
    json_write_dict(Out, Errors),
    close(Out).

print_identifier_info(AliasedExpression) :-
    forall(
        expression_identifier(AliasedExpression, AliasedIdentifier),
        (
            encode:decode_name(AliasedIdentifier, DeclaredCsIds, Identifier),
            call_stack:from_ids(DeclaredCsIds, DeclaredCs),
            call_stack:to_string(DeclaredCs, DeclaredCsStr),
            string_chars(DeclaredCsStr, DeclaredChars),
            writef:writef("%t declared in %s\n", [Identifier, DeclaredChars])
        )
    ),
    writef:writef("---\n", []).

input_dfs_from_main(Df) :-
    main_root_ctx(RootCtx),
    walk(RootCtx, Es),
    ranges:input_df(Es, Df).

output_dfs_from_main(Df) :-
    main_root_ctx(RootCtx),
    walk(RootCtx, Es),
    ranges:output_df(Es, Df).

list_dfs(Pred) :-
    forall(
        call(Pred, _{'local': Local, 'true': EncodedTrue, 'where': WhereEs}),
        (
            execution_sequence:to_chars(WhereEs, WhereCsStr),
            string_chars(WhereCsStr, WhereChars),
            decode_names(EncodedTrue, True),

            repr_chars(Local, LocalChars),
            repr_chars(True, TrueChars),
            writef:writef("%s\nlocal name: %s, true name: %s\n", [WhereChars, LocalChars, TrueChars]),
            print_identifier_info(EncodedTrue)
        )
    ).

list_input_dfs() :- list_dfs(input_dfs_from_main).
list_output_dfs() :- list_dfs(output_dfs_from_main).

index_range_print(Range) :-
    ranges:index_range_unpack(Range, Lower, Upper, Step),

    encode:decode_names(Lower, LowerDecoded),
    expressions:repr(LowerDecoded, LowerRepr),

    encode:decode_names(Upper, UpperDecoded),
    expressions:repr(UpperDecoded, UpperRepr),

    encode:decode_names(Step, StepDecoded),
    expressions:repr(StepDecoded, StepRepr),

    get_dict('df', Range, Df),
    get_dict('where', Df, WhereEs),

    execution_sequence:to_chars(WhereEs, WhereCsChars),

    writef:writef("In %s:\n  from %t to %t with step %t\n", [WhereCsChars, LowerRepr, UpperRepr, StepRepr]).

index_range_missmatch_print_all() :-
    main_root_ctx(RootCtx),
    forall(
        ranges:index_range_missmatch(RootCtx, Error),
        error_print(Error)
    ).

index_range_not_initialized_print_all() :-
    main_root_ctx(RootCtx),
    forall(
        ranges:index_range_not_initialized(RootCtx, Error),
        error_print(Error)
    ).

index_range_overlap_print_all() :-
    main_root_ctx(RootCtx),
    forall(
        ranges:index_range_overlap(RootCtx, Error),
        error_print(Error)
    ).

condition_tautology_print_all() :-
    forall(
        conditions:tautology(Error),
        error_print(Error)
    ).

bool_used_as_number_error_print_all() :-
    forall(
        conditions:bool_used_as_number(Error),
        error_print(Error)
    ).

print_all_errors() :-
    index_range_missmatch_print_all(),
    index_range_not_initialized_print_all(),
    index_range_overlap_print_all(),
    condition_tautology_print_all(),
    bool_used_as_number_error_print_all().

e :- print_all_errors().

:- discontiguous error_print/1.

error_print(Error) :-
    Error = error{
        'error_code': ErrorCode,
        'details': details{
            'initialized': InitRange,
            'used': UseRange
        }
    },
    member(ErrorCode, ["LUNA18", "LUNA19", "LUNA20", "LUNA21", "LUNA22"]),
    get_dict('df', InitRange, Df), get_dict('true', Df, luna_ref([BaseName|_])),
    decode_name(BaseName, DeclaredCsIds, DecodedBaseName),
    call_stack:from_ids(DeclaredCsIds, DeclaredCs),
    call_stack:to_string(DeclaredCs, DeclaredCsStr),
    string_chars(DeclaredCsStr, DeclaredCsChars),
    writef:writef("Found %t: DF %t\n", [ErrorCode, DecodedBaseName]),
    writef:writef("Declared: %s\n", [DeclaredCsChars]),
    writef:writef("Initialized: ", []),
    index_range_print(InitRange),
    writef:writef("Used: ", []),
    index_range_print(UseRange),
    writef:writef("\n", []).

error_print(Error) :-
    Error = error{
        'error_code': ErrorCode,
        'details': details{
            'used': UseRange,
            'use_conditions': UseCondsRaw,
            'initialized': InitRange,
            'init_conditions': InitCondsRaw
        }
    },
    ErrorCode = "LUNA37",
    get_dict('df', InitRange, Df), get_dict('true', Df, luna_ref([BaseName|_])),
    decode_name(BaseName, DeclaredCsIds, DecodedBaseName),
    call_stack:from_ids(DeclaredCsIds, DeclaredCs),
    call_stack:to_string(DeclaredCs, DeclaredCsStr),
    string_chars(DeclaredCsStr, DeclaredCsChars),
    writef:writef("Found %t: DF %t\n", [ErrorCode, DecodedBaseName]),
    writef:writef("Declared: %s\n", [DeclaredCsChars]),
    writef:writef("Initialized:\n", []),
    maplist(encode:decode_names, InitCondsRaw, InitConditions),
    maplist(expressions:repr_chars, InitConditions, InitCondReprs),
    writef("When ", []),
    forall(
        member(IntiCondRepr, InitCondReprs),
        writef("%s, ", [IntiCondRepr])
    ),
    writef("\n", []),
    index_range_print(InitRange),
    writef:writef("Used:\n", []),
    maplist(encode:decode_names, UseCondsRaw, UseConditions),
    maplist(expressions:repr_chars, UseConditions, UseCondReprs),
    writef("When ", []),
    forall(
        member(UseCondRepr, UseCondReprs),
        writef("%s, ", [UseCondRepr])
    ),
    writef("\n", []),
    index_range_print(UseRange),
    writef:writef("\n", []).

error_print(Error) :-
    Error = error{
        'error_code': ErrorCode,
        'details': details{
            'used': UseRange
        }
    },
    ErrorCode = "LUNA37",
    get_dict('df', UseRange, Df), get_dict('true', Df, luna_ref([BaseName|_])),
    decode_name(BaseName, DeclaredCsIds, DecodedBaseName),
    call_stack:from_ids(DeclaredCsIds, DeclaredCs),
    call_stack:to_string(DeclaredCs, DeclaredCsStr),
    string_chars(DeclaredCsStr, DeclaredCsChars),
    writef:writef("Found %t: DF %t\n", [ErrorCode, DecodedBaseName]),
    writef:writef("Declared: %s\n", [DeclaredCsChars]),
    writef:writef("Used:\n", []),
    index_range_print(UseRange),
    writef:writef("\n", []).

error_print(Error) :-
    Error = error{
        'error_code': ErrorCode,
        'details': details{
            'ranges': [InitRange1, InitRange2]
        }
    },
    ErrorCode = "LUNA35",

    get_dict('df', InitRange1, Df), get_dict('true', Df, luna_ref([BaseName|_])),
    encode:decode_name(BaseName, DeclaredCsIds, DecodedBaseName),
    call_stack:from_ids(DeclaredCsIds, DeclaredCs),
    call_stack:to_string(DeclaredCs, DeclaredCsStr),
    string_chars(DeclaredCsStr, DeclaredCsChars),
    writef:writef("Found %t: DF %t\n", [ErrorCode, DecodedBaseName]),
    writef:writef("Declared: %s\n", [DeclaredCsChars]),
    writef:writef("Initialized: ", []),
    index_range_print(InitRange1),
    writef:writef("Also in: ", []),
    index_range_print(InitRange2),
    writef:writef("\n", []).

error_print(Error) :-
    Error = _{
        'error_code': ErrorCode,
        'details': details{
            'type': Type,
            'cond': Cond,
            'statement_id': Id
        }
    },
    ErrorCode = "LUNA23",

    repr(Cond, DecodedCondStr),
    string_chars(DecodedCondStr, DecodedCondChars),
    call_stack:from_ids([Id], Cs),
    call_stack:to_string(Cs, IfCsStr),
    string_chars(IfCsStr, IfCsChars),
    writef:writef("Found %t:\n", [ErrorCode]),
    writef:writef("In: %s\n", [IfCsChars]),
    writef:writef("Condition %s is always %t\n", [DecodedCondChars, Type]),
    writef:writef("\n", []).

error_print(Error) :-
    Error = _{
        'error_code': ErrorCode,
        'details': details{
            'bad_expr': BadExpr,
            'arg_index': ArgIndex,
            'statement_id': Id
        }
    },
    ErrorCode = "LUNA25",

    repr(BadExpr, BadExprStr),
    string_chars(BadExprStr, BadExprChars),
    call_stack:from_ids([Id], Cs),
    call_stack:to_string(Cs, CsStr),
    string_chars(CsStr, CsChars),
    writef:writef("Found %t:\n", [ErrorCode]),
    writef:writef("In: %s\n", [CsChars]),
    writef:writef("Boolean value used in numerical context: argument %t of %s\n", [ArgIndex, BadExprChars]),
    writef:writef("\n", []).

% aliases - find input dfs
/*
luna_subroutine(MainId, _, "main", _, _, Body), CallStack = [MainId], Aliases = aliases{}, input_df(Body, Aliases, CallStack, Local, True, Where).
*/

% simple-loop-ok
%  luna_subroutine(MainId, _, "main", _, _, Body), CallStack = [MainId], Aliases = aliases{}, walk(Body, Aliases, CallStack).
/* luna_subroutine(MainId, _, "main", _, _, Body), CallStack = [MainId], Aliases = aliases{}, */
% walk([8, 9], aliases{}, [7]).
% walk([8, 9], aliases{}, [7], [[7, "main", _{}, _{}]], ExecutionSequence), maplist(first, ExecutionSequence, CallStack), call_stack:to_string(CallStack, S).

% nested-calls
% walk([20,21,22,26,27,28,31,34,35,38,42,43,44,45], aliases{}, [19]).

% expressions
% 2*i + 1 - 1 + 1 - 1
% 47 ?- simplify(["-", ["+", ["-", ["+", ["*", "i", 2], 1], 1], 1], 1], R).
% R = ["*", "i", 2].

% for-loop-error
% luna_subroutine(MainId, _, "main", struct, _, Body), index_range_mismatch(Body, _{}, [MainId]).

% aliases
%  luna_subroutine(MainId, _, "main", struct, _, Body), input_df(Body, _{}, [MainId], DfExpr, FullExpr, ExSeq), execution_sequence_call_stack(ExSeq, CallStack), call_stack:to_string(CallStack, S).
%  luna_subroutine(MainId, _, "main", struct, _, Body), output_df(Body, _{}, [MainId], Expr, ExSeq), execution_sequence_call_stack(ExSeq, CallStack), call_stack:to_string(CallStack, S).