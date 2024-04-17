:- module(ranges, [
    index_range_missmatch_error_json/1,
    index_range_not_initialized_error_json/1,
    index_range_overlap_error_json/1,

    index_range_missmatch/2,
    index_range_not_initialized/2,
    index_range_overlap/2,
    index_range_unpack/4,
    input_df/2,
    output_df/2
]).

:- use_module('src/pro/terms/expressions.pro', [
    identifier/1,
    expression_compare/3
]).
:- use_module('src/pro/terms/arithmetic.pro', [
    indexed_name/5,
    linear_expression/4
]).
:- use_module('src/pro/terms/logic.pro', [
    and/2,
    simultaneous/2
]).
:- use_module('src/pro/terms/ref.pro', [rewrite_without_refs/4]).
:- use_module('src/pro/ja.pro', [
    input_parameter/2,
    output_parameter/3,
    position/3,
    statement/2,
    subroutine/3
]).
:- use_module('src/pro/encode.pro', [
    encode_name/3
]).
:- use_module('src/pro/aliases.pro', [
    extend_from_body/4,
    extend/4,
    resolve_in_all/3,
    resolve/3
]).
:- use_module('src/pro/execution_sequence.pro', [
    walk/2,
    to_call_stack/2,
    for/2,
    exec/2,
    item/5,
    conditions/2,
    main_root_ctx/1
]).
:- use_module('src/pro/utility.pro').

:- use_module('src/pro/reporting.pro', [format_index_range/2, format_expression_decode/2]).


index_range_missmatch_error_json(ErrorJson) :-
    execution_sequence:main_root_ctx(RootCtx),
    index_range_missmatch(RootCtx, error{
        'error_code': ErrorCode,
        'details': details{
            'initialized': InitRange,
            'used': UseRange
        }
    }),
    reporting:format_index_range(InitRange, InitRangeDict),
    reporting:format_index_range(UseRange, UseRangeDict),
    ErrorJson = error{
        'error_code': ErrorCode,
        'details': details{
            'initialized': InitRangeDict,
            'used': UseRangeDict
        }
    }.

index_range_not_initialized_error_json(ErrorJson) :-
    execution_sequence:main_root_ctx(RootCtx),
    index_range_not_initialized(RootCtx, error{
        'error_code': "LUNA37",
        'details': details{
            'used': UseRange,
            'use_conditions': UseConditions,
            'initialized': InitRange,
            'init_conditions': InitConditions
        }
    }),
    reporting:format_index_range(UseRange, UseRangeDict),
    reporting:format_index_range(InitRange, InitRangeDict),
    maplist(reporting:format_expression_decode, UseConditions, UseConditionsDicts),
    maplist(reporting:format_expression_decode, InitConditions, InitConditionsDicts),
    ErrorJson = error{
        'error_code': "LUNA37",
        'details': details{
            'used': UseRangeDict,
            'use_conditions': UseConditionsDicts,
            'initialized': InitRangeDict,
            'init_conditions': InitConditionsDicts
        }
    }.
index_range_not_initialized_error_json(ErrorJson) :-
    execution_sequence:main_root_ctx(RootCtx),
    index_range_not_initialized(RootCtx, error{
        'error_code': "LUNA37",
        'details': details{
            'used': UseRange
        }
    }),
    reporting:format_index_range(UseRange, UseRangeDict),
    ErrorJson = error{
        'error_code': "LUNA37",
        'details': details{
            'used': UseRangeDict
        }
    }.

index_range_overlap_error_json(ErrorJson) :-
    execution_sequence:main_root_ctx(RootCtx),
    index_range_overlap(RootCtx, error{
        'error_code': "LUNA35",
        'details': details{
            'ranges': [InitRange1, InitRange2]
        }
    }),
    reporting:format_index_range(InitRange1, InitRangeDict1),
    reporting:format_index_range(InitRange2, InitRangeDict2),
    ErrorJson = error{
        'error_code': "LUNA35",
        'details': details{
            'ranges': [InitRangeDict1, InitRangeDict2]
        }
    }.


%! input_df(+Es:list, -LocalExpression, -TrueExpression, -ConsumerEs:list) is nondet
input_df(Es, LocalExpression, TrueExpression, ConsumerEs) :-
    append(_, ConsumerEs, Es), ConsumerEs = [ExecItem|_],
    execution_sequence:item(ExecItem, StatementId, _, Aliases, _), % Type is not important
    ja:input_parameter(StatementId, LocalExpression),
    aliases:resolve(Aliases, LocalExpression, TrueExpression).

%! input_df(+Es:list, -Df) is nondet
input_df(Es, Df) :-
    Df = df_ref{'local': Local, 'true': True, 'where': UseEs},
    input_df(Es, Local, True, UseEs).

%! output_df(+InitializerEs:list, -LocalExpression, -TrueExpression, +InitializerEs:list) is nondet
output_df(InitializerEs, LocalExpression, TrueExpression, InitializerEs) :-
    InitializerEs = [ExecItem|_],
    execution_sequence:item(ExecItem, StatementId, exec, Aliases, _),
    ja:output_parameter(StatementId, LocalExpression, _),
    aliases:resolve(Aliases, LocalExpression, TrueExpression).

%! output_df(+Es:list, -Df) is nondet
output_df(Es, Df) :-
    Df = df_ref{'local': Local, 'true': True, 'where': InitEs},
    output_df(Es, Local, True, InitEs).

%! for_loop(+Es:list, -Loop:dict) is nondet
for_loop(Es, Loop) :-
    Loop = for{
        'var': Var,
        'first': First,
        'last': Last,
        'where': LoopEs
    },
    append(_, LoopEs, Es),
    LoopEs = [ForItem|_],
    execution_sequence:item(ForItem, StatementId, for, Aliases, _),
    execution_sequence:for(ForItem, Var),
    ja:for(StatementId, _, LocalFirst, LocalLast),
    aliases:resolve(Aliases, LocalFirst, First),
    aliases:resolve(Aliases, LocalLast, Last).

df_loop(RootCtx, GetDf, IndexRange) :-
    IndexRange = index_range{
        'df': Df,
        'loop': Loop,
        'step': Step,
        'offset': Offset
    },
    execution_sequence:walk(RootCtx, Es),
    call(GetDf, Es, Df),
        get_dict('true', Df, TrueName),
        get_dict('where', Df, InitEs),
    arithmetic:indexed_name(TrueName, _, Var, Step, Offset),
    for_loop(InitEs, Loop), get_dict('var', Loop, Var).

df_init_loop(RootCtx, IndexRange) :- df_loop(RootCtx, output_df, IndexRange).
df_use_loop(RootCtx, IndexRange) :- df_loop(RootCtx, input_df, IndexRange).

df_single(RootCtx, GetDf, SingleInit) :-
    SingleInit = single_init{
        'df': Df,
        'var': Var,
        'step': Step,
        'offset': Offset
    },
    execution_sequence:walk(RootCtx, Es),
    call(GetDf, Es, Df),
        get_dict('true', Df, TrueName),
        get_dict('where', Df, InitEs),
    arithmetic:indexed_name(TrueName, _, Var, Step, Offset),
    \+ (
        for_loop(InitEs, Loop),
        get_dict('var', Loop, Var)
    ).

df_init_single(RootCtx, SingleInit) :- df_single(RootCtx, output_df, SingleInit).

maybe(Lhs, Sign, Rhs) :-
    ref:rewrite_without_refs(Lhs, _{0:0}, LhsRefs, LhsWithoutRefs),
    ref:rewrite_without_refs(Rhs, LhsRefs, _, RhsWithoutRefs),
    expressions:expression_compare(Sign, LhsWithoutRefs, RhsWithoutRefs).

never(Lhs, Sign, Rhs) :-
    ref:rewrite_without_refs(Lhs, _{0:0}, LhsRefs, LhsWithoutRefs),
    ref:rewrite_without_refs(Rhs, LhsRefs, _, RhsWithoutRefs),
    \+ expressions:expression_compare(Sign, LhsWithoutRefs, RhsWithoutRefs).

index_range_df_base_name(IndexRange, BaseName) :-
    get_dict('df', IndexRange, Df),
    get_dict('true', Df, luna_ref([BaseName|_])).

index_range_unpack(IndexRange, Lower, Upper, Step) :-
    _{'step': Step, 'offset': Offset, 'loop': Loop} :< IndexRange,
    _{'first': First, 'last': Last} :< Loop,
    linear_expression(Lower, First, Step, Offset),
    linear_expression(Upper, Last, Step, Offset).

index_range_missing_covered_by_single(InitRange, Var, Low, High, Step) :-
    index_range_df_base_name(InitRange, BaseName),
    forall(
        between(Low, High, Offset),
        (
            execution_sequence:main_root_ctx(RootCtx),
            df_init_single(RootCtx, SingleInit),
            index_range_implies(InitRange, SingleInit),
            get_dict('df', SingleInit, Df),
            get_dict('true', Df, TrueExpr),
            arithmetic:indexed_name(TrueExpr, BaseName, Var, Step, Offset)
        )
    ).

index_range_missing_indices(UseBoudnary, InitBoudnary, Var, Low, High, Step) :-
    arithmetic:linear_expression(UseBoudnary, Var, Step, UseOffset),
    arithmetic:linear_expression(InitBoudnary, Var, Step, InitOffset),
    Low is min(UseOffset, InitOffset),
    High is max(UseOffset, InitOffset).

check_luna18(InitRange, UseRange) :-
    index_range_unpack(InitRange, InitLowerBound, _, _),
    index_range_unpack(UseRange, UseLowerBound, _, _),
    maybe(UseLowerBound, "#<", InitLowerBound),
    maybe(UseLowerBound, "#>=", InitLowerBound).

check_luna19(InitRange, UseRange) :-
    index_range_unpack(InitRange, InitLowerBound, _, _),
    index_range_unpack(UseRange, UseLowerBound, _, _),
    never(UseLowerBound, "#>=", InitLowerBound).

check_luna20(InitRange, UseRange) :-
    index_range_unpack(InitRange, _, InitUpperBound, _),
    index_range_unpack(UseRange, _, UseUpperBound, _),
    maybe(UseUpperBound, "#>", InitUpperBound),
    maybe(UseUpperBound, "#=<", InitUpperBound).

check_luna21(InitRange, UseRange) :-
    index_range_unpack(InitRange, _, InitUpperBound, _),
    index_range_unpack(UseRange, _, UseUpperBound, _),
    never(UseUpperBound, "#=<", InitUpperBound).

check_luna22(InitRange, UseRange) :-
    index_range_unpack(InitRange, _, _, InitStep),
    index_range_unpack(UseRange, _, _, UseStep),
    never(0, "#=", ["mod", UseStep, InitStep]).

index_range_conditions(Range, Conditions) :-
    get_dict('df', Range, Df),
    get_dict('where', Df, Where),
    execution_sequence:conditions(Where, Conditions).

index_range_implies(Range1, Range2) :-
    index_range_conditions(Range1, Conds1),
    index_range_conditions(Range2, Conds2),
    logic:and(Conds1, Cond1),
%    writef("Cond1=%t\n", [Cond1]),
    logic:and(Conds2, Cond2),
%    writef("Cond2=%t\n", [Cond2]),
%    writef("---\n", []),
    logic:implies(Cond1, Cond2).

index_range_covers_lower(InitRange, UseRange) :-
    index_range_unpack(InitRange, InitLowerBound, _, _),
    index_range_unpack(UseRange, UseLowerBound, _, _),

    % Lower boudnary
    (   never(UseLowerBound, "#<", InitLowerBound)
        *-> true
        ; (
            never(UseLowerBound, "#>=", InitLowerBound),
            index_range_missing_indices(UseLowerBound, InitLowerBound, Var, Low, High, Step),
            High1 is High - 1,
            index_range_missing_covered_by_single(InitRange, Var, Low, High1, Step)
        )
    ).

index_range_covers_upper(InitRange, UseRange) :-
    index_range_unpack(InitRange, _, InitUpperBound, _),
    index_range_unpack(UseRange, _, UseUpperBound, _),

    % Upper boundary
    (   never(UseUpperBound, "#>", InitUpperBound)
        *-> true
        ; (
            never(UseUpperBound, "#=<", InitUpperBound),
            index_range_missing_indices(UseUpperBound, InitUpperBound, Var, Low, High, Step),
            Low1 is Low + 1,
            index_range_missing_covered_by_single(InitRange, Var, Low1, High, Step)
        )
    ).

index_range_covers(InitRange, UseRange) :-
    index_range_unpack(InitRange, _, _, InitStep),
    index_range_unpack(UseRange, _, _, UseStep),
    expression_compare("#=", UseStep, ["*", "N", InitStep]),
    index_range_covers_lower(InitRange, UseRange),
    index_range_covers_upper(InitRange, UseRange).

df_init_loop_of(RootCtx, BaseName, InitRange) :-
    df_init_loop(RootCtx, InitRange),
        get_dict('df', InitRange, InitializedDf),
        get_dict('true', InitializedDf, luna_ref([BaseName|_])).

df_use_loop_of(RootCtx, BaseName, UseRange) :-
    df_use_loop(RootCtx, UseRange),
        get_dict('df', UseRange, ConsumedDf),
        get_dict('true', ConsumedDf, luna_ref([BaseName|_])).

index_range_missmatch_check_errors(InitRange, UseRange, Error) :-
    ErrorTypes = [
        ["LUNA18", check_luna18],
        ["LUNA19", check_luna19],
        ["LUNA20", check_luna20],
        ["LUNA21", check_luna21],
        ["LUNA22", check_luna22]
    ],
    member([ErrorCode, ErrorChecker], ErrorTypes),
    call(ErrorChecker, InitRange, UseRange),
    Error = error{
        'error_code': ErrorCode,
        'details': details{
            'initialized': InitRange,
            'used': UseRange
        }
    }.

index_range_missmatch(RootCtx, Error) :-
    df_use_loop_of(RootCtx, BaseName, UseRange),
    bagof(
        InitRange,
        (
            df_init_loop_of(RootCtx, BaseName, InitRange),
            index_range_implies(UseRange, InitRange)
        ),
        InitRanges
    ),
    \+ (
        member(InitRange, InitRanges),
        index_range_covers(InitRange, UseRange)
    ),
    member(InitRange, InitRanges),
    (   index_range_missmatch_check_errors(InitRange, UseRange, Error)
    *-> true
    ;   writef("UseRange=%t\nInitRange=%t\n", [UseRange, InitRange])
    ,   throw("No covering range and no matched error")
    ).

index_range_not_initialized(RootCtx, Error) :-
    df_use_loop_of(RootCtx, BaseName, UseRange),
    \+ (
        df_init_loop_of(RootCtx, BaseName, InitRange),
        index_range_implies(UseRange, InitRange)
    ),
    index_range_conditions(UseRange, UseConditions),

    df_init_loop_of(RootCtx, BaseName, InitRange),
    index_range_conditions(InitRange, InitConditions),

    Error = error{
        'error_code': "LUNA37",
        'details': details{
            'used': UseRange,
            'use_conditions': UseConditions,
            'initialized': InitRange,
            'init_conditions': InitConditions
        }
    }.

index_range_not_initialized(RootCtx, Error) :-
    df_use_loop_of(RootCtx, BaseName, UseRange),
    \+ df_init_loop_of(RootCtx, BaseName, _),

    Error = error{
        'error_code': "LUNA37",
        'details': details{
            'used': UseRange
        }
    }.

:- use_module(library(clpfd)).

check_overlap(L1, U1, S1, L2, U2, S2) :-
    ref:rewrite_without_refs(L1, _{0:0}, L1Refs, L1WithoutRefs),
    expressions:expression_string(["+", L1WithoutRefs, ["*", "N1", S1]], X1Str),

    ref:rewrite_without_refs(U1, L1Refs, U1Refs, U1WithoutRefs),
    expressions:expression_string(U1WithoutRefs, U1Str),

    ref:rewrite_without_refs(L2, U1Refs, L2Refs, L2WithoutRefs),
    expressions:expression_string(["+", L2WithoutRefs, ["*", "N2", S2]], X2Str),

    ref:rewrite_without_refs(U2, L2Refs, _, U2WithoutRefs),
    expressions:expression_string(U2WithoutRefs, U2Str),

    atomics_to_string([
        "(#>=(N1, 0), #>=(N2, 0), ",
        "#=(", X1Str, ", ", X2Str, "), ",
        "#=<(", X1Str, ", ", U1Str, "), ",
        "#=<(", X2Str, ", ", U2Str, "))"
    ], "", ClpfdTermStr),
    term_string(ClpfdTerm, ClpfdTermStr),
    ClpfdTerm.

index_range_overlaps_with(Range1, Range2) :-
    index_range_conditions(Range1, Conds1),
    index_range_conditions(Range2, Conds2),
    logic:and(Conds1, Cond1),
%    writef("Cond1=%t\n", [Cond1]),
    logic:and(Conds2, Cond2),
%    writef("Cond2=%t\n", [Cond2]),
%    writef("---\n", []),
    logic:simultaneous(Cond1, Cond2),

    index_range_unpack(Range1, LowerBound1, UpperBound1, Step1),
    index_range_unpack(Range2, LowerBound2, UpperBound2, Step2),
    check_overlap(LowerBound1, UpperBound1, Step1, LowerBound2, UpperBound2, Step2).

index_range_where_cs(Range, LoopCs) :-
    get_dict('df', Range, Df),
    get_dict('where', Df, Es),
    execution_sequence:to_call_stack(Es, LoopCs).

index_range_overlap(RootCtx, Error) :-
    df_init_loop_of(RootCtx, BaseName, InitRange1), index_range_where_cs(InitRange1, Cs1),
    df_init_loop_of(RootCtx, BaseName, InitRange2), index_range_where_cs(InitRange2, Cs2),
    compare((<), Cs1, Cs2),

    index_range_overlaps_with(InitRange1, InitRange2),
    Error = error{
        'error_code': "LUNA35",
        'details': details{
            'ranges': [InitRange1, InitRange2]
        }
    }.
