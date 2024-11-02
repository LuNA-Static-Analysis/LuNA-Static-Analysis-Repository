:- module(ranges, [
    index_range_not_initialized_error_json/1,
    index_range_overlap_error_json/1,

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


format_range_and_conditions(Range, RangeAndConditioins) :-
    reporting:format_index_range(Range, RangeDict),
    index_range_conditions(Range, Conditions),
    maplist(reporting:format_expression_decode, Conditions, ConditionsDicts),
    RangeAndConditioins = [RangeDict, ConditionsDicts],
    !.


format_range_and_conditions(Range, RangeAndConditioins) :-
    single_index{'df': Df} :< Range,
    reporting:format_df(Df, DfDict),
    execution_sequence:conditions(Df.'where', Conditions),
    maplist(reporting:format_expression_decode, Conditions, ConditionsDicts),
    RangeAndConditioins = [DfDict, ConditionsDicts],
    !.

index_range_not_initialized_error_json(ErrorJson) :-
    execution_sequence:main_root_ctx(RootCtx),
    index_range_not_initialized(RootCtx, error{
        'error_code': "SEM3.3",
        'details': details{
            'used': UseRange,
            'initialized': InitRanges
        }
    }),
    reporting:format_index_range(UseRange, UseRangeDict),
    index_range_conditions(UseRange, UseConditions),
    maplist(reporting:format_expression_decode, UseConditions, UseConditionsDicts),
    maplist(format_range_and_conditions, InitRanges, InitRangesFormatted),
    ErrorJson = error{
        'error_code': "SEM3.3",
        'details': details{
            'used': UseRangeDict,
            'use_conditions': UseConditionsDicts,
            'initialized': InitRangesFormatted
        }
    }.

index_range_overlap_error_json(ErrorJson) :-
    execution_sequence:main_root_ctx(RootCtx),
    index_range_overlap(RootCtx, error{
        'error_code': "SEM2.2",
        'details': details{
            'ranges': [InitRange1, InitRange2]
        }
    }),
    reporting:format_index_range(InitRange1, InitRangeDict1),
    reporting:format_index_range(InitRange2, InitRangeDict2),
    ErrorJson = error{
        'error_code': "SEM2.2",
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
    SingleInit = single_index{
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
    index_range{'step': Step, 'offset': Offset, 'loop': Loop} :< IndexRange,
    _{'first': First, 'last': Last} :< Loop,
    linear_expression(Lower, First, Step, Offset),
    linear_expression(Upper, Last, Step, Offset).

index_range_unpack(IndexRange, Lower, Upper, 0) :-
    single_index{'step': IndexStep, 'offset': IndexOffset, 'var': IndexVar} :< IndexRange,
    linear_expression(Lower, IndexVar, IndexStep, IndexOffset),
    Upper = Lower.

index_range_unpack(IndexRange, Lower, Upper, Step) :-
    index_range_union{
        'lower': Lower, 
        'upper': Upper, 
        'step': Step
    } :< IndexRange.

index_range_concat_ordered(Range1, Range2, Step, Result) :-
    index_range_unpack(Range1, Lower1, Upper1, Step),
    index_range_unpack(Range2, Lower2, Upper2, Step),
    ref:expressions_equivalent(["+", Upper1, Step], Lower2),
    Result = index_range_union{
        'lower': Lower1,
        'upper': Upper2,
        'step': Step,
        'ranges': [Range1, Range2]
    }.

index_range_concat_ordered(Range1, Range2, Step, Result) :-
    index_range_unpack(Range1, Lower1, Upper1, Step),
    index_range_unpack(Range2, Lower2, Upper2, Step),

    % Lower2 <= Upper1
    never(Lower2, "#>", Upper1),
    % never(Upper1, "#>", Upper2),

    Upper1AsLinear = ["+", ["*", "N2", Step], Lower2],
    ref:rewrite_without_refs(Upper1AsLinear, _{}, U1LinearRefs, Upper1AsLinearWithoutRefs),
    expressions:expression_string(Upper1AsLinearWithoutRefs, Upper1AsLinearStr),
    ref:rewrite_without_refs(Upper1, U1LinearRefs, U1Refs, Upper1WithoutRefs),
    expressions:expression_string(Upper1WithoutRefs, Upper1Str),
    ref:rewrite_without_refs(Upper2, U1Refs, _, Upper2WithoutRefs),
    expressions:expression_string(Upper2WithoutRefs, Upper2Str),
    atomics_to_string([
        "(#>=(N2, 0), ",
        "#=(", Upper1AsLinearStr, ", ", Upper1Str, "), ",
        "#=<(", Upper1AsLinearStr, ", ", Upper2Str, "))"
    ], "", ClpfdTermStr),
    % throw(ClpfdTermStr),
    term_string(ClpfdTerm, ClpfdTermStr),
    ClpfdTerm,

    Result = index_range_union{
        'lower': Lower1,
        'upper': Upper2,
        'step': Step,
        'ranges': [Range1, Range2]
    }.

index_range_concat_ordered(Range1, Range2, Step, Result) :-
    index_range_unpack(Range1, Index1, Index1, 0),
    index_range_unpack(Range2, Lower2, Upper2, Step),
    ref:expressions_equivalent(["+", Index1, Step], Lower2),
    Result = index_range_union{
        'lower': Index1,
        'upper': Upper2,
        'step': Step,
        'ranges': [Range1, Range2]
    }.

index_range_concat_ordered(Range1, Range2, Step, Result) :-
    index_range_unpack(Range1, Lower1, Upper1, Step),
    index_range_unpack(Range2, Index2, Index2, 0),
    ref:expressions_equivalent(["+", Upper1, Step], Index2),
    Result = index_range_union{
        'lower': Lower1,
        'upper': Index2,
        'step': Step,
        'ranges': [Range1, Range2]
    }.

index_range_concat_ordered(Range1, Range2, Step, Result) :-
    index_range_unpack(Range1, Index1, Index1, 0),
    index_range_unpack(Range2, Index2, Index2, 0),
    ref:expressions_equivalent(["+", Index1, Step], Index2),
    Result = index_range_union{
        'lower': Index1,
        'upper': Index2,
        'step': Step,
        'ranges': [Range1, Range2]
    }.

index_range_merge_once(RangesIn, Step, Union, Rest) :-
    append([L1, [Range1], L2, [Range2], L3], RangesIn),
    (
        index_range_concat_ordered(Range1, Range2, Step, Union)
    ;   index_range_concat_ordered(Range2, Range1, Step, Union)
    ),
    append([L1, L2, L3], Rest).

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
    never(UseLowerBound, "#<", InitLowerBound).

index_range_covers_upper(InitRange, UseRange) :-
    index_range_unpack(InitRange, _, InitUpperBound, _),
    index_range_unpack(UseRange, _, UseUpperBound, _),

    % Upper boundary
    never(UseUpperBound, "#>", InitUpperBound).

index_range_covers(InitRange, UseRange) :-
    index_range_unpack(InitRange, _, _, InitStep),
    index_range_unpack(UseRange, _, _, UseStep),
    expression_compare("#=", UseStep, ["*", "N", InitStep]),
    index_range_covers_lower(InitRange, UseRange),
    index_range_covers_upper(InitRange, UseRange).

index_range_is_covered(UseRange, InitRanges) :-
    member(InitRange, InitRanges),
    index_range_covers(InitRange, UseRange),
    !.

index_range_is_covered(UseRange, InitRanges) :-
    index_range_unpack(UseRange, _, _, UseStep),
    index_range_merge_once(InitRanges, UseStep, Union, Rest),
    index_range_is_covered(UseRange, [Union|Rest]).

df_init_loop_of(RootCtx, BaseName, InitRange) :-
    df_init_loop(RootCtx, InitRange),
        get_dict('df', InitRange, InitializedDf),
        get_dict('true', InitializedDf, luna_ref([BaseName|_])).

df_use_loop_of(RootCtx, BaseName, UseRange) :-
    df_use_loop(RootCtx, UseRange),
        get_dict('df', UseRange, ConsumedDf),
        get_dict('true', ConsumedDf, luna_ref([BaseName|_])).

df_single_init_of(RootCtx, BaseName, SingleInit) :-
    df_init_single(RootCtx, SingleInit),
    SingleInit.df.true = luna_ref([BaseName|_]).

df_init_of(RootCtx, BaseName, InitRange) :- 
    df_init_loop_of(RootCtx, BaseName, InitRange).
df_init_of(RootCtx, BaseName, InitRange) :- 
    df_single_init_of(RootCtx, BaseName, InitRange).

index_range_not_initialized(RootCtx, Error) :-
    df_use_loop_of(RootCtx, BaseName, UseRange),
    index_range_unpack(UseRange, _, _, UseStep),
    findall(
        InitRange,
        df_init_of(RootCtx, BaseName, InitRange),
        InitRanges
    ),
    include(index_range_implies(UseRange), InitRanges, ImpliedInits),
    \+ index_range_is_covered(UseRange, ImpliedInits),
    Error = error{
        'error_code': "SEM3.3",
        'details': details{
            'used': UseRange,
            'initialized': InitRanges
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
        'error_code': "SEM2.2",
        'details': details{
            'ranges': [InitRange1, InitRange2]
        }
    }.
