:- module(reporting, [
    format_expression_decode/2,
    format_expression_no_decode/2,
    format_df/2,
    format_for/2,
    format_index_range/2
]).

:- use_module('src/pro/encode.pro', [decode_names/2, decode_name/3]).
:- use_module('src/pro/call_stack.pro', [from_ids/2]).
:- use_module('src/pro/execution_sequence.pro', [to_call_stack/2]).
:- use_module('src/pro/terms/expressions.pro', [repr/2, identifier/1]).

expression_strip_luna_prefix(Expr, Expr) :-
    number(Expr),
    !.
expression_strip_luna_prefix(ExprIn, ExprOut) :-
    expressions:identifier(ExprIn),
    sub_string(ExprIn, 0, 5, _, "LUNA_"),
    !,
    BeforeName is 5,
    sub_string(ExprIn, BeforeName, _, 0, ExprOut),
    !.
expression_strip_luna_prefix(Expr, Expr) :-
    expressions:identifier(Expr),
    !.
expression_strip_luna_prefix([Op|Args], [Op|ArgsOut]) :-
    maplist(expression_strip_luna_prefix, Args, ArgsOut),
    !.
expression_strip_luna_prefix(luna_ref(Items), luna_ref(ItemsOut)) :-
    maplist(expression_strip_luna_prefix, Items, ItemsOut),
    !.
expression_strip_luna_prefix(ExprIn, _) :-
    term_string(ExprIn, ExpressionStr),
    atomics_to_string(["expression_strip_luna_prefix/2: Unexpected expression format: ", ExpressionStr], "", Message),
    throw(Message).

format_expression_decode(ExprIn, ExprOut) :-
    encode:decode_names(ExprIn, DecodedExpr),
    expression_strip_luna_prefix(DecodedExpr, DecodedExprNoPrefixes),
    expressions:repr(DecodedExprNoPrefixes, ExprOut).

format_expression_no_decode(ExprIn, ExprOut) :-
    expression_strip_luna_prefix(ExprIn, DecodedExprNoPrefixes),
    expressions:repr(DecodedExprNoPrefixes, ExprOut).

format_for(Loop, FormattedLoop) :-
    Loop = for{
        'var': Var,
        'first': First,
        'last': Last,
        'where': LoopEs
    },
    format_expression_decode(Var, VarDict),
    format_expression_decode(First, FirstDict),
    format_expression_decode(Last, LastDict),
    execution_sequence:to_call_stack(LoopEs, LoopCs),
    FormattedLoop = for{
        'var': VarDict,
        'first': FirstDict,
        'last': LastDict,
        'where': LoopCs
    }.

format_index_range(Range, FormattedRange) :-
    Range = index_range{
        'df': Df,
        'loop': Loop,
        'step': Step,
        'offset': Offset
    },
    format_df(Df, FormattedDf),
    format_for(Loop, FormattedFor),
    format_expression_decode(Step, StepDict),
    format_expression_decode(Offset, OffsetDict),
    FormattedRange = index_range{
        'df': FormattedDf,
        'loop': FormattedFor,
        'step': StepDict,
        'offset': OffsetDict
    }.

base_name(BaseName, BaseName) :- identifier(BaseName), !.
base_name(luna_ref([BaseName|_]), BaseName) :- true, !.

format_df(Df, FormattedDf) :-
    Df = df_ref{
        'true': True,
        'local': Local,
        'where': WhereEs
    },
    execution_sequence:to_call_stack(WhereEs, WhereCs),

    base_name(True, BaseName),
    encode:decode_name(BaseName, DeclaredCsIds, DecodedBaseName),
    expression_strip_luna_prefix(DecodedBaseName, FormattedBaseName),
    call_stack:from_ids(DeclaredCsIds, DeclaredCs),

    format_expression_decode(True, TrueDict),
    format_expression_no_decode(Local, LocalDict),

    FormattedDf = df_ref{
        'df': df{
            'name': FormattedBaseName,
            'declared': DeclaredCs
        },
        'local': LocalDict,
        'true': TrueDict,
        'where': WhereCs
    }.
