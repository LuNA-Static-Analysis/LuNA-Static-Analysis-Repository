:- module(conditions, [
    bool_used_as_number_error_json/1,
    tautology_error_json/1,

    bool_used_as_number/1,
    tautology/1
]).

:- use_module('src/pro/terms/logic.pro', [tautology/2, bool_used_as_number/3]).
:- use_module('src/pro/call_stack.pro', [entry_from_id/2]).

:- use_module('src/pro/reporting.pro', [format_expression_no_decode/2]).

bool_used_as_number_error_json(ErrorJson) :-
    bool_used_as_number(error{
        'error_code': "SEM7",
        'details': details{
            'bad_expr': BadExpr,
            'arg_index': ArgIndex,
            'statement_id': Id
        }
    }),
    call_stack:entry_from_id(Id, Entry),
    reporting:format_expression_no_decode(BadExpr, BadExprDict),
    ErrorJson = error{
        'error_code': "SEM7",
        'details': details{
            'bad_expr': BadExprDict,
            'arg_index': ArgIndex,
            'where': Entry
        }
    }.

tautology_error_json(ErrorJson) :-
    tautology(error{
        'error_code': "SEM5",
        'details': details{
            'type': T,
            'cond': Cond,
            'statement_id': Id
        }
    }),
    call_stack:entry_from_id(Id, Entry),
    reporting:format_expression_no_decode(Cond, CondDict),
    ErrorJson = error{
        'error_code': "SEM5",
        'details': details{
            'type': T,
            'condition': CondDict,
            'where': Entry
        }
    }.

bool_used_as_number(Error) :-
    ja:if(Id, Cond),
    logic:bool_used_as_number(Cond, BadExpr, ArgIndex),
    Error = error{
        'error_code': "SEM7",
        'details': details{
            'bad_expr': BadExpr,
            'arg_index': ArgIndex,
            'statement_id': Id
        }
    }.

bool_used_as_number(Error) :-
    ja:for(Id, _, First, _),
    logic:bool_used_as_number(First, BadExpr, ArgIndex),
    Error = error{
        'error_code': "SEM7",
        'details': details{
            'bad_expr': BadExpr,
            'arg_index': ArgIndex,
            'statement_id': Id
        }
    }.

bool_used_as_number(Error) :-
    ja:for(Id, _, _, Last),
    logic:bool_used_as_number(Last, BadExpr, ArgIndex),
    Error = error{
        'error_code': "SEM7",
        'details': details{
            'bad_expr': BadExpr,
            'arg_index': ArgIndex,
            'statement_id': Id
        }
    }.

bool_used_as_number(Error) :-
    ja:exec(Id, _, Args),
    member(Arg, Args),
    logic:bool_used_as_number(Arg, BadExpr, ArgIndex),
    Error = error{
        'error_code': "SEM7",
        'details': details{
            'bad_expr': BadExpr,
            'arg_index': ArgIndex,
            'statement_id': Id
        }
    }.

tautology(Error) :-
    ja:if(Id, Cond),
    logic:tautology(Cond, T),
    Error = error{
        'error_code': "SEM5",
        'details': details{
            'type': T,
            'cond': Cond,
            'statement_id': Id
        }
    }.