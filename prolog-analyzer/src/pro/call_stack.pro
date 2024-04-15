:- module(call_stack, [
    entry_from_id/2,
    from_ids/2,
    to_string/2
]).

:- use_module('src/pro/ja.pro').

entry_from_id(Id, Entry) :-
    ja:position(Id, File, Line),
    ja:exec(Id, Name, _),
    Entry = entry{
        'name': Name,
        'file': File,
        'line': Line
    },
    !.
entry_from_id(Id, Entry) :-
    ja:position(Id, File, Line),
    ja:subroutine(Id, Name, _),
    Entry = entry{
        'name': Name,
        'file': File,
        'line': Line
    },
    !.
entry_from_id(Id, Entry) :-
    ja:position(Id, File, Line),
    ja:statement(Id, Type),
    atom_string(Type, TypeStr),
    Entry = entry{
        'name': TypeStr,
        'file': File,
        'line': Line
    },
    !.
entry_from_id(Id, _) :-
    term_string(Id, IdStr),
    atomics_to_string(["entry_from_id/2: Entry id not recognized: ", IdStr], "", Message),
    throw(Message).

from_ids(StatementIds, Cs) :-
    reverse(StatementIds, StatementIdsR),
    ja:subroutine(MainId, "main", _),
    entry_from_id(MainId, MainEntry),
    maplist(entry_from_id, StatementIdsR, Cs1),
    (   Cs1 = [MainEntry|_]
    *-> Cs = Cs1, true
    ;   Cs = [MainEntry|Cs1]
    ).

call_stack_entry_string(
    _{'name': Name, 'file': File, 'line': Line},
    Str
) :-
    atomics_to_string([File, Line, Name], ":", Str),
    !.
call_stack_entry_string(Entry, _) :-
    throw(value_error("call_stack_entry_string/2: Unexpected entry format", Entry)).

to_string(CallStack, S) :-
    maplist(call_stack_entry_string, CallStack, EntryStrings),
    atomics_to_string(EntryStrings, " -> ", S).
