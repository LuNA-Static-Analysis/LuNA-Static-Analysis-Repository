:- module(utility, [
    wrap/2,
    pair/3,
    triplet/4,
    key_value_pair/3,
    zip_to_dict/4
]).

wrap(A, [A]) :- true.
unwrap(Wrapped, Unwrapped) :- wrap(Unwrapped, Wrapped).

pair(A, B, [A, B]) :- true.

triplet(A, B, C, [A, B, C]) :- true.

key_value_pair(Key, Value, Result) :- Result = Key:Value.

zip_to_dict(Keys, Values, Tag, Dict) :-
    maplist(atom_string, KeyAtoms, Keys),
    maplist(key_value_pair, KeyAtoms, Values, KeyValuePairs),
    dict_create(Dict, Tag, KeyValuePairs).
