:- module(
    dict_utils,
    [
        dict_max_key/2,
        dict_find_key/4
    ]
).
%:- meta_predicate dict_find_key(2, -, -).

% TODO implement get_dict wrapper that handles key paths

key_max(K1, K2, Max) :-
    K1 > K2
    ->  Max = K1
    ;   Max = K2.

% dict_max_key(+Dict, -MaxKey) is det
dict_max_key(Dict, MaxKey) :-
    dict_pairs(Dict, _, Pairs),
    pairs_keys(Pairs, Keys),
    foldl(key_max, Keys, 0, MaxKey).

% dict_find_key(+Equals, +Dict, +TargetValue, -Key) is semidet
dict_find_key(Equals, Dict, TargetValue, Key) :-
    get_dict(Key, Dict, OtherValue),
    call(Equals, TargetValue, OtherValue),
    !.
