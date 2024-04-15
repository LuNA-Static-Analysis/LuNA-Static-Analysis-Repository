:- module(string_utility, [concat3/4, concat_all/2]).

/*
 *  Helper for concatinating multiple strigs
 */
concat3(S1, S2, S3, Result) :-
    string_concat(S1, S2, Tmp),
    string_concat(Tmp, S3, Result).


concat_all([], "").
concat_all([Prefix|Rest], Result) :-
    concat_all(Rest, RestResult),
    string_concat(Prefix, RestResult, Result).
