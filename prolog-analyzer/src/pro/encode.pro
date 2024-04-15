:- module(encode, [
    encode_name/3,
    decode_name/3,
    decode_names/2,
    decode_names_in_all/2
]).

encode_name(CallStack, Name, EncodedName) :-
    atomics_to_string(CallStack, "_", CallStackStr),
    atomics_to_string(["Var", CallStackStr, "__", Name], EncodedName).

empty_string("") :- true.

decode_name(EncodedName, CallStackIds, Name) :-
    % Name
    sub_string(EncodedName, Before, 7, _, "__LUNA_"),
    !,
    BeforeName is Before + 2,
    sub_string(EncodedName, BeforeName, NameLength, 0, Name),
    % CallStack
    AfterCallStack is NameLength + 2,
    sub_string(EncodedName, 3, _, AfterCallStack, EncodedCallStack),
    split_string(EncodedCallStack, "_", "", CallStackItemsRaw),
    % TODO use =("") instead of empty string?
    exclude(empty_string, CallStackItemsRaw, CallStackItems),
    maplist(number_codes, CallStackIds, CallStackItems).

decode_names(Expression, Expression) :- number(Expression), !.
decode_names(Encoded, Expression) :-
    expressions:identifier(Encoded),
    decode_name(Encoded, _, Expression),
    !.
decode_names([Op|EncodedArgs], Expression) :-
    maplist(encode:decode_names, EncodedArgs, Args),
    Expression = [Op|Args],
    !.
decode_names(luna_ref(EncodedItems), Expression) :-
    maplist(encode:decode_names, EncodedItems, Items),
    Expression = luna_ref(Items),
    !.
decode_names(AliasedExpression, _) :-
    term_string(AliasedExpression, ExpressionStr),
    atomics_to_string(["decode_names/2: Unexpected expression format: ", ExpressionStr], "", Message),
    throw(Message).

decode_names_in_all(EncodedExpressions, DecodedExpressions) :-
    maplist(decode_names, EncodedExpressions, DecodedExpressions).