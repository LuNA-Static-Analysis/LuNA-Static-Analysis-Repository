:- module(aliases, [
    extend_from_body/4,
    extend/4,
    resolve/3,
    resolve_in_all/3
]).

:- use_module('src/pro/utility.pro').

extend_from_body(BaseAliases, Body, CallStack, ExtendedAliases) :-
    member(DfsId, Body),
    ja:dfs(DfsId, Names),

    maplist(atom_string, AtomNames, Names),
    maplist(encode:encode_name(CallStack), AtomNames, Aliases),
    maplist(utility:key_value_pair, AtomNames, Aliases, KeyValuePairs),

    put_dict(KeyValuePairs, BaseAliases, ExtendedAliases),
    % cut if `dfs` exists
    !.

extend_from_body(BaseAliases, _, _, ExtendedAliases) :-
    ExtendedAliases = BaseAliases.


extend(BaseAliases, Names, CallStack, ExtendedAliases) :-
    maplist(atom_string, AtomNames, Names),
    maplist(encode:encode_name(CallStack), AtomNames, Aliases),
    maplist(utility:key_value_pair, AtomNames, Aliases, KeyValuePairs),
    put_dict(BaseAliases, KeyValuePairs, ExtendedAliases).


/* Resolving aliases in expressions
 *
 * Cuts are used since the `Expression`'s structure can be a limited number of
 * things (number, identifier, expression tree etc.), and if it is not one of them,
 * there's an error somewhere that shoud be reported.
 * Therefore if one variant succedes, we dont look for others, and the error is
 * reported only if all variants are not applicable.
 */
resolve(_, Expression, Expression) :-
    number(Expression), !.

resolve(Aliases, Expression, TrueExpression) :-
    expressions:identifier(Expression),
    atom_string(Identifier, Expression),
    get_dict(Identifier, Aliases, TrueExpression),
    !.

resolve(Aliases, luna_ref(Expressions), TrueExpression) :-
    maplist(resolve(Aliases), Expressions, TrueExpressions),
    (TrueExpressions = [luna_ref([TrueBaseName|TrueIndicesBegin])|TrueIndicesEnd]
    ->  append([TrueIndicesBegin, TrueIndicesEnd], TrueIndices),
        TrueExpression = luna_ref([TrueBaseName|TrueIndices])
    ;
        TrueExpression = luna_ref(TrueExpressions)
    ),
    !.

resolve(Aliases, [Op, Lhs, Rhs], TrueExpression) :-
    resolve(Aliases, Lhs, TrueLhs),
    resolve(Aliases, Rhs, TrueRhs),
    TrueExpression = [Op, TrueLhs, TrueRhs], !.

resolve(Aliases, [Op, Operand], TrueExpression) :-
    resolve(Aliases, Operand, TrueOperand),
    TrueExpression = [Op, TrueOperand], !.

% throw an error if an expression is not one of supported things instead of failing silently
resolve(_, Expression, _) :-
    term_string(Expression, ExpressionStr),
    atomics_to_string(["resolve/3: Unexpected expression format: ", ExpressionStr], "", Message),
    throw(Message).

resolve_in_all(Aliases, Expressions, TrueExpressions) :-
    maplist(resolve(Aliases), Expressions, TrueExpressions).