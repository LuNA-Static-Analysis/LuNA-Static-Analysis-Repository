% TODO two variants: first works if Key is instantiated (converts to string),
% TODO second will require key to not be instantiated and will convert wahtever get_dict/3 returns to string
get_dict_string(Key, Dict, Value) :-
    atom_string(AtomKey, Key),
    get_dict(AtomKey, Dict, Value).


% helper for getting body of a statement if it has any
% TODO if, while
statement_body(StatementId, Body) :-
    luna_statement(StatementId, _, for, [_, _, _, Body]).

% splits string representation of arithemtic expressions
expression_identifiers(Expression, []) :-
    \+ string(Expression).

expression_identifiers(Expression, Identifiers) :-
    string(Expression),
    split_string(Expression, /* SepChars */ "+-*/()", /* PadChars */ "/ ", Identifiers).


luna_identifier(Expression) :- string(Expression).


/*
 * Usages in expressions:
 *  - Arithmetic expressions
 *  - Indexed names
 */

used_in_expression(Name, Name) :-
    string(Name), !.

% TODO parameter "From" to allow highlighting multiple uses
used_in_expression(Name, Expression /* , From */) :-
    string(Name),
    string(Expression),
    Name \= Expression,
    expression_identifiers(Expression, Identifiers),
    member(Name, Identifiers), !.

% TODO indicate position
used_in_expression(Name, Expression) :-
    Expression = luna_ref(Expressions),
    member(InnerExpression, Expressions),
    used_in_expression(Name, InnerExpression), !.

/*
 * Usages in statemetns:
 *  - In call arguments
 *  - In loop bounds
 */

% exec
name_statement_usages(Name, StatementId, Expression) :-
    luna_statement(StatementId, _, exec, [_, CallArgs]),
    member(Expression, CallArgs),
    used_in_expression(Name, Expression).

% for
name_statement_usages(Name, StatementId, ForFirst) :-
    luna_statement(StatementId, _, for, [_, ForFirst, _, _]),
    used_in_expression(Name, ForFirst).

% for
name_statement_usages(Name, StatementId, ForLast) :-
    luna_statement(StatementId, _, for, [_, _, ForLast, _]),
    used_in_expression(Name, ForLast).

/*
 * TODO every body can have name declarations?
 * Usages in a body (collection of statements):
 *  - In top-level statement
 *  - In a statement inside other statement's body
 */

% top-level
name_body_usages(Name, Body, StatementId, Expression) :-
    member(StatementId, Body),
    name_statement_usages(Name, StatementId, Expression).

% inner
name_body_usages(Name, Body, StatementId, Expression) :-
    member(TopLevelStatementId, Body),
    statement_body(TopLevelStatementId, TopLevelStatementBody),
    name_body_usages(Name, TopLevelStatementBody, StatementId, Expression).

/*
 * Usages in a subroutibe body:
 *  !!! Only names declared in this subroutine !!!
 *  - In top-level statements
 *  - In a statement inside other statement's body
 */
declared_in(Name, SubroutineBody) :-
    member(DfsId, SubroutineBody),
    luna_statement(DfsId, _, dfs, [Names]),
    member(Name, Names).

% `Name` is decleared in `SubroutineName`
name_usages(Name, SubroutineName, StatementId, Expression) :-
    luna_subroutine(_, _, SubroutineName, _, _, SubroutineBody),
    declared_in(Name, SubroutineBody),
    name_body_usages(Name, SubroutineBody, StatementId, Expression).

% `Name` is a parameter of `SubroutineName`
name_usages(Name, SubroutineName, StatementId, Expression) :-
    luna_subroutine(_, _, SubroutineName, _, SubroutineArgs, SubroutineBody),
    member([_, Name], SubroutineArgs),
    name_body_usages(Name, SubroutineBody, StatementId, Expression).

/*
 * Transitive usages in called subroutines.
 */

zip(L1, L2, Result) :-
    maplist(pair, L1, L2, Result).

% extern
name_usages_transitive(Name, Body, Usages) :-
    % Find exec `Callee` that uses `Name` in that `Expression`
    name_body_usages(Name, Body, StatementId, Expression),
    luna_statement(StatementId, _, exec, [Callee, CallArgs]),
    % Find which arguments are `Expression` and match to their `CalleeArgName`
    luna_subroutine(_, _, Callee, extern, CalleeArgNames, _),
    zip(CallArgs, CalleeArgNames, ArgPairs),
    member([Expression, [Type]], ArgPairs),
    % Append found useages
    Usages = [[StatementId, Callee, Expression, Type, ""]].

% structs
name_usages_transitive(Name, Body, Usages) :-
    % Find exec `Callee` that uses `Name` in that `Expression`
    name_body_usages(Name, Body, StatementId, Expression),
    luna_statement(StatementId, _, exec, [Callee, CallArgs]),
    % Find which arguments are `Expression` and match to their `CalleeArgName`
    luna_subroutine(_, _, Callee, struct, CalleeArgNames, CalleBody),
    zip(CallArgs, CalleeArgNames, ArgPairs),
    member([Expression, [Type, CalleeArgName]], ArgPairs),
    % Find `CalleeArgName` in `Callee`'s body
    name_usages_transitive(CalleeArgName, CalleBody, CalleeUsages),
    % Append found useages
    Usages = [[StatementId, Callee, Expression, Type, CalleeArgName]|CalleeUsages].

% TODO
indexed_by(Name, Index, Expression) :-
    Expression = luna_ref([Name, Index]).

key_value_pair(Key, Value, Result) :- Result = Key:Value.

indexed_df(Name, Body, Var, First, Last, Usages) :-
    name_usages_transitive(Name, Body, Usages),
    member([StatementId, _ /* Callee */, _ /* Expression */, _ /* Type */, _ /* CalleeArgName */], Usages),
    luna_statement(_, _, for, [Var, First, Last, ForBody]),
    member(StatementId, ForBody)
%    ,
%    name_usages_transitive(Var, ForBody, VarUsages)
    .


replicate(Element, N, Result) :-
    length(Result, N),
    maplist(=(Element), Result).


%! resolve_alias_if_name(+Aliases, +Exrpression, -TrueExpression) is det
%resolve_name_aliases(Aliases, Exrpression, TrueExpression) :-
%    true.

%! expression_input_df(+Expression, +Aliases:dict, -DfExpression) is nondet
expression_input_df(Expression, _, Expression) :-
    luna_identifier(Expression), base_df_name(Expression), !.

expression_input_df(Expression, Aliases, Expression) :-
    luna_identifier(Expression),
    resolve_aliases(Aliases, Expression, TrueExpression),
    base_df_name(TrueExpression).


% Identifier is a base name (was declared in a `df` operator)
name_alias(_, Identifier, Identifier) :-
    base_df_name(Identifier), !.

% Name has indices
name_alias(CallStack, luna_ref([Identifier|Indices]), NameExpression) :-
    find_exec_call_stack(CallStack, ExecCallStack),
    ExecCallStack = [[_, _, IdentifierAliases, IdentifierTypes]|ParentCallStack],

    get_dict_string(Identifier, IdentifierTypes, name),
    get_dict_string(Identifier, IdentifierAliases, ParameterExpression),

    name_alias(ParentCallStack, ParameterExpression, ResolvedIdentifier),

    resolve_aliases_in_all(IdentifierAliases, Indices, TrueIndicesEnd),

    (ResolvedIdentifier = luna_ref([TrueIdentifier|TrueIndicesBegin])
    ->  append([TrueIndicesBegin, TrueIndicesEnd], TrueIndices),
        NameExpression = luna_ref([TrueIdentifier|TrueIndices])
    ;
        NameExpression = luna_ref([ResolvedIdentifier|TrueIndicesEnd])
    ).


% Identifier is a parameter with type `name`
name_alias(CallStack, Identifier, NameExpression) :-
    find_exec_call_stack(CallStack, ExecCallStack),
    ExecCallStack = [[_, _, IdentifierAliases, IdentifierTypes]|ParentCallStack],

    get_dict_string(Identifier, IdentifierTypes, name),
    get_dict_string(Identifier, IdentifierAliases, ParameterExpression),

    name_alias(ParentCallStack, ParameterExpression, NameExpression).

% TODO Option 1: check call stack?
% TODO Option 2: extend call stack information - save info about what identifiers are declared as dfs
% TODO Option 3: find declaring statement (will have to check call stack later for declaration anyway)
%! base_df_name(+Name:string) is nondet.
base_df_name(Name) :-
    luna_statement(_, _, dfs, [Names]),
    member(Name, Names).

% Find a call stack that ends with an exec
find_exec_call_stack(CallStack, CallStack) :-
    % Entry for exec has 4 entries: [StatementId, Callee, CalleeAliases, CalleeArgTypesDict]
    CallStack = [[_, _, _, _]|_], !.

find_exec_call_stack(CallStack, ExecCallStack) :-
    CallStack = [_|ParentCallStack],
    find_exec_call_stack(ParentCallStack, ExecCallStack).

%! output_df(+Body:list, +Aliases:dict, +CallStack:list, -Expression, -InitCallStack) is nondet
output_df(
    Body, Aliases, CallStack,
    Expression, InitExSeq
) :-
    walk(Body, Aliases, CallStack, [], InitExSeq),

    % Only external code fragments can initialize DFs, therefore it's going to be at the end of a call stack
    InitExSeq = [InitializerExpression|_],
    InitializerExpression = [_, _, CalleeAliases, CalleeArgTypesDict],

    get_dict(ArgName, CalleeArgTypesDict, name), % CalleeArgTypesDict[ArgName] = name
    get_dict(ArgName, CalleeAliases, Expression).

%! input_df(+Body:list, +Aliases:dict, +CallStack:list, -DfExpression, -FullExpression, -UseCallStack) is nondet
input_df(
    Body, Aliases, CallStack,
    DfExpression, InputExpression, UseExSeq
) :-
    % TODO for arithmetic expressions, do not resolve aliases?
    % TODO but what if ```foo(name x) { bar(x + 1) } ... main() { foo(z) }```? it is x + 1 in reality
    % TODO only resolve names?
    % x + 2 * y -> x, y
    % x[i] + 2 -> x[i], i?
    % 10 * x[0][y[k + 1] - 1] -> x[0][y[k + 1] - 1], y[k + 1], k?
    % TODO check if an identifier is a `name` (can be argument name, loop counter etc.)
    walk(Body, Aliases, CallStack, [], ExSeq),

    append([_, [ConsumerExec], Rest], ExSeq),
    ConsumerExec = [_, _, CalleeAliases, CalleeTypes],

    get_dict(Alias, CalleeTypes, value),
    get_dict(Alias, CalleeAliases, InputExpression),
    DfExpression = "TODO",

    UseExSeq = [ConsumerExec|Rest].


index_range_mismatch(Body, Aliases, CallStack) :-
    use_loops(Body, Aliases, CallStack, Name, UseFirst, UseLast, UseStep, UseOffset),
    \+ (
        init_loops(Body, Aliases, CallStack, Name, InitFirst, InitLast, InitStep, InitOffset),
        check_bounds(UseFirst, UseLast, UseStep, UseOffset, InitFirst, InitLast, InitStep, InitOffset)
    ),
    writef("no matching init loop: ", []),
    print_loop('used', Name, UseFirst, UseLast, UseOffset, UseStep),
    writef("found init loops:\n", []),
    forall(
        init_loops(Body, Aliases, CallStack, Name, First, Last, Step, Offset),
        (
            (
                (check_luna18(UseFirst, UseStep, UseOffset, First, Step, Offset) -> writef("[LUNA18] ", []); true),
                (check_luna19(UseFirst, UseStep, UseOffset, First, Step, Offset) -> writef("[LUNA19] ", []); true),
                (check_luna20(UseLast, UseStep, UseOffset, Last, Step, Offset) -> writef("[LUNA20] ", []); true),
                (check_luna21(UseLast, UseStep, UseOffset, Last, Step, Offset) -> writef("[LUNA21] ", []); true),
                (check_luna22_(UseStep, Step) -> writef("[LUNA22] ", []); true)
            ),
            print_loop('initialized', Name, First, Last, Offset, Step)
        )
    )
    .