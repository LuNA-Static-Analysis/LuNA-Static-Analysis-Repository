:- use_module(library(http/json)).

:- use_module('src/pro/checks/ranges.pro', [
    index_range_not_initialized_error_json/1,
    index_not_initialized_error_json/1,
    index_range_overlap_error_json/1,
    index_overlap_error_json/1
]).
:- use_module('src/pro/checks/conditions.pro', [
    bool_used_as_number_error_json/1,
    tautology_error_json/1
]).

print_usage :-
    writef("Usage: swipl -t main src/pro/run.pro -- FACTS_FILE_NAME OUTPUT_FILE_NAME IGNORED_ERROR_CODES..."),
    nl.

parse_args(FactsFileName, OutputFileName, IngnoredErrorCodes) :-
    current_prolog_flag(argv, Argv),
%    writef("Argv=%t\n", [Argv]),
    (   [--, FactsFileName, OutputFileName | IngnoredErrorCodes] = Argv
    *-> true
    ;   print_usage,
        halt(1)
    ).

load_program_facts(FactsFileName) :-
    (   exists_source(FactsFileName),
        consult(FactsFileName)
    *-> true
    ;   string_chars(FactsFileName, FactsFileNameChars),
        writef("Failed to consult %s\n", [FactsFileNameChars]),
        halt(1)
    ).

write_errors_json(OutputFileName, IngnoredErrorCodes) :-
    (   member('SEM5', IngnoredErrorCodes)
    ->  Errors1 = []
    ;   findall(Error, tautology_error_json(Error), Errors1)
    ),
    (   member('SEM7', IngnoredErrorCodes)
    ->  Errors2 = []
    ;   findall(Error, bool_used_as_number_error_json(Error), Errors2)
    ),
    (   member('SEM2.2', IngnoredErrorCodes)
    ->  Errors3 = []
    ;   findall(Error, index_range_overlap_error_json(Error), Errors3)
    ),
    (   member('SEM2.1', IngnoredErrorCodes)
    ->  Errors4 = []
    ;   findall(Error, index_overlap_error_json(Error), Errors4)
    ),
    (   member('SEM3.3', IngnoredErrorCodes)
    ->  Errors5 = []
    ;   findall(Error, index_range_not_initialized_error_json(Error), Errors5)
    ),
    (   member('SEM3.1', IngnoredErrorCodes)
    ->  Errors6 = []
    ;   findall(Error, index_not_initialized_error_json(Error), Errors6)
    ),
    append([Errors1, Errors2, Errors3, Errors4, Errors5, Errors6], Errors),
    open(OutputFileName, write, Out, [create([default])]),
    json_write_dict(Out, Errors),
    close(Out).

main :-
    parse_args(FactsFileName, OutputFileName, IngnoredErrorCodes),
    load_program_facts(FactsFileName),
    catch(
        write_errors_json(OutputFileName, IngnoredErrorCodes),
        E,
        (   print_message(error, E),
            halt(1)
        )
    ),
    halt.