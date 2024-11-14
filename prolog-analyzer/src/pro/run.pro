:- use_module(library(http/json)).

:- use_module('src/pro/checks/ranges.pro', [
    index_range_not_initialized_error_json/1,
    index_range_overlap_error_json/1,
    index_overlap_error_json/1
]).
:- use_module('src/pro/checks/conditions.pro', [
    bool_used_as_number_error_json/1,
    tautology_error_json/1
]).

print_usage :-
    writef("Usage: swipl -t main src/pro/run.pro -- <FACTS_FILE_NAME> <OUTPUT_FILE_NAME>"),
    nl.

parse_args(FactsFileName, OutputFileName) :-
    current_prolog_flag(argv, Argv),
%    writef("Argv=%t\n", [Argv]),
    (   [--, FactsFileName, OutputFileName] = Argv
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

write_errors_json(OutputFileName) :-
    findall(Error, tautology_error_json(Error), Errors1),
    findall(Error, bool_used_as_number_error_json(Error), Errors2),
    findall(Error, index_range_overlap_error_json(Error), Errors3),
    findall(Error, index_overlap_error_json(Error), Errors4),
    findall(Error, index_range_not_initialized_error_json(Error), Errors5),
    append([Errors1, Errors2, Errors3, Errors4, Errors5], Errors),
    open(OutputFileName, write, Out, [create([default])]),
    json_write_dict(Out, Errors),
    close(Out).

main :-
    parse_args(FactsFileName, OutputFileName),
    load_program_facts(FactsFileName),
    catch(
        write_errors_json(OutputFileName),
        E,
        (   print_message(error, E),
            halt(1)
        )
    ),
    halt.