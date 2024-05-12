:- use_module(library(http/json)).

:- use_module('src/pro/checks/ranges.pro', [
    index_range_missmatch_error_json/1,
    index_range_not_initialized_error_json/1,
    index_range_overlap_error_json/1
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

call_time_seconds(Goal, TimeSeconds) :-
    call_time(Goal, T),
    _{'wall': TimeSeconds} :< T.

run_checks(Errors) :-
    call_time_seconds(findall(Error, tautology_error_json(Error), Errors1), T1),
    call_time_seconds(findall(Error, bool_used_as_number_error_json(Error), Errors2), T2),
    call_time_seconds(findall(Error, index_range_overlap_error_json(Error), Errors3), T3),
    call_time_seconds(findall(Error, index_range_missmatch_error_json(Error), Errors4), T4),
    call_time_seconds(findall(Error, index_range_not_initialized_error_json(Error), Errors5), T5),

    append([Errors1, Errors2, Errors3, Errors4, Errors5], Errors),

    format("LUNA23: ~3fs", [T1]), nl,
    format("LUNA25: ~3fs", [T2]), nl,
    format("LUNA35: ~3fs", [T3]), nl,
    format("LUNA18-22: ~3fs", [T4]), nl,
    format("LUNA37: ~3fs", [T5]), nl.

write_errors_json(Errors, OutputFileName) :-
    open(OutputFileName, write, Out, [create([default])]),
    json_write_dict(Out, Errors),
    close(Out).

main_ :-
    parse_args(FactsFileName, OutputFileName),
    call_time_seconds(load_program_facts(FactsFileName), LoadTime),
    format("Input: ~3fs", [LoadTime]), nl,
    catch(
        (
            call_time_seconds(run_checks(Errors), ChecksTime),
            format("Checks: ~3fs", [ChecksTime]), nl,
            call_time_seconds(write_errors_json(Errors, OutputFileName), OutputTime),
            format("Output: ~3fs", [OutputTime]), nl
        ),
        E,
        (   print_message(error, E),
            halt(1)
        )
    ).

main :-
    call_time_seconds(main_, MainTime),
    format("Main: ~3fs", [MainTime]), nl.
