import json
import sys
from typing import Any, TextIO


def get_callstack_entry(call: dict[str, Any]) -> str:
    return f'  File "{call["file"]}", line {call["line"]}, in {call["name"]}\n'


def get_callstack(callstack: list[dict[str, Any]]) -> str:
    result = ''
    for call in callstack:
        result += get_callstack_entry(call)
    return result


def get_all_callstacks(callstacks: list[list[dict[str, Any]]]) -> str:
    result = ''
    for callstack in callstacks:
        result += get_callstack(callstack) + "\n"
    return result


def get_cf(cf: dict[str, Any]) -> str:
    return f'Name: {cf["name"]}, type: {cf["type"]}, file "{cf["file"]}", line {cf["line"]}\n'


def get_all_cfs(cfs: list[dict[str, Any]]) -> str:
    result = ''
    for cf in cfs:
        result += get_cf(cf)
    return result


def get_df(df: dict[str, Any]) -> str:
    result = f'Name: {df["name"]}\n'

    if declared := df.get('declared'):
        result += f'Declared:\n{get_all_callstacks(declared)}\n'
    if initialized := df.get('initialized'):
        result += f'Initialized:\n{get_all_callstacks(initialized)}\n'
    if used := df.get('used'):
        result += f'Used:\n{get_all_callstacks(used)}\n'

    return result


def get_all_dfs(dfs: list[dict[str, Any]]) -> str:
    result = ''
    for df in dfs:
        result += get_df(df)
    return result


def get_for(for_: dict[str, Any]) -> str:
    return (
            f'Loop for {for_["var"]} from {for_["first"]} to {for_["last"]}\n'
            + f'in\n{get_callstack(for_["where"])}'
    )


def get_df_ref(df_ref: dict[str, Any]) -> str:
    return (
            f'DF:\n{get_df(df_ref["df"])}\n'
            + f'{df_ref["true"]} as {df_ref["local"]}\n'
            + f'in\n{get_callstack(df_ref["where"])}'
    )


def get_index_range(index_range: dict[str, Any]) -> str:
    df_ref: dict[str, Any] = index_range['df']
    df: dict[str, Any] = df_ref['df']
    loop: dict[str, Any] = index_range['loop']

    return (
            f'DF {df_ref["true"]} as {df_ref["local"]} in\n{get_callstack(df_ref["where"])}'
            + f'With {loop["var"]} from {loop["first"]} to {loop["last"]},'
            + f' step {index_range["step"]} and offset {index_range["offset"]}\n'
            + f'Note: {df["name"]} declared in\n{get_callstack(df["declared"])}\n'
    )


def get_conditions(conditions: list[str]) -> str:
    if not conditions:
        return ''

    if 1 == len(conditions):
        return f'when {conditions[0]} is true'

    return 'when ' + ', '.join(conditions[:-1]) + f' and {conditions[-1]} are true'


def report_error(
        output_file: TextIO,
        templates_map: dict[str, str],
        error: dict[str, Any]
) -> None:
    error_code = error['error_code'][4:6]  # getting an error number
    match int(error_code):  # react according to what error it is exactly
        case 1:
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$cf_name", error["details"]["cf"]["name"])
                .replace("$callstack_entry", get_cf(error["details"]["cf"]))
                .replace("$cf", get_cf(error["details"]["cf"]))
            )
        case 2:
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$cf_name", error["details"]["call_stack_entry"]["name"])
                .replace("$callstack_entry",
                         get_callstack_entry(error["details"]["call_stack_entry"]))
            )
        case 3:
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$df_name", error["details"]["df"]["name"])
                .replace("$decl_callstacks",
                         get_all_callstacks(error["details"]["df"]["declared"]))
                .replace("$uses_callstacks",
                         get_all_callstacks(error["details"]["df"]["used"]))
                .replace("$defs_callstacks",
                         get_all_callstacks(error["details"]["df"]["initialized"]))
            )
        case 4:
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$callstack_entry",
                         get_callstack_entry(error["details"]["call_stack_entry"]))
                .replace("$cf", get_cf(error["details"]["cf"]))
            )
        case 5:
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$df", get_df(error['details']['df']))
            )
        case 6:
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$cf_name", error["details"]["cf"]["name"])
                .replace("$callstack_entry", get_cf(error["details"]["cf"]))
                .replace("$cf", get_cf(error["details"]["cf"]))
            )
        case 7:
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$decl_callstacks",
                         get_all_callstacks(error["details"]["df"]["declared"]))
            )
        case 8:
            pass  # no such error exists
        case 9:  # TODO
            output_file.write((templates_map[error_code] + "\n\n"))
        case 10:
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$df_name", error["details"]["df"]["name"])
                .replace("$decl_callstacks",
                         get_all_callstacks(error["details"]["df"]["declared"]))
                .replace("$uses_callstacks",
                         get_all_callstacks(error["details"]["df"]["used"]))
                .replace("$defs_callstacks",
                         get_all_callstacks(error["details"]["df"]["initialized"]))
            )
        case 11:
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$cfs", get_all_cfs(error["details"]["cfs"]))
            )
        case 12:
            output_file.write(
                (templates_map[error_code] + "\n\n"))
        case 13:
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$dfs", get_all_dfs(error["details"]["dfs"]))
            )
        case 14:
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$df", get_df(error['details']['df']))
            )
        case 15:  # TODO
            output_file.write(
                (templates_map[error_code] + "\n\n")

            )
        case 16:
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$cf_name", error["details"]["cfs"][0]["name"])
                .replace("$cfs", get_all_cfs(error["details"]["cfs"]))
            )
        case 17:
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$cf", get_cf(error["details"]["cf"]))
            )
        case n if n in [18, 19, 20, 21, 22]:
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$consumption_loop", get_index_range(error['details']['used']))
                .replace("$initialization_loop", get_index_range(error['details']['initialized']))
            )
        case 23:
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$bool", str(error["details"]["type"]))
                .replace("$expr", str(error["details"]["condition"]))
                .replace("$callstack_entry", get_callstack_entry(error["details"]["where"]))
            )
        case 24:
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$bool", str(error["details"]["type"]))
                .replace("$expr", str(error["details"]["condition"]))
                .replace("$callstack_entry", get_callstack_entry(error["details"]["where"]))
            )
        case 25:
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$index", str(error["details"]["arg_index"]))
                .replace("$expr", str(error["details"]["bad_expr"]))
                .replace("$callstack_entry", get_callstack_entry(error["details"]["where"]))
            )
        case 26:
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$expr", str(error["details"]["expression"]))
                .replace("$cf", get_cf(error["details"]["cf"]))
                .replace("$callstack", get_callstack(error["details"]["callstack"]))
            )
        case 27:  # TODO
            output_file.write((templates_map[error_code] + "\n\n"))
        case 28:  # TODO
            output_file.write((templates_map[error_code] + "\n\n"))
        case 29:  # TODO
            output_file.write((templates_map[error_code] + "\n\n"))
        case 30:  # TODO
            output_file.write((templates_map[error_code] + "\n\n"))
        case 31:  # TODO
            output_file.write((templates_map[error_code] + "\n\n"))
        case 32:  # TODO
            output_file.write((templates_map[error_code] + "\n\n"))
        case 33:  # TODO
            output_file.write((templates_map[error_code] + "\n\n"))
        case 34:  # TODO
            output_file.write((templates_map[error_code] + "\n\n"))
        case 35:
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$initialization_loop1", get_index_range(error['details']['ranges'][0]))
                .replace("$initialization_loop2", get_index_range(error['details']['ranges'][1]))
            )
        case 36:
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$expr", str(error["details"]["expression"]))
                .replace("$callstack", get_callstack(error["details"]["callstack"]))
            )
        case 37:
            use_conditions = get_conditions(error['details'].get('use_conditions', []))
            output_file.write(
                templates_map[error_code]
                .replace('$use_conditions', f' {use_conditions}' if use_conditions else '')
                .replace('$consumption_loop', get_index_range(error['details']['used']))
            )
            # TODO should probably split into two errors since single template does not cover both varianst
            if initialized := error['details'].get('initialized'):
                init_conditions = get_conditions(error['details'].get('init_conditions', []))
                output_file.write(
                    'Initialized$init_conditions:\n$initialization_loop'
                    .replace('$init_conditions', f' {init_conditions}' if init_conditions else '')
                    .replace('$initialization_loop', get_index_range(initialized))
                )
            output_file.write('\n')
        case _:
            print("INTERNAL ERROR: unknown error code encountered")


def report_errors(
        output_file: TextIO,
        templates_map: dict,
        error_list: list[dict[str, Any]]
) -> None:
    if not error_list:
        output_file.write("No errors found.\n")
        return

    output_file.write(f'Found {len(error_list)} errors:\n')

    error_number = 0
    for error in error_list:
        error_number += 1
        output_file.write(f'({error_number}) ')
        report_error(output_file, templates_map, error)


def main(argv: list[str]) -> None:
    errors_file_name = argv[0] if argv else 'found_errors.json'

    with open('report_templates.json', mode='rt') as templates_file:
        templates_map = json.load(templates_file)

    with open(errors_file_name, mode='rt') as json_file:
        error_list = json.load(json_file)

    with open('adapt_output.txt', mode='wt') as output_file:
        report_errors(output_file, templates_map, error_list)


if __name__ == '__main__':
    main(sys.argv[1:])
