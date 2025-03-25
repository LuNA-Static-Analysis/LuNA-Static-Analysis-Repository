import re
import json
import sys
from pathlib import Path
from typing import Any, TextIO, Final

import click

TEXT_INFO_FILE_NAME: Final[str] = 'preprocessed.fa.ti'


class TextInfo:
    def __init__(self, project_dir: Path, text_info_json: dict) -> None:
        self._project_dir: Path = project_dir
        self._text_info: dict = text_info_json
        self._source_lines: dict[Path, list[str]] = {}

    def file_name(self, position: int) -> str:
        fid, _, _, _ = self._text_info['text'][position]
        path: str = self._text_info['paths'][str(fid)]

        return re.sub(r'(\./)+', './', path)

    def line_number(self, position: int) -> int:
        _, ln, _, _ = self._text_info['text'][position]
        if ln is None:
            ln = 0
        return ln + 1

    def code_line(self, line_number: int) -> str:
        position = 0
        while self.line_number(position) < line_number:
            position += 1
        path = self._project_dir / self.file_name(position)
        line_index = line_number - 1

        if path not in self._source_lines:
            with path.open(mode='rt') as f:
                self._source_lines[path] = f.readlines()

        line = self._source_lines[path][line_index].strip().strip('{;').strip().strip('@')
        return line


def get_call_stack_entry(call: dict[str, Any], text_info: TextInfo) -> str:
    assert isinstance(call, dict), str(type(call))
    return f'  File "{call["file"]}", line {call["line"]}, in {call["name"]}\n' \
           f'    {text_info.code_line(int(call["line"]))}\n'


def get_call_stack(callstack: list[dict[str, Any]], text_info: TextInfo) -> str:
    result = ''
    for call in callstack:
        result += get_call_stack_entry(call, text_info)
    return result


def get_all_callstacks(
        callstacks: list[list[dict[str, Any]]],
        text_info: TextInfo,
        separator: str = '\n'
) -> str:
    return separator.join(map(lambda it: get_call_stack(it, text_info), callstacks))


def get_cf(cf: dict[str, Any]) -> str:
    return f'Name: {cf["name"]}, type: {cf["type"]}, file: {cf["file"]}, line: {cf["line"]}\n'


def get_all_cfs(cfs: list[dict[str, Any]]) -> str:
    result = ''
    for cf in cfs:
        result += get_cf(cf)
    return result


def get_df(df: dict[str, Any], text_info: TextInfo, include_name: bool = True) -> str:
    result = f'Name: {df["name"]}\n' if include_name else ''
    separator = '\nAlso in:\n'

    if declared := df.get('declared'):
        result += f'Declared:\n{get_all_callstacks(declared, text_info, separator=separator)}\n'
    if initialized := df.get('initialized'):
        result += f'Initialized:\n{get_all_callstacks(initialized, text_info, separator=separator)}\n'
    if used := df.get('used'):
        result += f'Used:\n{get_all_callstacks(used, text_info, separator=separator)}\n'

    return result


def get_all_dfs(dfs: list[dict[str, Any]], text_info: TextInfo) -> str:
    result = ''
    for df in dfs:
        result += get_df(df, text_info)
    return result


def get_for(for_: dict[str, Any], text_info: TextInfo) -> str:
    return (
            f'Loop for {for_["var"]} from {for_["first"]} to {for_["last"]}\n'
            + f'in:\n{get_call_stack(for_["where"], text_info)}'
    )


def get_df_ref(
        df_ref: dict[str, Any],
        text_info: TextInfo,
        include_declared: bool = False
) -> str:
    df = df_ref['df']
    name_str = f'{df_ref["true"]} as {df_ref["local"]}' if df_ref['true'] != df_ref['local'] else df_ref['local']

    conditions = get_conditions(df_ref["conditions"])
    conditions_str = f' {conditions}' if conditions else ''

    result = f'DF {name_str}{conditions_str} in:\n{get_call_stack(df_ref["where"], text_info)}'

    if include_declared:
        result += f'\nNote: {df["name"]} declared in:\n{get_call_stack(df["declared"][0], text_info)}'

    return result


def get_index_range(
        index_range: dict[str, Any],
        text_info: TextInfo,
        include_declared: bool = False
) -> str:
    df_ref: dict[str, Any] = index_range['df']
    df: dict[str, Any] = df_ref['df']
    loop: dict[str, Any] = index_range['loop']

    result = (
            f'{get_df_ref(df_ref, text_info)}\n'
            + f'  from {index_range["true_lower"]} to {index_range["true_upper"]} with step {index_range["step"]}'
            + f' (with {loop["var"]} from {loop["first"]} to {loop["last"]},'
            + f' step {index_range["step"]} and offset {index_range["offset"]})\n'
    )

    if include_declared:
        result += f'\nNote: {df["name"]} declared in:\n{get_call_stack(df["declared"][0], text_info)}'

    return result


def get_df_ref_or_index_range(
        ref_or_index_range: dict[str, Any],
        text_info: TextInfo,
        include_declared: bool = False
) -> str:
    if 'loop' in ref_or_index_range:
        return get_index_range(ref_or_index_range, text_info, include_declared)

    if {'df', 'true', 'local', 'where'} <= ref_or_index_range.keys():
        return get_df_ref(ref_or_index_range, text_info, include_declared)

    raise NotImplementedError(f'Not a \'df_ref\' or \'index_range\': {ref_or_index_range}')


def get_conditions(conditions: list[str]) -> str:
    if not conditions:
        return ''

    if 1 == len(conditions):
        return f'when {conditions[0]} is true'

    return 'when ' + ', '.join(conditions[:-1]) + f' and {conditions[-1]} are true'

def get_typed_id(
        typed_id: dict[str, Any],
        text_info: TextInfo
) -> str:
    return f'Name: {typed_id["name"]}\nType: {typed_id["type"]}\nAt: {get_call_stack_entry(typed_id["call_stack_entry"], text_info)}'

REF_SEPARATOR: Final[str] = '\nAlso here:\n'


def report_error(
        output_file: TextIO,
        templates_map: dict[str, str],
        text_info: TextInfo,
        error: dict[str, Any]
) -> None:
    error_code: str = error['error_code']
    match error_code.upper():
        case 'SYN1':#done, tested
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$call_stack",
                         get_call_stack(error["details"]["call_stack"], text_info))
                .replace("$cf", get_cf(error["details"]["cf"]))
            )
        case 'SYN2':#done, tested
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$cf_name", error["details"]["call_stack_entry"]["name"])
                .replace("$call_stack_entry",
                         get_call_stack_entry(error["details"]["call_stack_entry"], text_info))
            )
        case 'SYN3':#done, tested
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$cf_name", error["details"]["cf"]["name"])
                .replace("$call_stack_entry",
                         get_call_stack_entry(error["details"]["call_stack_entry"], text_info))
                .replace("$cf", get_cf(error["details"]["cf"]))
            )
        case 'SYN4':#done, not workable
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$call_stack_entry",
                         get_call_stack_entry(error["details"]["call_stack_entry"], text_info))
            )
        case 'SYN5.1':#todo
            output_file.write(
                (templates_map[error_code] + "\n")
            )
        case 'SYN5.2':#done, tested
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$typed_id",
                         get_typed_id(error["details"]["typed_id"], text_info))
            )

        case 'SYN5.3':#done, tested
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace('$df', get_df(error['details']['df'], text_info, include_name=True))
            )

        case 'SYN5.4' | 'SYN5.5':#done, tested
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$cf", get_cf(error["details"]["cf"]))
            )

        case 'SYN5.6':#done, tested
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$typed_id",
                         get_typed_id(error["details"]["typed_id"], text_info))
            )

        case 'SYN5.7' | 'SYN5.8':#done, tested
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$typed_id",
                         get_typed_id(error["details"]["typed_id"], text_info))
            )

        case 'SYN6.1' | 'SYN6.2':#done, tested
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$cfs", get_all_cfs(error["details"]["cfs"]))
            )

            
        case 'SYN7':#done
            output_file.write(
                (templates_map[error_code] + "\n\n"))
            
        case 'SYN8.1' | 'SYN8.2' | 'SYN8.3' | 'SYN8.4':#done, tested
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$id_name", error["details"]["id_name"])
                .replace("$call_stack_entry",
                         get_call_stack_entry(error["details"]["call_stack_entry"], text_info))
            )

        case 'SYN9':#done, tested
            output_file.write(
                (templates_map[error_code] + '\n')
                .replace('$df_name', error['details']['df']['name'])
                .replace('$df', get_df(error['details']['df'], text_info, include_name=False))
            )

        case 'SYN10':#done, not workable
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$cf", get_cf(error["details"]["cf"]))
            )

        case 'SYN11':#done, tested
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$expr", str(error["details"]["expression"]))
                .replace("$callstack", get_call_stack(error["details"]["call_stack"], text_info))
            )

        case 'SYN12':#done, not workable
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$cf", get_cf(error["details"]["cf"]))
            )



        case 'SEM1':#done
            output_file.write(
                (templates_map[error_code] + '\n')
                .replace("$cf", get_cf(error["details"]["cf"]))
                .replace('$df', get_df(error['details']['df'], text_info, include_name=False))
            )

        case 'SEM2.1':#done
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$df_true", error["details"]["initialized"]["true"])
                .replace("$initialized", get_df_ref(error["details"]["initialized"], text_info, include_declared=True))
                .replace(
                    "$other_initializations",
                    REF_SEPARATOR.join(
                        get_df_ref_or_index_range(it, text_info) for it in error["details"]["other_initializations"])
                )
            )

        case 'SEM2.2':#done
            output_file.write(
                (templates_map[error_code] + '\n')
                .replace('$df_name', error['details']['ranges'][0]['df']['df']['name'])
                .replace(
                    '$initialization_loop1',
                    get_index_range(error['details']['ranges'][0], text_info, include_declared=True)
                )
                .replace('$initialization_loop2', get_index_range(error['details']['ranges'][1], text_info))
            )

        case 'SEM3.1':#done
            output_file.write(
                (templates_map[error_code] + '\n')
                .replace('$df_true', error['details']['used']['true'])
                .replace('$used', get_df_ref(error['details']['used'], text_info, include_declared=True))
                .replace(
                    '$initialized',
                    REF_SEPARATOR.join(
                        get_df_ref_or_index_range(it, text_info) for it in error['details']['initialized']
                    )
                )
            )
        
        case 'SEM3.2':#todo
            output_file.write(
                (templates_map[error_code] + '\n')
            )

        case 'SEM3.3':#done
            output_file.write(
                (templates_map[error_code] + '\n')
                .replace('$df_name', error['details']['used']['df']['df']['name'])
                .replace('$used', get_index_range(error['details']['used'], text_info, include_declared=True))
                .replace(
                    '$initialized',
                    REF_SEPARATOR.join(
                        get_df_ref_or_index_range(it, text_info) for it in error['details']['initialized']
                    )
                )
            )
        
        case 'SEM3.4':#todo
            output_file.write(
                (templates_map[error_code] + '\n')
            )

        case 'SEM3.5':#todo
            output_file.write(
                (templates_map[error_code] + '\n')
            )

        case 'SEM3.6':#todo
            output_file.write(
                (templates_map[error_code] + '\n')
            )

        case 'SEM4':#done
            output_file.write(
                (templates_map[error_code] + '\n')
                .replace('$df_name', error['details']['df']['name'])
                .replace('$df', get_df(error['details']['df'], text_info, include_name=False))
            )

        case 'SEM5' | 'SEM6':#done
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$bool", str(error["details"]["type"]))
                .replace("$expr", str(error["details"]["condition"]))
                .replace("$call_stack_entry", get_call_stack_entry(error["details"]["where"], text_info))
            )

        case 'SEM7':#done
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$index", str(error["details"]["arg_index"]))
                .replace("$expr", str(error["details"]["bad_expr"]))
                .replace("$call_stack_entry", get_call_stack_entry(error["details"]["where"], text_info))
            )

        case 'SEM8':#done
            output_file.write(
                (templates_map[error_code] + '\n')
                .replace('$callstack', get_call_stack(error['details']['call_stack'], text_info))
            )

        case 'SEM9':#todo
            output_file.write(
                (templates_map[error_code] + '\n')
            )
        
        case 'SEM10':#todo
            output_file.write(
                (templates_map[error_code] + '\n')
            )

        case 'SEM11':#todo
            output_file.write(
                (templates_map[error_code] + '\n')
            )
        
        case _:
            print(f'INTERNAL ERROR: error code "{error_code}" is not supported')


def report_errors(
        output_file: TextIO,
        templates_map: dict,
        text_info: TextInfo,
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
        report_error(output_file, templates_map, text_info, error)


@click.command()
@click.argument(
    'errors-file',
    required=False,
    type=click.Path(
        exists=True,
        dir_okay=False,
        resolve_path=True,
        path_type=Path
    )
)
@click.option(
    '--project-dir',
    required=True,
    help='LuNA project directory.',
    type=click.Path(
        exists=True,
        file_okay=False,
        resolve_path=True,
        path_type=Path,
    )
)
@click.option(
    '--build-dir',
    required=False,
    show_default=True,
    help='LuNA project build directory.',
    type=Path,
    default=Path('bin')
)
@click.option(
    '-o', '--output',
    required=False,
    help='Output file. If not specified, write to standard output.',
    default=None,
    type=click.Path(
        path_type=Path,
        resolve_path=True
    )
)
def main(
        errors_file: Path,
        project_dir: Path,
        build_dir: Path,
        output: Path
) -> None:
    with open('report_templates.json', mode='rt') as templates_file:
        templates_map = json.load(templates_file)

    with errors_file.open(mode='rt') as errors_json:
        error_list = json.load(errors_json)

    with (build_dir / TEXT_INFO_FILE_NAME).open(mode='rt', encoding='utf-8') as text_info_file:
        text_info = TextInfo(project_dir, json.load(text_info_file))

    report_errors(output or sys.stdout, templates_map, text_info, error_list)


if __name__ == '__main__':
    main(sys.argv[1:])
