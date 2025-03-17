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


def get_callstack_entry(call: dict[str, Any], text_info: TextInfo) -> str:
    assert isinstance(call, dict), str(type(call))
    return f'  File "{call["file"]}", line {call["line"]}, in {call["name"]}\n' \
           f'    {text_info.code_line(int(call["line"]))}\n'


def get_callstack(callstack: list[dict[str, Any]], text_info: TextInfo) -> str:
    result = ''
    for call in callstack:
        result += get_callstack_entry(call, text_info)
    return result


def get_all_callstacks(
        callstacks: list[list[dict[str, Any]]],
        text_info: TextInfo,
        separator: str = '\n'
) -> str:
    return separator.join(map(lambda it: get_callstack(it, text_info), callstacks))


def get_cf(cf: dict[str, Any]) -> str:
    return f'Name: {cf["name"]}, type: {cf["type"]}, file "{cf["file"]}", line {cf["line"]}\n'


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
            + f'in:\n{get_callstack(for_["where"], text_info)}'
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

    result = f'DF {name_str}{conditions_str} in:\n{get_callstack(df_ref["where"], text_info)}'

    if include_declared:
        result += f'\nNote: {df["name"]} declared in:\n{get_callstack(df["declared"][0], text_info)}'

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
        result += f'\nNote: {df["name"]} declared in:\n{get_callstack(df["declared"][0], text_info)}'

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


REF_SEPARATOR: Final[str] = '\nAlso here:\n'


def report_error(
        output_file: TextIO,
        templates_map: dict[str, str],
        text_info: TextInfo,
        error: dict[str, Any]
) -> None:
    error_code: str = error['error_code']
    match error_code.upper():
        case 'LUNA1' | 'LUNA01':
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$cf_name", error["details"]["cf"]["name"])
                .replace("$callstack_entry", get_cf(error["details"]["cf"]))
                .replace("$cf", get_cf(error["details"]["cf"]))
            )
        case 'SYN2':
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$cf_name", error["details"]["call_stack_entry"]["name"])
                .replace("$callstack_entry",
                         get_callstack_entry(error["details"]["call_stack_entry"], text_info))
            )
        case 'SEM2.1':
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
        case 'LUNA3' | 'LUNA03':
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$df_name", error["details"]["df"]["name"])
                .replace("$decl_callstacks",
                         get_all_callstacks(error["details"]["df"]["declared"], text_info))
                .replace("$uses_callstacks",
                         get_all_callstacks(error["details"]["df"]["used"], text_info))
                .replace("$defs_callstacks",
                         get_all_callstacks(error["details"]["df"]["initialized"], text_info))
            )
        case 'SYN1':
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$callstack_entry",
                         get_callstack_entry(error["details"]["call_stack_entry"], text_info))
                .replace("$cf", get_cf(error["details"]["cf"]))
            )
        case 'LUNA5' | 'LUNA05':
            output_file.write(
                (templates_map[error_code] + '\n')
                .replace('$df_name', error['details']['df']['name'])
                .replace('$df', get_df(error['details']['df'], text_info, include_name=False))
            )
        case 'SYN3':
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$cf_name", error["details"]["cf"]["name"])
                .replace("$callstack_entry", get_cf(error["details"]["cf"]))
                .replace("$cf", get_cf(error["details"]["cf"]))
            )
        case 'LUNA7' | 'LUNA07':
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$decl_callstacks",
                         get_all_callstacks(error["details"]["df"]["declared"], text_info))
            )
        case 'LUNA9' | 'LUNA09':
            output_file.write((templates_map[error_code] + "\n\n"))
        case 'SEM4':
            output_file.write(
                (templates_map[error_code] + '\n')
                .replace('$df_name', error['details']['df']['name'])
                .replace('$df', get_df(error['details']['df'], text_info, include_name=False))
            )
        case 'SYN6.1' | 'SYN6.2':
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$cfs", get_all_cfs(error["details"]["cfs"]))
            )
        case 'SYN7':
            output_file.write(
                (templates_map[error_code] + "\n\n"))
        case 'SYN8':
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$dfs", get_all_dfs(error["details"]["dfs"], text_info))
            )
        case 'SYN9':
            output_file.write(
                (templates_map[error_code] + '\n')
                .replace('$df_name', error['details']['df']['name'])
                .replace('$df', get_df(error['details']['df'], text_info, include_name=False))
            )
        case 'LUNA15':  # TODO
            output_file.write((templates_map[error_code] + "\n\n"))
        case 'LUNA16':
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$cf_name", error["details"]["cfs"][0]["name"])
                .replace("$cfs", get_all_cfs(error["details"]["cfs"]))
            )
        case 'LUNA17':
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$cf", get_cf(error["details"]["cf"]))
            )
        case 'SEM5':
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$bool", str(error["details"]["type"]))
                .replace("$expr", str(error["details"]["condition"]))
                .replace("$callstack_entry", get_callstack_entry(error["details"]["where"], text_info))
            )
        case 'LUNA24':
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$bool", str(error["details"]["type"]))
                .replace("$expr", str(error["details"]["condition"]))
                .replace("$callstack_entry", get_callstack_entry(error["details"]["where"], text_info))
            )
        case 'SEM7':
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$index", str(error["details"]["arg_index"]))
                .replace("$expr", str(error["details"]["bad_expr"]))
                .replace("$callstack_entry", get_callstack_entry(error["details"]["where"], text_info))
            )
        case 'LUNA26':
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$expr", str(error["details"]["expression"]))
                .replace("$cf", get_cf(error["details"]["cf"]))
                .replace("$callstack", get_callstack(error["details"]["callstack"], text_info))
            )
        case 'LUNA27':  # TODO
            output_file.write((templates_map[error_code] + "\n\n"))
        case 'LUNA28':  # TODO
            output_file.write((templates_map[error_code] + "\n\n"))
        case 'LUNA29':  # TODO
            output_file.write((templates_map[error_code] + "\n\n"))
        case 'LUNA30':  # TODO
            output_file.write((templates_map[error_code] + "\n\n"))
        case 'LUNA31':  # TODO
            output_file.write((templates_map[error_code] + "\n\n"))
        case 'LUNA32':  # TODO
            output_file.write((templates_map[error_code] + "\n\n"))
        case 'LUNA33':  # TODO
            output_file.write((templates_map[error_code] + "\n\n"))
        case 'SEM8':
            output_file.write(
                (templates_map[error_code] + '\n')
                .replace('$callstack', get_callstack_entry(error['details']['call_stack_entry'], text_info))
            )
        case 'SEM2.2':
            output_file.write(
                (templates_map[error_code] + '\n')
                .replace('$df_name', error['details']['ranges'][0]['df']['df']['name'])
                .replace(
                    '$initialization_loop1',
                    get_index_range(error['details']['ranges'][0], text_info, include_declared=True)
                )
                .replace('$initialization_loop2', get_index_range(error['details']['ranges'][1], text_info))
            )
        case 'LUNA36':
            output_file.write(
                (templates_map[error_code] + "\n")
                .replace("$expr", str(error["details"]["expression"]))
                .replace("$callstack", get_callstack(error["details"]["callstack"], text_info))
            )
        case 'LUNA38' | 'LUNA39':
            output_file.write(
                (templates_map[error_code] + '\n')
                .replace('$callstack', get_callstack(error['details']['for']['where'], text_info))
                .replace('$var', error['details']['for']['var'])
                .replace('$first', error['details']['for']['first'])
                .replace('$last', error['details']['for']['last'])
            )
        case 'SEM3.1':
            output_file.write(
                templates_map[error_code]
                .replace('$df_true', error['details']['used']['true'])
                .replace('$used', get_df_ref(error['details']['used'], text_info, include_declared=True))
                .replace(
                    '$initialized',
                    REF_SEPARATOR.join(
                        get_df_ref_or_index_range(it, text_info) for it in error['details']['initialized']
                    )
                )
            )
            output_file.write('\n')
        case 'SEM3.3':
            output_file.write(
                templates_map[error_code]
                .replace('$df_name', error['details']['used']['df']['df']['name'])
                .replace('$used', get_index_range(error['details']['used'], text_info, include_declared=True))
                .replace(
                    '$initialized',
                    REF_SEPARATOR.join(
                        get_df_ref_or_index_range(it, text_info) for it in error['details']['initialized']
                    )
                )
            )
            output_file.write('\n')
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
