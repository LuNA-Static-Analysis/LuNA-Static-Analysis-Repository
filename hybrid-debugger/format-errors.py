import json
from pathlib import Path
from typing import Final, Iterable, Any

import click
from luna_trace.dtypes import Id, CF, CodeMetaInformation
from luna_trace.formatting import call_history as call_stack_readable, full_df_name
from luna_trace.load import load_meta_info, load_cf_info, load_id_map

import error_codes
from internal_formatting import parse_call_stack_item, parse_call_stack

DEFAULT_BUILD_DIR: Final[str] = 'build'

ERROR_FILE_HEADER: Final[str] = '--- ADAPT ERROR BEGIN ---'
ERROR_FILE_FOOTER: Final[str] = '--- ADAPT ERROR END ---'


def parse_luna_df_id(luna_df_id: str) -> Id:
    luna_df_id = luna_df_id.strip().lower()
    luna_df_id = luna_df_id.removeprefix('id<')
    luna_df_id = luna_df_id.removesuffix('>')
    return tuple(int(it.strip()) for it in luna_df_id.split(','))


def parse_luna_call_stack(luna_call_stack: str) -> list[int]:
    return list(map(int, luna_call_stack.split()))


def find_initializers(df_id: Id, cf_info: dict[Id, CF]) -> set[Id]:
    return {
        cf_id for cf_id, info in cf_info.items()
        if df_id in info.output
    }


def strip_code_line_to_name(code_line: str) -> str:
    code_line = code_line.strip()
    tokens = code_line.split()

    if tokens[0] in ['for', 'while', 'if', 'let']:
        return tokens[0]

    if tokens[0] == 'cf':
        code_line = code_line[code_line.find(':') + 1:].strip()
        tokens = code_line.split()

    if tokens[0] == 'sub':
        return tokens[1][:tokens[1].find('(')].strip()

    if '(' in tokens[0]:
        tokens[0] = tokens[0][:tokens[0].find('(')]

    return tokens[0].strip()


def call_stack_readable_to_json(call_stack: list[str]) -> list[dict[str, str | int]]:
    # parsing "code line (maybe with spaces) [file:line]"
    result = []
    for entry in call_stack:
        code_line, position = entry.rsplit(maxsplit=1)
        file, line_str = position.strip('[]').split(':')
        result.append({
            'file': file,
            'line': int(line_str),
            'name': strip_code_line_to_name(code_line),
        })
    return result


def call_stack_to_json(call_stack: Iterable[int], meta: CodeMetaInformation) -> list[dict[str, str | int]]:
    return call_stack_readable_to_json(call_stack_readable(call_stack, meta))


def print_call_stack_readable(call_stack: Iterable[int], meta: CodeMetaInformation) -> None:
    for it in call_stack_readable(call_stack, meta):
        print(f'  in {it}')


def try_load_error_file(path: Path) -> dict[str, str] | None:
    text: str = path.read_text(encoding='utf-8').strip()
    if not text.startswith(ERROR_FILE_HEADER):
        return None
    if not text.endswith(ERROR_FILE_FOOTER):
        return None

    text = text.removeprefix(ERROR_FILE_HEADER).removesuffix(ERROR_FILE_FOOTER).strip()

    def key(line: str) -> str:
        return line.split('=')[0].strip()

    def value(line: str) -> str:
        return line.split('=')[1].strip()

    return {
        key(line): value(line)
        for line in text.splitlines()
    }


def fold_duplicate_errors(errors: list[dict[str, Any]]) -> list[dict[str, Any]]:
    unique_errors = []
    instances: dict[int, int] = {}
    for error in errors:
        is_unique = True
        for i, unique_error in enumerate(unique_errors):
            if error == unique_error:
                instances[i] = instances.get(i, 0) + 1
                is_unique = False
                break

        if not is_unique:
            continue

        instances[len(unique_errors)] = 1
        unique_errors.append(error)

    for i, error in enumerate(unique_errors):
        error['instances'] = instances[i]

    return unique_errors


@click.command()
@click.argument(
    'project-path',
    nargs=1,
    type=click.Path(
        dir_okay=True,
        file_okay=False,
        resolve_path=True,
        path_type=Path,
    )
)
@click.option(
    '--errors-file',
    required=False,
    show_default=True,
    default=None,
    help='Append errors list to existing file or create a new errors file in ADAPT format.',
    type=click.Path(
        dir_okay=False,
        file_okay=True,
        resolve_path=True,
        path_type=Path,
    )
)
def main(
        project_path: Path,
        errors_file: Path | None,
) -> None:
    meta = load_meta_info(build_dir=str(project_path / 'build'))

    cf_info = load_cf_info([str(it) for it in project_path.glob('cf.*.json.list')])
    df_info = load_id_map([str(it) for it in project_path.glob('id.*.map.list')])

    error_file_paths = project_path.glob('adapt_error_*.dat')

    adapt_errors: list[dict] = []

    for path in error_file_paths:
        error = try_load_error_file(path)
        if error is None:
            print(f'Error file {path} in not valid, skipping')
            continue

        error_code = error['error_code']

        match error_code:
            case error_codes.ERROR_LUNA_LOOP_BOUNDS_MISMATCH:
                init_local = error['init_name']
                use_local = error['use_name']
                declared = parse_call_stack(error['declared'])

                init_where = parse_call_stack(error['init_where'])
                use_where = parse_call_stack(error['use_where'])

                init_loop_var = error['init_loop_var']
                init_first = error['init_first']
                init_last = error['init_last']
                use_loop_var = error['use_loop_var']
                use_first = error['use_first']
                use_last = error['use_last']

                init_first_value = error['init_first_value']
                init_last_value = error['init_last_value']
                use_first_value = error['use_first_value']
                use_last_value = error['use_last_value']

                df_name = init_local[:init_local.find('[')]
                df = {
                    'name': df_name,
                    'declared': [[
                        {
                            'file': file,
                            'line': line,
                            'name': name
                        }
                        for file, line, name in declared
                    ]]
                }

                init_df_ref = {
                    'df': df,
                    'true': init_local,
                    'local': init_local,
                    'where': [
                        {
                            'file': file,
                            'line': line,
                            'name': name
                        }
                        for file, line, name in init_where
                    ],
                }

                # TODO assuming step=1 and offset=0
                init_index_range = {
                    'df': init_df_ref,
                    'loop': {
                        'var': init_loop_var,
                        'first': init_first,
                        'last': init_last,
                    },
                    'true_lower': init_first_value,
                    'true_upper': init_last_value,
                    'step': 1,
                    'offset': 0,
                }

                use_df_ref = {
                    'df': df,
                    'true': use_local,
                    'local': use_local,
                    'where': [
                        {
                            'file': file,
                            'line': line,
                            'name': name
                        }
                        for file, line, name in use_where
                    ],
                }

                # TODO assuming step=1 and offset=0
                use_index_range = {
                    'df': use_df_ref,
                    'loop': {
                        'var': use_loop_var,
                        'first': use_first,
                        'last': use_last,
                    },
                    'true_lower': use_first_value,
                    'true_upper': use_last_value,
                    'step': 1,
                    'offset': 0,
                }

                adapt_errors.append({
                    'error_code': error_code,
                    'details': {
                        'initialized': [init_index_range],
                        'used': use_index_range
                    }
                })
            case error_codes.ERROR_LUNA_INIT_CONDITION_FALSE:
                df_local = error['name']
                df_declared = parse_call_stack(error['declared'])
                init_condition = error['init_condition']

                init_where = parse_call_stack(error['init_where'])
                use_where = parse_call_stack(error['use_where'])

                df_name = df_local[:df_local.find('[')]
                df = {
                    'name': df_name,
                    'declared': [[
                        {
                            'file': file,
                            'line': line,
                            'name': name
                        }
                        for file, line, name in df_declared
                    ]]
                }

                # TODO local == true is an assumption that is not guaranteed to hold, same for initialized/used
                adapt_errors.append({
                    'error_code': error_code,
                    'details': {
                        'initialized': [{
                            'df': df,
                            'true': df_local,
                            'local': df_local,
                            'conditions': [init_condition],
                            'where': [
                                {
                                    'file': file,
                                    'line': line,
                                    'name': name
                                }
                                for file, line, name in init_where
                            ],
                        }],
                        'used': {
                            'df': df,
                            'true': df_local,
                            'local': df_local,
                            'where': [
                                {
                                    'file': file,
                                    'line': line,
                                    'name': name
                                }
                                for file, line, name in use_where
                            ],
                        }
                    }
                })
            case error_codes.ERROR_CPP_INPUT_ARGUMENT_CHANGED:
                cpp_argument_name = error['cpp_argument_name']

                cpp_where = error['cpp_where']
                cpp_file, cpp_line, cpp_function_name = parse_call_stack_item(cpp_where)

                adapt_errors.append({
                    'error_code': error_code,
                    'details': {
                        'where': {
                            'file': cpp_file,
                            'line': cpp_line,
                            'name': cpp_function_name,
                        },
                        'argument_name': cpp_argument_name,
                    }
                })
            case error_codes.ERROR_CPP_USER_TYPE_MISMATCH:
                cpp_argument_name = error['cpp_argument_name']

                cpp_where = error['cpp_where']
                cpp_file, cpp_line, cpp_function_name = parse_call_stack_item(cpp_where)

                expected_user_type = error['expected_user_type']
                got_user_type = error['got_user_type']
                got_luna_type = error['got_luna_type']
                got_type = got_user_type if got_user_type else f'{got_luna_type} (built-in)'

                luna_df_id = parse_luna_df_id(error['luna_df_id'])
                df = df_info[luna_df_id[:2]]
                full_name = full_df_name(df.name, luna_df_id[2:])
                initializers = find_initializers(luna_df_id, cf_info)

                luna_call_stack = parse_luna_call_stack(error['luna_call_stack'])

                where_used = [{
                    'file': cpp_file,
                    'line': cpp_line,
                    'name': cpp_function_name,
                }] + call_stack_to_json(luna_call_stack, meta)

                df_ref = {
                    'df': {
                        'name': df.name,
                        'declared': [
                            call_stack_to_json([df.block], meta)
                        ],
                        'initialized': [call_stack_to_json(cf_info[cf_id].history, meta) for cf_id in initializers],
                    },
                    'local': cpp_argument_name,
                    'true': full_name,
                    'where': where_used,
                }

                adapt_errors.append({
                    'error_code': error_code,
                    'details': {
                        'expected_type': expected_user_type,
                        'got_type': got_type,
                        'used': df_ref
                    }
                })
            case error_codes.ERROR_CPP_SERIALIZED_BUFFER_MISMATCH:
                cpp_where = error['cpp_where']
                cpp_file, cpp_line, cpp_function_name = parse_call_stack_item(cpp_where)

                cpp_type = error['cpp_type']

                adapt_errors.append({
                    'error_code': error_code,
                    'details': {
                        'where': {
                            'file': cpp_file,
                            'line': cpp_line,
                            'name': cpp_function_name,
                        },
                        'type': cpp_type,
                    }
                })
            case _:
                raise NotImplementedError(f'Unknown error code: {error_code}')

    adapt_errors = fold_duplicate_errors(adapt_errors)

    if errors_file is not None and errors_file.exists():
        existing_adapt_errors = json.loads(errors_file.read_text(encoding='utf-8'))
        adapt_errors = existing_adapt_errors + adapt_errors

    if errors_file is None:
        errors_file = project_path / 'errors.json'
    errors_file.parent.mkdir(exist_ok=True)

    errors_file.write_text(json.dumps(adapt_errors, indent=2))


if __name__ == '__main__':
    main()
