import json
import re
import shutil
import subprocess
import sys
from pathlib import Path
from typing import Final

import error_codes
from adapt_function_names import ASSERT_NAME_C, ASSERT_NAME_LUNA
from file_paths import HYBRID_DEBUGGER_HOME
from internal_formatting import format_call_stack, format_call_stack_item
from path_utility import resolve_relative_to


class TextInfo:
    def __init__(self, text_info_json: dict) -> None:
        self._text_info: dict = text_info_json

    def file_name(self, position: int) -> str:
        fid, _, _, _ = self._text_info['text'][position]
        path: str = self._text_info['paths'][str(fid)]

        return re.sub(r'(\./)+', './', path)

    def line_number(self, position: int) -> int:
        _, ln, _, _ = self._text_info['text'][position]
        return ln + 1

    def file_and_line(self, position: int) -> tuple[str, int]:
        return self.file_name(position), self.line_number(position)


def ja_indexed_expression_to_str(expr: dict) -> str:
    assert 'id' == expr['type']

    ref = expr['ref']
    name, indices = ref[0], ref[1:]
    if not indices:
        return name

    return f'{name}[{", ".join(map(ja_expression_to_str, indices))}]'


JA_UNARY_OPERATORS: Final[set[str]] = {'neg', }
JA_BINARY_OPERATORS: Final[set[str]] = {
    '+', '-', '*', '/',
    '>', '>=', '<', '<=', '==', '!=',
    '&&', '||'
}


def ja_arith_expression_to_str(expr: dict) -> str:
    match expr['type']:
        case 'iconst':
            return str(expr['value'])
        case op if op in JA_BINARY_OPERATORS:
            lhs, rhs = map(ja_expression_to_str, expr['operands'])
            return f'({lhs} {op} {rhs})'
        case op if op in JA_UNARY_OPERATORS:
            operand = ja_expression_to_str(expr['operands'][0])
            return f'{op}{operand}'
        case unsupported_type:
            raise NotImplementedError(unsupported_type)


def ja_expression_to_str(expr: dict) -> str:
    match expr['type']:
        case 'id':
            return ja_indexed_expression_to_str(expr)
        case 'iconst':
            return ja_arith_expression_to_str(expr)
        case 'icast':
            return f'int({ja_expression_to_str(expr["expr"])})'
        case op if op in JA_BINARY_OPERATORS or op in JA_UNARY_OPERATORS:
            return ja_arith_expression_to_str(expr)
        case unsupported_type:
            raise NotImplementedError(unsupported_type)


def print_statement(statement: dict, indent: int = 0) -> None:
    print(f'{" " * indent}{statement["type"]}')

    indent += 1
    padding = ' ' * indent

    match statement['type']:
        case 'dfs':
            print(f'{padding}names=[{", ".join(statement["names"])}]')
        case 'exec':
            print(f'{padding}code={statement["code"]}')
            print(f'{padding}args=[{", ".join(map(ja_expression_to_str, statement["args"]))}]')
        case 'for':
            print(f'{padding}var={statement["var"]}')
            print(f'{padding}first={ja_expression_to_str(statement["first"])}')
            print(f'{padding}last={ja_expression_to_str(statement["last"])}')

            for body_statement in statement['body']:
                print_statement(body_statement, indent + 1)
        case _:
            assert False, 'unknown statement'


def find_simple_init_loops(sub: dict, program: dict) -> dict[str, list[tuple[list, dict, dict]]]:
    """
    :return: {DF_NAME: [(BODY_CONTAINING_FOR, FOR, INIT_STATEMENT), ...], ...}
    """
    result: dict[str, list[tuple[list, dict, dict]]] = {}

    for statement in sub['body']:
        if statement['type'] != 'for':
            continue

        for_var_name = statement['var']

        for body_statement in statement['body']:
            # TODO handle initializations in while loops
            if body_statement['type'] != 'exec':
                continue

            code_fragment_name = body_statement['code']
            code_fragment = program[code_fragment_name]

            for formal, actual in zip(code_fragment['args'], body_statement['args']):
                if formal['type'] != 'name':
                    continue

                if len(actual['ref']) != 2:
                    continue

                base_name, index = actual['ref']
                if index['ref'] != [for_var_name]:
                    continue

                result[base_name] = result.get(base_name, []) + [(sub['body'], statement, body_statement)]

    return result


def refs_in_operands(expr: dict) -> list[list]:
    operands = expr.get('operands', [])
    result = []

    for operand in operands:
        if ref := operand.get('ref'):
            result.append(ref)

        result += refs_in_operands(operand)

    return result


def find_simple_use_loops(sub: dict, program: dict) -> dict[str, list[tuple[list, dict, dict]]]:
    """
    :return: {DF_NAME: [(BODY_CONTAINING_FOR, FOR, USE_STATEMENT), ...], ...}
    """
    result: dict[str, list[tuple[list, dict, dict]]] = {}

    for statement in sub['body']:
        if statement['type'] != 'for':
            continue

        for_var_name = statement['var']

        for body_statement in statement['body']:
            # TODO handle consumptions in loop bounds, conditions, etc
            # TODO handle consumptions in subexpressions
            if body_statement['type'] != 'exec':
                continue

            code_fragment_name = body_statement['code']
            code_fragment = program[code_fragment_name]

            for formal, actual in zip(code_fragment['args'], body_statement['args']):
                if formal['type'] == 'name':
                    continue

                if 'operands' in actual:
                    refs = refs_in_operands(actual)
                elif 'ref' in actual:
                    refs = [actual['ref']]
                else:
                    refs = []

                for ref in refs:
                    if len(ref) != 2:
                        continue

                    base_name, index = ref
                    if index['ref'] != [for_var_name]:
                        continue

                    result[base_name] = result.get(base_name, []) + [(sub['body'], statement, body_statement)]

    return result


def find_conditional_single_inits(sub: dict, program: dict) -> dict[str, list[tuple[list, dict, dict]]]:
    result: dict[str, list[tuple[list, dict, dict]]] = {}

    for statement in sub['body']:
        if statement['type'] != 'if':
            continue

        for body_statement in statement['body']:
            # TODO handle initializations in while loops
            if body_statement['type'] != 'exec':
                continue

            code_fragment_name = body_statement['code']
            code_fragment = program[code_fragment_name]

            for formal, actual in zip(code_fragment['args'], body_statement['args']):
                if formal['type'] != 'name':
                    continue

                ref = ja_expression_to_str(actual)
                result[ref] = result.get(ref, []) + [(sub['body'], statement, body_statement)]

    return result


def find_single_uses(sub: dict, program: dict) -> dict[str, list[tuple[list, dict]]]:
    result: dict[str, list[tuple[list, dict]]] = {}

    for statement in sub['body']:
        if statement['type'] == 'exec':
            code_fragment_name = statement['code']
            code_fragment = program[code_fragment_name]

            for formal, actual in zip(code_fragment['args'], statement['args']):
                if formal['type'] == 'name':
                    continue

                if actual['type'] != 'id':
                    continue

                ref = ja_expression_to_str(actual)
                result[ref] = result.get(ref, []) + [(sub['body'], statement)]

    return result


def is_error_loop_in_main(error_loop: dict) -> bool:
    error_loop_where = error_loop['where']

    if len(error_loop_where) != 2:
        return False

    if error_loop_where[0]['name'] != 'main' or error_loop_where[1]['name'] != 'for':
        return False

    return True


# FIXME also match init/use statement since one loop can contain multiple
def find_matching_program_loop(
        error_loop: dict,
        name: str,
        program_loops: dict[str, list[tuple[list, dict, dict]]],
        ti: TextInfo
) -> tuple[list, dict, dict] | None:
    error_loop_where = error_loop['where']
    error_loop_line = error_loop_where[1]['line']

    for body, loop, init_or_use_statement in program_loops[name]:
        line = ti.line_number(loop['begin'])
        if line != error_loop_line:
            continue

        return body, loop, init_or_use_statement


def generate_luna_string_literal(value: str) -> dict:
    return {
        'is_expr': True,
        'type': 'sconst',
        'value': value
    }


def generate_luna_icast(expr: dict) -> dict:
    return {
        'is_expr': True,
        'type': 'icast',
        'expr': expr
    }


def generate_luna_plus(*args: dict) -> dict:
    assert len(args) >= 1

    match args:
        case [e1]:
            return e1
        case [e1, *rest] if rest:
            return {
                'is_expr': True,
                'op_pos': 0,
                'operands': [e1, generate_luna_plus(*rest)],
                'type': '+'
            }
        case _:
            raise NotImplementedError()


# TODO handle init/use loops in subs - pass loop bounds as parameters if needed
# TODO handle single uses
# TODO handle init/use conditions
# TODO eliminate assertions that can be resolved statically (i.e. N == N)
def generate_loop_assertions(
        ja: dict,
        ti: TextInfo,
        found_errors: list[dict] | None = None,
) -> tuple[int, dict]:
    assertions_count = 0

    main_position = ja['main']['begin']
    main_file = ti.file_name(main_position)
    main_line = ti.line_number(main_position)

    ja_with_assertions = ja
    ja_with_assertions[ASSERT_NAME_LUNA] = {
        'args': [
            {
                'type': 'int'
            },
            {
                'type': 'string'
            }
        ],
        'code': ASSERT_NAME_C,
        'code_type': 'default',
        'rules': [],
        'type': 'extern'
    }

    init_loops: dict[str, list[tuple[list, dict, dict]]] = find_simple_init_loops(ja['main'], ja)
    use_loops: dict[str, list[tuple[list, dict, dict]]] = find_simple_use_loops(ja['main'], ja)

    # Filter use loops to only include those mentioned in SEM3.3 errors
    if found_errors is not None:
        sem3_3_errors = [error for error in found_errors if error['error_code'].lower() == 'sem3.3']
        use_loops2 = {}
        init_loops2 = {}
        # TODO conditions are ignored
        # TODO multiple/non-loop initializations are ignored
        # TODO report ignored errors?
        for sem3_3_error in sem3_3_errors:
            initialized = sem3_3_error['details']['initialized']
            used = sem3_3_error['details']['used']
            name = used['df']['df']['name']

            if name not in init_loops or name not in use_loops:
                continue

            if len(initialized) != 1 or 'loop' not in initialized[0]:
                continue

            error_init_loop = initialized[0]['loop']
            if not is_error_loop_in_main(error_init_loop):
                continue

            if (program_init_loop := find_matching_program_loop(error_init_loop, name, init_loops, ti)) is not None:
                init_loops2[name] = init_loops2.get(name, []) + [program_init_loop]

            error_use_loop = used['loop']
            if not is_error_loop_in_main(error_use_loop):
                continue

            if (program_use_loop := find_matching_program_loop(error_use_loop, name, use_loops, ti)) is not None:
                use_loops2[name] = use_loops2.get(name, []) + [program_use_loop]

        use_loops = use_loops2
        del use_loops2
        init_loops = init_loops2
        del init_loops2

    for base_name, name_use_loops in use_loops.items():
        assert len(name_use_loops) > 0
        assert len(init_loops[base_name]) > 0
        assert len(init_loops[base_name]) == 1, f'{base_name} has {len(init_loops[base_name])} init loops'
        _, init_loop, init_statement = init_loops[base_name][0]

        init_loop_position = init_loop['begin']
        init_statement_position = init_statement['begin']
        init_statement_code = init_statement['code']

        init_first = init_loop['first']
        init_last = init_loop['last']

        init_where = format_call_stack([
            (main_file, main_line, 'main'),
            (ti.file_name(init_loop_position), ti.line_number(init_loop_position), 'for'),
            (ti.file_name(init_statement_position), ti.line_number(init_statement_position), init_statement_code),
        ])

        for body, use_loop, use_statement in name_use_loops:
            use_loop_position = use_loop['begin']
            use_statement_position = use_statement['begin']
            use_statement_code = use_statement['code']

            use_where = format_call_stack([
                (main_file, main_line, 'main'),
                (ti.file_name(use_loop_position), ti.line_number(use_loop_position), 'for'),
                (ti.file_name(use_statement_position), ti.line_number(use_statement_position), use_statement_code),
            ])

            use_first = use_loop['first']
            use_last = use_loop['last']

            # TODO might not be the case?
            declared = format_call_stack_item(main_file, main_line, 'main')

            init_loop_var = init_loop['var']
            init_name = f'{base_name}[{init_loop_var}]'
            use_loop_var = use_loop['var']
            use_name = f'{base_name}[{use_loop_var}]'

            error_message = '\n'.join([
                f'error_code={error_codes.ERROR_LUNA_LOOP_BOUNDS_MISMATCH}',
                f'init_name={init_name}',
                f'use_name={use_name}',
                f'declared={declared}',
                f'init_where={init_where}',
                f'use_where={use_where}',
                f'init_loop_var={init_loop_var}',
                f'init_first={ja_expression_to_str(init_first)}',
                f'init_last={ja_expression_to_str(init_last)}',
                f'use_loop_var={use_loop_var}',
                f'use_first={ja_expression_to_str(use_first)}',
                f'use_last={ja_expression_to_str(use_last)}',
            ])

            bounds_check_ja = {
                'is_expr': True,
                'type': '&&',
                'operands': [
                    {
                        'is_expr': True,
                        'type': '<=',
                        'operands': [init_first, use_first],
                        'op_pos': 0
                    },
                    {
                        'is_expr': True,
                        'type': '<=',
                        'operands': [use_last, init_last],
                        'op_pos': 0
                    },
                ],
                'op_pos': 0
            }

            asserts_ja = [
                {
                    'type': 'exec',
                    'code': ASSERT_NAME_LUNA,
                    'id': ['_l0'],
                    'args': [
                        bounds_check_ja,
                        generate_luna_plus(
                            generate_luna_string_literal(error_message),
                            generate_luna_string_literal('\ninit_first_value='), generate_luna_icast(init_first),
                            generate_luna_string_literal('\ninit_last_value='), generate_luna_icast(init_last),
                            generate_luna_string_literal('\nuse_first_value='), generate_luna_icast(use_first),
                            generate_luna_string_literal('\nuse_last_value='), generate_luna_icast(use_last),
                        )
                    ],
                    'rules': [],
                    'begin': 0
                },
            ]
            for assert_ja in asserts_ja:
                body.append(assert_ja)
                assertions_count += 1

    return assertions_count, ja_with_assertions


def generate_init_condition_assertions(ja: dict, ti: TextInfo) -> tuple[int, dict]:
    assertions_count = 0

    main_position = ja['main']['begin']
    main_file = ti.file_name(main_position)
    main_line = ti.line_number(main_position)

    ja_with_assertions = ja
    ja_with_assertions[ASSERT_NAME_LUNA] = {
        'args': [
            {
                'type': 'int'
            },
            {
                'type': 'string'
            }
        ],
        'code': ASSERT_NAME_C,
        'code_type': 'default',
        'rules': [],
        'type': 'extern'
    }

    conditional_single_inits = find_conditional_single_inits(ja['main'], ja)
    single_uses = find_single_uses(ja['main'], ja)

    for name, uses in single_uses.items():
        if name not in conditional_single_inits:
            continue

        assert len(conditional_single_inits[name]) <= 1
        _, single_init_if, single_init = conditional_single_inits[name][0]
        assert single_init_if['type'] == 'if'

        if_position = single_init_if['begin']
        init_condition = single_init_if['cond']

        init_position = single_init['begin']
        init_code = single_init['code']

        for body, single_use in uses:
            use_position = single_use['begin']
            use_code = single_use['code']

            init_where = format_call_stack([
                (main_file, main_line, 'main'),
                (ti.file_name(if_position), ti.line_number(if_position), 'if'),
                (ti.file_name(init_position), ti.line_number(init_position), init_code),
            ])

            use_where = format_call_stack([
                (main_file, main_line, 'main'),
                (ti.file_name(use_position), ti.line_number(use_position), use_code),
            ])

            # TODO might not be the case?
            declared = format_call_stack_item(main_file, main_line, 'main')

            error_message = '\n'.join([
                f'error_code={error_codes.ERROR_LUNA_INIT_CONDITION_FALSE}',
                f'name={name}',
                f'init_where={init_where}',
                f'init_condition={ja_expression_to_str(init_condition)}',
                f'use_where={use_where}',
                f'declared={declared}',
            ])

            asserts_ja = [
                {
                    'type': 'exec',
                    'code': ASSERT_NAME_LUNA,
                    'id': ['_l0'],
                    'args': [
                        init_condition,
                        {
                            'is_expr': True,
                            'type': 'sconst',
                            'value': error_message
                        }
                    ],
                    'rules': [],
                    'begin': 0
                },
            ]
            for assert_ja in asserts_ja:
                body.append(assert_ja)
                assertions_count += 1

    return assertions_count, ja_with_assertions


def instrument_luna(
        fa_path: Path,
        fa_args: list[str],
        build_dir: Path,
        output_dir: Path,
        found_errors_path: Path | None = None,
) -> None:
    luna_ja_only_path = resolve_relative_to(Path('luna_ja_only', 'luna'), root=HYBRID_DEBUGGER_HOME).resolve()

    project_path = fa_path.parent

    try:
        subprocess.run(
            args=[
                'python3',
                str(luna_ja_only_path),
                '-q',
                '--compile-only',
                f'--build-dir={str(build_dir)}',
                fa_path,
                *fa_args
            ],
            # cwd=project_path.resolve(),
            capture_output=True,
            check=True
        )
    except subprocess.CalledProcessError as e:
        print(e.stderr.decode(), file=sys.stderr)
        sys.exit(1)

    found_errors = None
    if found_errors_path is not None:
        print(f'Attempting to use "{found_errors_path}" to only generate necessary checks')
        try:
            found_errors = json.loads(found_errors_path.read_text(encoding='utf-8'))
        except FileNotFoundError:
            print(f'Could not find "{found_errors_path}"')

    text_info = TextInfo(json.loads(build_dir.joinpath('preprocessed.fa.ti').read_text(encoding='utf-8')))

    loop_assertions_count, ja_with_assertions = generate_loop_assertions(
        ja=json.loads(build_dir.joinpath('program_foreign.ja').read_text(encoding='utf-8')),
        ti=text_info,
        found_errors=found_errors,
    )
    condition_assertions_count, ja_with_assertions = generate_init_condition_assertions(
        ja=ja_with_assertions,
        ti=text_info
    )

    print(f'Generated:')
    print(f'    {loop_assertions_count} loop bounds assertions')
    print(f'    {condition_assertions_count} init condition assertions')

    dst_ja_path = build_dir / 'program_foreign.ja'
    dst_ja_path.rename(build_dir.joinpath('program_foreign.bak.ja'))
    dst_ja_path.write_text(json.dumps(ja_with_assertions, indent=2), encoding='utf-8')
    print(f'Instrumented code written to "{dst_ja_path}"')

    fa_paths = [it.resolve() for it in project_path.iterdir() if it.suffix == '.fa']
    for path in fa_paths:
        shutil.copyfile(src=path, dst=output_dir / path.name)
