import re
from pathlib import Path
from typing import Any, Callable, Final, Iterable, NewType, TypeAlias
from enum import Enum
from itertools import chain, count
import click
import json

IntSequence: TypeAlias = Callable[[], int]

Subroutine = NewType('Subroutine', dict[str, Any])
JsonAlgorithm = NewType('JsonAlgorithm', dict[str, Subroutine])
Statement = NewType('Statement', dict[str, Any])

SubroutineArgument = NewType('SubroutineArgument', dict[str, Any])
Expression = NewType('Expression', dict[str, Any])


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
        return ln + 1

    def code_line(self, position: int) -> str:
        path = self._project_dir / self.file_name(position)
        line_index = self.line_number(position) - 1

        if path not in self._source_lines:
            with path.open(mode='rt') as f:
                self._source_lines[path] = f.readlines()

        line = self._source_lines[path][line_index].strip().strip('{;').strip().strip('@')
        return line

    def file_and_line(self, position: int) -> (str, int):
        return self.file_name(position), self.line_number(position)


def int_seq() -> Callable[[], int]:
    iterator = count(start=0, step=1)
    return lambda: next(iterator)


class SubroutineType(str, Enum):
    STRUCT = 'struct'
    """
    Structured code fragment.
    """

    EXTERN = 'extern'
    """
    External code fragment.
    """


class SubroutineArgumentType(str, Enum):
    VALUE = 'value'
    NAME = 'name'


class StatementType(str, Enum):
    EXEC = 'exec'
    """
    Both struct and extern.
    """

    FOR = 'for'

    WHILE = 'while'

    IF = 'if'

    DFS = 'dfs'


class UnsupportedExpressionError(NotImplementedError):
    def __init__(self, pos: int, expr_type: str, *args: Any) -> None:
        self._pos = pos
        self._expr_type = expr_type

        super().__init__(*args)

    @property
    def pos(self) -> int:
        return self._pos

    @property
    def expr_type(self) -> str:
        return self._expr_type


LUNA_SUBROUTINE: Final[str] = 'luna_subroutine'
LUNA_STATEMENT: Final[str] = 'luna_statement'
LUNA_REF: Final[str] = 'luna_ref'

DEFAULT_PREFIX: Final[str] = 'LUNA_'


def repr_name(name: str, prefix: str = DEFAULT_PREFIX) -> str:
    return f'"{prefix + name}"'


def ref_to_expr(ref: list[Any]) -> Expression:
    expr: Any = {
        'ref': ref,
        'type': 'id'
    }
    return expr


def repr_indexed_name(expr: Expression) -> str:
    assert 'id' == expr['type']

    ref = expr['ref']
    name, indices = ref[0], ref[1:]
    if not indices:
        return repr_name(name)

    return f'{LUNA_REF}([' + ', '.join(chain([repr_name(name)], map(repr_expression_tree, indices))) + '])'


UNARY_OPERATORS: Final[set[str]] = {'neg', }
BINARY_OPERATORS: Final[set[str]] = {
    '+', '-', '*', '/',
    '>', '>=', '<', '<=', '==', '!=',
    '&&', '||'
}
OPERATOR_REPRESENTATION: Final[dict[str, str]] = {
    '/': '//',
    'neg': '-',
    # '==': '#=',
    # '!=': '#\\\\=',
    # '>=': '#>=',
    # '<=': '#=<',
    # '>': '#>',
    # '<': '#<',
    # '&&': '#/\\\\',
    # '||': '#\\\\/',
}


def repr_arith_expr(expr: Expression) -> str:
    match expr['type']:
        case 'iconst':
            return str(expr['value'])
        case op if op in BINARY_OPERATORS:
            lhs, rhs = map(repr_expression_tree, expr['operands'])
            op = OPERATOR_REPRESENTATION.get(op, op)

            return f'["{op}", {lhs}, {rhs}]'
        case op if op in UNARY_OPERATORS:
            operand = repr_expression_tree(expr['operands'][0])
            op = OPERATOR_REPRESENTATION.get(op, op)

            return f'["{op}", {operand}]'
        case unsupported_type:
            raise UnsupportedExpressionError(
                expr['begin'],
                unsupported_type,
                f'Unsupported expression type: "{unsupported_type}"'
            )


def repr_expression_tree(expr: Expression) -> str:
    match expr['type']:
        case 'id':
            return repr_indexed_name(expr)
        case 'iconst':
            return repr_arith_expr(expr)
        case op if op in BINARY_OPERATORS or op in UNARY_OPERATORS:
            return repr_arith_expr(expr)
        case unsupported_type:
            raise UnsupportedExpressionError(
                expr.get('begin', -1),
                unsupported_type,
                f'Unsupported expression type: "{unsupported_type}".'
            )


def gen_requests_desc(statement: Statement) -> str:
    request_rules = [
        rule
        for rule in statement.get('rules', [])
        if 'enum' == rule['ruletype'] and 'request' in rule['property']
    ]
    requested_items = list(map(
        ref_to_expr,
        chain.from_iterable(rule['items'] for rule in request_rules)
    ))
    return '[' + ', '.join(map(repr_expression_tree, requested_items)) + ']'


def gen_if_facts(
        next_id: IntSequence,
        text_info: TextInfo,
        statement: Statement
) -> tuple[int, Iterable[str]]:
    assert statement['type'] == StatementType.IF

    self_id = next_id()
    file, line = text_info.file_and_line(statement['begin'])

    cond = repr_expression_tree(statement['cond'])
    body_ids, body_facts = gen_body_facts(next_id, text_info, statement['body'])
    body = gen_body_ids_desc(body_ids)

    self_fact = \
        f'{LUNA_STATEMENT}({self_id}, "{file}", {line}, if, ' \
        f'[{cond}, {body}], {gen_requests_desc(statement)}).'

    return self_id, chain([self_fact], body_facts)


# TODO make all expression have body/condition
def gen_exec_facts(
        next_id: IntSequence,
        text_info: TextInfo,
        statement: Statement
) -> tuple[int, str]:
    assert statement['type'] == StatementType.EXEC

    self_id = next_id()
    file, line = text_info.file_and_line(statement['begin'])
    callee = statement['code']
    args = '[' + ', '.join(map(repr_expression_tree, statement['args'])) + ']'

    return (
        self_id,
        f'{LUNA_STATEMENT}({self_id}, "{file}", {line}, exec, ["{callee}", {args}], {gen_requests_desc(statement)}).'
    )


def gen_for_facts(
        next_id: IntSequence,
        text_info: TextInfo,
        statement: Statement
) -> tuple[int, Iterable[str]]:
    assert statement['type'] == StatementType.FOR

    self_id = next_id()
    file, line = text_info.file_and_line(statement['begin'])
    var = repr_name(statement['var'])
    first = repr_expression_tree(statement['first'])
    last = repr_expression_tree(statement['last'])

    body_ids, body_facts = gen_body_facts(next_id, text_info, statement['body'])
    body = gen_body_ids_desc(body_ids)

    self_fact = \
        f'{LUNA_STATEMENT}({self_id}, "{file}", {line}, for, ' \
        f'[{var}, {first}, {last}, {body}], {gen_requests_desc(statement)}).'

    return self_id, chain([self_fact], body_facts)


def gen_dfs_facts(
        next_id: IntSequence,
        text_info: TextInfo,
        statement: Statement
) -> tuple[int, str]:
    assert statement['type'] == StatementType.DFS

    self_id = next_id()
    pos = statement.get('begin', -1)
    file, line = text_info.file_and_line(pos) if pos > 0 else ('', -1)

    names = '[' + ', '.join(map(repr_name, statement['names'])) + ']'

    return self_id, f'{LUNA_STATEMENT}({self_id}, "{file}", {line}, dfs, [{names}], {gen_requests_desc(statement)}).'


def get_sub_arg_type(arg: SubroutineArgument) -> SubroutineArgumentType:
    return SubroutineArgumentType.NAME if arg['type'] == 'name' else SubroutineArgumentType.VALUE


def gen_sub_arg_desc(sub: Subroutine, arg: SubroutineArgument) -> str:
    arg_desc = f'{get_sub_arg_type(arg).value}'

    if sub['type'] == SubroutineType.STRUCT:
        return f'[{arg_desc}, {repr_name(arg["id"])}]'

    return f'[{arg_desc}]'


def gen_sub_args_desc(sub: Subroutine) -> str:
    return '[' + ','.join(gen_sub_arg_desc(sub, arg) for arg in sub['args']) + ']'


def gen_body_ids_desc(body_ids: Iterable[int]) -> str:
    return '[' + ','.join(map(str, body_ids)) + ']'


def gen_body_facts(
        next_id: IntSequence,
        text_info: TextInfo,
        body: Iterable[Statement]
) -> tuple[Iterable[int], Iterable[str]]:
    body_ids: list[int] = []
    body_facts: list[str] = []

    for statement in body:
        match statement['type']:
            case StatementType.EXEC:
                exec_id, exec_fact = gen_exec_facts(next_id, text_info, statement)

                body_ids.append(exec_id)
                body_facts.append(exec_fact)
            case StatementType.FOR:
                for_id, for_facts = gen_for_facts(next_id, text_info, statement)

                body_ids.append(for_id)
                body_facts.extend(for_facts)
            case StatementType.WHILE:
                raise NotImplementedError()
            case StatementType.IF:
                if_id, if_facts = gen_if_facts(next_id, text_info, statement)

                body_ids.append(if_id)
                body_facts.extend(if_facts)
            case StatementType.DFS:
                dfs_id, dfs_fact = gen_dfs_facts(next_id, text_info, statement)

                body_ids.append(dfs_id)
                body_facts.append(dfs_fact)
            case unknown_type:
                raise NotImplementedError(
                    f'{unknown_type} is not a valid statement type.')

    return body_ids, body_facts


def gen_sub_facts(
        next_id: IntSequence,
        ja: JsonAlgorithm,
        text_info: TextInfo,
        sub_name: str
) -> Iterable[str]:
    sub = ja[sub_name]
    self_id = next_id()
    body_ids, body_facts = gen_body_facts(next_id, text_info, sub.get('body', []))

    pos = sub.get('begin', -1)
    file, line = text_info.file_and_line(pos) if pos > 0 else ('', -1)
    sub_type = sub['type']

    args_desc = gen_sub_args_desc(sub)
    body_ids_desc = gen_body_ids_desc(body_ids)

    return chain(
        [f'{LUNA_SUBROUTINE}({self_id}, "{file}", {line}, "{sub_name}", {sub_type}, {args_desc}, {body_ids_desc}).'],
        body_facts
    )


def ja2pro(ja: JsonAlgorithm, text_info: TextInfo) -> Iterable[str]:
    next_id = int_seq()

    # TODO Use chain.from_iterable
    # Type checker says chain(Iterable[str]) is not Iterable[str], but it is
    result: Any = chain(*(gen_sub_facts(next_id, ja, text_info, name) for name in ja))
    return chain(
        [f':- discontiguous {LUNA_SUBROUTINE}/7.', f':- discontiguous {LUNA_STATEMENT}/6.'],
        result
    )


PROGRAM_FILE_NAME: Final = 'program_recom.ja'
TEXT_INFO_FILE_NAME: Final = 'preprocessed.fa.ti'


@click.command()
@click.option(
    '--project-dir',
    required=True,
    help='LuNA project directory.',
    type=Path
)
@click.option(
    '--build-dir',
    required=False,
    help='LuNA project build directory.',
    type=Path,
    default=Path('bin')
)
@click.option(
    '-o',
    required=False,
    help='Output file.',
    type=Path,
    default=Path('generated') / 'out.pro'
)
def main(
        project_dir: Path,
        build_dir: Path,
        o: Path
) -> None:
    if not build_dir.is_absolute():
        build_dir = project_dir / build_dir

    if not build_dir.exists():
        exit(f'Build directory "{build_dir.absolute()}" does not exist')

    if not build_dir.is_dir():
        exit(f'"{build_dir.absolute()}" is not a directory')

    with (build_dir / PROGRAM_FILE_NAME).open(mode='rt', encoding='utf-8') as program_file:
        program: JsonAlgorithm = json.load(program_file)

    with (build_dir / TEXT_INFO_FILE_NAME).open(mode='rt', encoding='utf-8') as text_info_file:
        text_info = TextInfo(project_dir, json.load(text_info_file))

    o.parent.mkdir(parents=True, exist_ok=True)
    with o.open(mode='wt') as f:
        for fact in ja2pro(program, text_info):
            print(fact, file=f)


if __name__ == '__main__':
    main()
