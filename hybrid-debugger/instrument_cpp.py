from __future__ import annotations

import dataclasses
import json
import re
from collections.abc import Callable
from dataclasses import dataclass, field
from enum import StrEnum
from pathlib import Path
from typing import Final, TypeAlias

from clang import cindex

import error_codes
from file_paths import C_HASH_HEADER_PATH, C_ASSERT_HEADER_PATH
from adapt_function_names import HASH_NAME_C, ASSERT_NAME_C
from internal_formatting import format_call_stack_item

cindex.Config.set_library_file('/usr/lib/llvm-18/lib/libclang.so')  # adjust path


@dataclass(frozen=True)
class Edit:
    position: int
    text: str

    @staticmethod
    def from_lines(position: int, lines: list[str], indent: int | str) -> Edit:
        if type(indent) is int:
            indent = ' ' * indent

        return Edit(position=position, text=f'\n{indent}'.join(['', *lines]) + '\n')


class Pragma:
    pass


class PragmaName(StrEnum):
    USER_TYPE = 'user_type'
    SERIALIZED_SIZE = 'serialized_size'
    SERIALIZE = 'serialize'
    DESERIALIZE = 'deserialize'


@dataclass(frozen=True)
class PragmaUserType(Pragma):
    argument_name: str
    type_name: str


@dataclass(frozen=True)
class PragmaSerializedSize(Pragma):
    type_name: str


@dataclass(frozen=True)
class PragmaSerialize(Pragma):
    type_name: str


@dataclass(frozen=True)
class PragmaDeserialize(Pragma):
    type_name: str


class UnknownPragma(Exception):
    def __init__(self, pragma_type: str) -> None:
        self.pragma_type = pragma_type

    def __str__(self) -> str:
        return f'Unknown pragma: "{self.pragma_type}"'


class InvalidPragmaSyntax(Exception):
    def __str__(self) -> str:
        return f'Invalid pragma syntax'


class InvalidUserTypePragmaArgumentName(Exception):
    def __init__(self, argument_name: str, function_name: str) -> None:
        self.argument_name = argument_name
        self.function_name = function_name

    def __str__(self) -> str:
        return f'Function "{self.function_name}" has no argument named "{self.argument_name}"'


class UserTypeRedefinition(Exception):
    def __init__(self, argument_name: str, function_name: str) -> None:
        self.argument_name = argument_name
        self.function_name = function_name

    def __str__(self) -> str:
        return f'Function "{self.function_name}" cannot have multiple user types declared for argument "{self.argument_name}"'


class InvalidSignature(Exception):
    def __init__(self, function_name: str, expected_signature: str) -> None:
        self.function_name = function_name
        self.expected_signature = expected_signature

    def __str__(self) -> str:
        return f'Function "{self.function_name}" must have signature {self.expected_signature}'


class MissingPragmaForType(Exception):
    def __init__(self, pragma_name: str, type_name: str) -> None:
        self.pragma_name = pragma_name
        self.type_name = type_name

    def __str__(self) -> str:
        return f'No function annotated with "{self.pragma_name}" for type "{self.type_name}"'


def try_parse_pragma(line: str) -> Pragma | None:
    if not re.match(r'\s*#\s*pragma\s+adapt', line):
        return None

    match = re.match(
        r'\s*#\s*pragma\s+adapt\s+(\w+)\s*\(\s*([^)]+)\s*\)',
        line
    )

    if not match:
        raise InvalidPragmaSyntax()

    pragma_type = match.group(1)
    pragma_arguments = [it.strip() for it in match.group(2).strip().split(',')]

    match pragma_type, pragma_arguments:
        case PragmaName.USER_TYPE, [argument_name, type_name]:
            return PragmaUserType(argument_name=argument_name, type_name=type_name)
        case PragmaName.SERIALIZED_SIZE, [type_name]:
            return PragmaSerializedSize(type_name=type_name)
        case PragmaName.SERIALIZE, [type_name]:
            return PragmaSerialize(type_name=type_name)
        case PragmaName.DESERIALIZE, [type_name]:
            return PragmaDeserialize(type_name=type_name)
        case _:
            raise UnknownPragma(pragma_type)


def find_pragmas_before_line(lines: list[str], line_number: int) -> list[Pragma]:
    result = []

    line_index = line_number - 2

    while line_index >= 0:
        pragma = try_parse_pragma(lines[line_index])
        if not pragma:
            break

        result.insert(0, pragma)
        line_index -= 1

    return result


@dataclass(frozen=True)
class FunctionArgument:
    name: str
    type: str

    @staticmethod
    def from_cursor(cursor: cindex.Cursor) -> FunctionArgument:
        return FunctionArgument(
            name=cursor.spelling,
            type=get_declared_type(cursor)
        )


@dataclass(frozen=True)
class Statement:
    extent: cindex.SourceRange


@dataclass(frozen=True, unsafe_hash=True)
class Function:
    name: str
    arguments: tuple[FunctionArgument, ...]
    return_type: str
    is_extern_c: bool

    location: cindex.SourceLocation = field(hash=False)

    body: Statement | None = field(hash=False)

    @staticmethod
    def from_cursor(cursor: cindex.Cursor) -> Function:
        body: Statement | None = None
        for child in cursor.get_children():
            if child.kind != cindex.CursorKind.COMPOUND_STMT:
                continue

            body = Statement(extent=child.extent)
            break

        return Function(
            name=cursor.spelling,
            arguments=tuple(FunctionArgument.from_cursor(arg) for arg in cursor.get_arguments()),
            return_type=cursor.result_type.spelling,
            is_extern_c=is_function_extern_c(cursor),
            location=cursor.location,
            body=body
        )


def input_output_args(function: Function) -> tuple[dict[str, FunctionArgument], dict[str, FunctionArgument]]:
    input_args: dict[str, FunctionArgument] = {
        arg.name: arg
        for arg in function.arguments
        if arg.type == ARGUMENT_TYPE_INPUT_DF
    }
    output_args: dict[str, FunctionArgument] = {
        arg.name: arg
        for arg in function.arguments
        if arg.type == ARGUMENT_TYPE_OUTPUT_DF
    }
    return input_args, output_args


def parse_function_pragmas(source_lines: list[str], function: Function) -> list[Pragma]:
    pragmas = find_pragmas_before_line(lines=source_lines, line_number=function.location.line)

    # Validate
    for pragma in pragmas:
        match pragma:
            case PragmaUserType(argument_name, _):
                if not any(arg.name == argument_name for arg in function.arguments):
                    raise InvalidUserTypePragmaArgumentName(
                        argument_name=argument_name,
                        function_name=function.name
                    )
                if sum(1 for arg in function.arguments if arg.name == argument_name) > 1:
                    raise InvalidPragmaSyntax()
            case PragmaSerializedSize(type_name):
                if len(function.arguments) == 1 and function.arguments[0].type == type_name:
                    continue

                raise InvalidSignature(function_name=function.name, expected_signature=f'({type_name})')
            case PragmaSerialize(type_name):
                if len(function.arguments) == 2:
                    arg1, arg2 = function.arguments

                    if arg1.type == type_name and arg2.type == 'void *':
                        continue

                raise InvalidSignature(function_name=function.name, expected_signature=f'({type_name}, void *)')
            case PragmaDeserialize(type_name):
                if len(function.arguments) == 2:
                    arg1, arg2 = function.arguments

                    if arg1.type == f'{type_name} &' and arg2.type == 'const void *':
                        continue

                raise InvalidSignature(function_name=function.name, expected_signature=f'({type_name} &, const void *)')
            case _:
                pass

    return pragmas


def generate_signature_cpp(function: Function) -> str:
    return f'{function.return_type} {function.name}({", ".join([f"{argument.type} {argument.name}" for argument in function.arguments])}'')'


def generate_forward_declaration_cpp(function: Function) -> str:
    return f'{generate_signature_cpp(function)};'


ARGUMENT_TYPE_INPUT_DF: Final[str] = 'const InputDF &'
ARGUMENT_TYPE_OUTPUT_DF: Final[str] = 'OutputDF &'


def collect_returns(node, returns):
    for child in node.get_children():
        if child.kind == cindex.CursorKind.RETURN_STMT:
            returns.append(child)
        collect_returns(child, returns)


def is_function_extern_c(cursor: cindex.Cursor) -> bool:
    # TODO In libclang, mangled_name often equals spelling for extern "C"
    if cursor.kind != cindex.CursorKind.FUNCTION_DECL:
        return False

    try:
        mangled = cursor.mangled_name
        if mangled == cursor.spelling:
            return True
    except Exception:
        pass

    return False


def find_translation_unit_functions(translation_unit: cindex.TranslationUnit) -> list[Function]:
    def find_functions(node: cindex.Cursor) -> list[Function]:
        result = []

        if node.kind == cindex.CursorKind.FUNCTION_DECL:
            result.append(Function.from_cursor(cursor=node))

        for child in node.get_children():
            result.extend(find_functions(child))

        return result

    return find_functions(translation_unit.cursor)


def get_declared_type(cursor: cindex.Cursor) -> str:
    tokens = list(cursor.get_tokens())

    # drop argument name (last token in most cases)
    if not tokens:
        return ''

    # heuristic: last token is usually the name
    tokens = tokens[:-1]

    const = False
    ref = ''
    base_tokens = []

    for t in tokens:
        s = t.spelling

        if s == 'const':
            const = True
        elif s in ('&', '&&'):
            ref = s
        else:
            base_tokens.append(s)

    base = ' '.join(base_tokens).strip()

    # canonical form
    result = ''
    if const:
        result += 'const '
    result += base
    if ref:
        result += ' ' + ref

    return ' '.join(result.split())


def generate_hash_call_cpp(argument_name: str) -> str:
    return f'{HASH_NAME_C}({argument_name})'


def generate_hash_variable_name_cpp(argument_name: str) -> str:
    return f'__adapt_hash_{argument_name}'


def generate_hash_initialization_cpp(argument_name: str) -> str:
    return f'auto const {generate_hash_variable_name_cpp(argument_name)} = {generate_hash_call_cpp(argument_name)};'


def generate_hash_check_cpp(
        function: str,
        file: str,
        line: int,
        argument_name: str
) -> str:
    error_message_cpp = 'std::string("'
    error_message_cpp += '\\n'.join([
        f'error_code={error_codes.ERROR_CPP_INPUT_ARGUMENT_CHANGED}',
        f'cpp_where={format_call_stack_item(file, line, function)}',
        f'cpp_argument_name={argument_name}',
        f'message=input argument changed',
    ])
    error_message_cpp += '")'
    error_message_cpp = f'({error_message_cpp}).c_str()'

    return ''.join([
        f'{ASSERT_NAME_C}({generate_hash_call_cpp(argument_name)}=={generate_hash_variable_name_cpp(argument_name)}, ',
        error_message_cpp,
        ');'
    ])


def apply_edits(source: str, edits: list[Edit]) -> str:
    def key(e: Edit) -> int:
        return e.position

    for edit in sorted(edits, key=key, reverse=True):
        source = source[:edit.position] + edit.text + source[edit.position:]

    return source


# TODO also save init call stack?
def generate_user_type_assignment_cpp(argument_name: str, type_name: str) -> str:
    return f'{argument_name}.dbg_set_user_type("{type_name}");'


def generate_user_type_check_cpp(
        function: str,
        file: str,
        line: int,
        argument_name: str,
        type_name: str) -> str:
    error_message_cpp = 'std::string("'
    error_message_cpp += '\\n'.join([
        f'error_code={error_codes.ERROR_CPP_USER_TYPE_MISMATCH}',
        f'cpp_where={format_call_stack_item(file, line, function)}',
        f'cpp_argument_name={argument_name}',
        f'expected_user_type={type_name}',
        f'message=wrong or missing user type',
    ])
    error_message_cpp += '")'
    error_message_cpp += f' + "\\nluna_df_id=" + {argument_name}.dbg_id_str()'
    error_message_cpp += f' + "\\nluna_call_stack=" + {argument_name}.dbg_call_stack_str()'
    error_message_cpp += f' + "\\ngot_user_type=" + {argument_name}.dbg_user_type_str()'
    error_message_cpp += f' + "\\ngot_luna_type=" + {argument_name}.dbg_luna_type_str()'
    error_message_cpp = f'({error_message_cpp}).c_str()'

    return ''.join([
        f'{ASSERT_NAME_C}(',
        f'{argument_name}.dbg_user_type_str() == "{type_name}", ',
        error_message_cpp,
        ');'
    ])


def generate_copy_of_function_cpp(function: Function, source: str) -> str:
    return (f'{generate_signature_cpp(function)} '
            + source[function.body.extent.start.offset:function.body.extent.end.offset])


def generate_deserialization_check_cpp(
        function: str,
        file: str,
        line: int,
        serialized_argument_name: str,
        serialized_argument_type: str,
        buffer_argument_name: str,
        serialized_size_function_name: str,
        serialize_function_name: str,
        deserialize_function_name: str,
) -> list[str]:
    size_variable_name = '_size'
    buffer_variable_name = '_buf'
    deserialized_argument_name = '_deserialized'

    # TODO re-calculate and compare buffer size
    error_message_cpp = '"' + '\\n'.join([
        f'error_code={error_codes.ERROR_CPP_SERIALIZED_BUFFER_MISMATCH}',
        f'cpp_where={format_call_stack_item(file, line, function)}',
        f'cpp_type={serialized_argument_type}',
        f'message=serialized buffers differ after deserialization',
    ]) + '"'

    return [
        '{',
        f'    auto {size_variable_name} = {serialized_size_function_name}({serialized_argument_name});',
        f'    {serialized_argument_type} {deserialized_argument_name};',
        f'    {deserialize_function_name}({deserialized_argument_name}, {buffer_argument_name});',
        f'    std::vector<uint8_t> {buffer_variable_name}({size_variable_name});',
        f'    {serialize_function_name}({deserialized_argument_name}, {buffer_variable_name}.data());',
        f'    {ASSERT_NAME_C}(0 == memcmp({buffer_variable_name}.data(), {buffer_argument_name}, {size_variable_name}), {error_message_cpp});',
        '}'
    ]


def get_source_indentation(source: str, location: cindex.SourceLocation) -> int:
    indent = 0
    char_index = location.offset - 2
    while char_index >= 0:
        match source[char_index]:
            case ' ':
                indent += 1
            case '\t':
                indent += 4
            case _:
                break

        char_index -= 1

    return indent


def get_pragma(pragmas: list[Pragma], predicate: Callable[[Pragma], bool]) -> Pragma | None:
    return next((pragma for pragma in pragmas if predicate(pragma)), None)


def get_function_with_pragma(
        functions: list[Function],
        pragmas: dict[Function, list[Pragma]],
        predicate: Callable[[Pragma], bool]
) -> Function | None:
    for function in functions:
        if get_pragma(pragmas[function], predicate):
            return function

    return None


def get_user_type(pragmas: list[Pragma], argument_name: str) -> str | None:
    for pragma in pragmas:
        match pragma:
            case PragmaUserType(some_argument_name, type_name):
                if argument_name != some_argument_name:
                    continue

                return type_name
            case _:
                continue

    return None


def find_function_by_name(functions: list[Function], name: str) -> Function:
    for function in functions:
        if function.name == name:
            return function

    raise ValueError(f'Function {name} does not exist')


def find_possible_argument_types(
        function: Function,
        argument_name: str,
        functions: list[Function],
        pragmas: dict[Function, list[Pragma]],
        data_dependencies: dict[str, dict[str, list[ArgumentDataDependency]]],
) -> set[str]:
    if function.name not in data_dependencies or argument_name not in data_dependencies[function.name]:
        return set()

    types = set()

    for other_function_name, other_argument_name in data_dependencies[function.name][argument_name]:
        other_function = find_function_by_name(functions, other_function_name)
        _, output_args = input_output_args(other_function)
        other_argument = output_args[other_argument_name]

        if other_argument_user_type := get_user_type(pragmas[other_function], other_argument.name):
            types.add(other_argument_user_type)

    return types


def is_type_check_redundant(
        function: Function,
        argument_name: str,
        functions: list[Function],
        pragmas: dict[Function, list[Pragma]],
        data_dependencies: dict[str, dict[str, list[ArgumentDataDependency]]],
) -> bool:
    possible_types = find_possible_argument_types(function, argument_name, functions, pragmas, data_dependencies)
    required_type = get_user_type(pragmas[function], argument_name)

    return required_type is not None and {required_type} == possible_types


ArgumentDataDependency: TypeAlias = tuple[str, str]


def load_data_dependencies(path: Path, functions: list[Function]) -> dict[str, dict[str, list[ArgumentDataDependency]]]:
    result: dict[str, dict[str, list[ArgumentDataDependency]]] = {}

    for function_name, data_dependencies in json.loads(path.read_text(encoding='utf-8')).items():
        result[function_name] = {}

        function = find_function_by_name(functions, function_name)

        for argument_index_str, argument_data_dependencies in data_dependencies.items():
            argument_index = int(argument_index_str)
            argument_name = function.arguments[argument_index].name

            result[function_name][argument_name] = []

            for other_function_name, other_argument_index_str in argument_data_dependencies:
                other_function = find_function_by_name(functions, other_function_name)

                other_argument_index = int(other_argument_index_str)
                other_argument_name = other_function.arguments[other_argument_index].name

                result[function_name][argument_name].append((other_function_name, other_argument_name))

    return result


def instrument_cpp(
        cpp_path: Path,
        output_dir: Path,
        data_dependencies_file_path: Path,
) -> None:
    index = cindex.Index.create()
    translation_unit = index.parse(cpp_path, args=[
        '-std=c++11'
    ])

    source = cpp_path.read_text(encoding='utf-8')
    source_lines = source.splitlines()

    functions = [
        function
        for function in find_translation_unit_functions(translation_unit)
        if str(function.location.file) == str(cpp_path)
           and function.body is not None
    ]

    if data_dependencies_file_path is not None and data_dependencies_file_path.exists():
        data_dependencies = load_data_dependencies(data_dependencies_file_path, functions)
    else:
        data_dependencies = {}

    edits = [
        Edit(position=0, text=f'#include <cstdint>\n'),
        Edit(position=0, text=f'#include <cstddef>\n'),
        Edit(position=0, text=f'#include <vector>\n'),
        Edit(position=0, text=f'#include "{C_HASH_HEADER_PATH.name}"\n'),
        Edit(position=0, text=f'#include "{C_ASSERT_HEADER_PATH.name}"\n'),
    ]

    type_checks_count = 0
    redundant_type_checks_count = 0
    type_assignments_count = 0
    immutability_checks_count = 0
    hash_initializations_count = 0
    deserialization_checks_count = 0

    pragmas: dict[Function, list[Pragma]] = {
        function: parse_function_pragmas(source_lines, function)
        for function in functions
    }

    for function in functions:
        closing_bracket_indent = get_source_indentation(source, function.body.extent.end)
        generated_indent = 4 + closing_bracket_indent

        # file = Path(str(function.location.file)).name
        file = str(function.location.file)
        line = function.location.line

        body_start = function.body.extent.start.offset
        body_end = function.body.extent.end.offset - closing_bracket_indent

        input_args, output_args = input_output_args(function)

        # Unconditional hash checks
        hash_initializations = []
        hash_checks = []
        for argument_name in input_args:
            hash_initializations.append(generate_hash_initialization_cpp(argument_name))
            hash_checks.append(generate_hash_check_cpp(
                file=file,
                line=function.location.line,
                function=function.name,
                argument_name=argument_name
            ))

        immutability_checks_count += len(hash_checks)
        hash_initializations_count += len(hash_initializations)

        if hash_initializations:
            edits.append(Edit.from_lines(position=body_start + 1, lines=hash_initializations, indent=generated_indent))
        if hash_checks:
            edits.append(Edit.from_lines(position=body_end - 1, lines=hash_checks, indent=generated_indent))

        # Pragma-controlled code generation
        type_checks = []
        type_assignments = []

        for pragma in pragmas[function]:
            match pragma:
                # User type assertions
                case PragmaUserType(argument_name, type_name):
                    if argument_name in input_args:
                        if is_type_check_redundant(
                                function,
                                argument_name=argument_name,
                                functions=functions,
                                pragmas=pragmas,
                                data_dependencies=data_dependencies,
                        ):
                            redundant_type_checks_count += 1
                            continue

                        type_checks.append(generate_user_type_check_cpp(
                            file=file,
                            line=function.location.line,
                            function=function.name,
                            argument_name=argument_name,
                            type_name=type_name
                        ))
                    if argument_name in output_args:
                        type_assignments.append(generate_user_type_assignment_cpp(
                            argument_name=argument_name,
                            type_name=type_name
                        ))

                # Serialization assertions
                case PragmaSerialize(type_name):
                    serialized_size_function = get_function_with_pragma(
                        functions, pragmas,
                        lambda p: isinstance(p, PragmaSerializedSize) and p.type_name == type_name
                    )
                    if not serialized_size_function:
                        raise MissingPragmaForType(pragma_name=PragmaName.SERIALIZED_SIZE, type_name=type_name)

                    deserialize_function = get_function_with_pragma(
                        functions, pragmas,
                        lambda p: isinstance(p, PragmaDeserialize) and p.type_name == type_name
                    )
                    if not deserialize_function:
                        raise MissingPragmaForType(pragma_name=PragmaName.DESERIALIZE, type_name=type_name)

                    serialize_function_nonrec_name = f'_adapt_{function.name}_nonrec'
                    serialize_function_nonrec = dataclasses.replace(function, name=serialize_function_nonrec_name)

                    edits.append(Edit.from_lines(
                        position=body_end - 1,
                        lines=generate_deserialization_check_cpp(
                            file=file,
                            line=line,
                            function=function.name,
                            serialized_argument_name=function.arguments[0].name,
                            serialized_argument_type=function.arguments[0].type,
                            buffer_argument_name=function.arguments[1].name,
                            serialized_size_function_name=serialized_size_function.name,
                            serialize_function_name=serialize_function_nonrec_name,
                            deserialize_function_name=deserialize_function.name,
                        ),
                        indent=generated_indent
                    ))

                    edits.append(Edit.from_lines(
                        position=body_end - 1,
                        lines=[
                            generate_forward_declaration_cpp(serialize_function_nonrec),
                            generate_forward_declaration_cpp(serialized_size_function),
                            generate_forward_declaration_cpp(deserialize_function)
                        ],
                        indent=generated_indent
                    ))

                    edits.append(Edit(
                        position=body_end,
                        text=f'\n{generate_copy_of_function_cpp(serialize_function_nonrec, source)}'
                    ))

                    deserialization_checks_count += 1

        type_checks_count += len(type_checks)
        type_assignments_count += len(type_assignments)

        if type_checks:
            edits.append(Edit.from_lines(position=body_start + 1, lines=type_checks, indent=generated_indent))
        if type_assignments:
            edits.append(Edit.from_lines(position=body_start + 1, lines=type_assignments, indent=generated_indent))

    source = apply_edits(source, edits)

    output_dir.mkdir(parents=True, exist_ok=True)

    dst_cpp_path = output_dir / cpp_path.name
    dst_cpp_path.write_text(source, encoding='utf-8')

    print(f'Generated:')
    print(f'    {type_checks_count} user type checks (skipped {redundant_type_checks_count} redundant checks)')
    print(f'    {immutability_checks_count} immutability checks')
    print(f'    {deserialization_checks_count} deserialization checks')
    print(f'Instrumented code written to "{dst_cpp_path}"')
