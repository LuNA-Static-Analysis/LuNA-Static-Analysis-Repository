from typing import Final

CALL_STACK_SEPARATOR: Final[str] = ','


def format_call_stack_item(file: str, line: int, name: str) -> str:
    return f'{file} {line} {name}'


def parse_call_stack_item(call_stack_item_str: str) -> tuple[str, int, str]:
    file, line_str, name = call_stack_item_str.split(maxsplit=2)
    return file, int(line_str), name


def format_call_stack(call_stack: list[tuple[str, int, str]]) -> str:
    return CALL_STACK_SEPARATOR.join([format_call_stack_item(*it) for it in call_stack])


def parse_call_stack(call_stack_str: str) -> list[tuple[str, int, str]]:
    return [
        parse_call_stack_item(it)
        for it in call_stack_str.split(CALL_STACK_SEPARATOR)
    ]
