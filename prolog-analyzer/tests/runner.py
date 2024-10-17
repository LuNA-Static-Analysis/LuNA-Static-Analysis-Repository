from __future__ import annotations

import json
import platform
import re
import subprocess
from dataclasses import dataclass
from pathlib import Path
from subprocess import CompletedProcess, TimeoutExpired, CalledProcessError
from typing import Final, Iterable, Collection, Literal, NewType

import click

Error = NewType('Error', str)

REGEX_SPECIAL_CHARACTERS: Final[str] = '\\.+*?[^]$(){}=!<>|:-'
ESCAPE_CODE_REGEX: Final[re.Pattern] = re.compile(r'\x1b\[[0-9;]*[a-zA-Z]')


def escape_all(s: str, special_characters: Iterable[str]) -> str:
    for char in special_characters:
        s = s.replace(char, rf'\{char}')

    return s


def make_regex_pattern(simple_pattern: str) -> str:
    special_characters = REGEX_SPECIAL_CHARACTERS.replace(WILDCARD, '')
    escaped_pattern = escape_all(simple_pattern, special_characters)
    return escaped_pattern.replace(WILDCARD, r'.*')


WILDCARD: Final[str] = '*'
WILDCARD_PREFIX: Final[str] = 'wc:'


def parse_pattern(pattern: str) -> re.Pattern:
    # wc == wildcard (i.e. glob syntax)
    if pattern.startswith(WILDCARD_PREFIX):
        return re.compile(make_regex_pattern(pattern.removeprefix(WILDCARD_PREFIX)))

    return re.compile(pattern)


@dataclass(frozen=True)
class Command:
    command: str
    timeout: int | None
    platform: str | None
    can_fail: bool | None

    @staticmethod
    def from_dict(d: dict) -> Command:
        return Command(
            command=d['command'],
            timeout=d.get('timeout'),
            platform=d.get('platform'),
            can_fail=d.get('can_fail', False)
        )

    @property
    def available(self) -> bool:
        return platform.system() == self.platform

    def execute(self, working_directory: Path | None = None) -> tuple[CompletedProcess | None, Error | None]:
        try:
            return subprocess.run(
                self.command,
                shell=True,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                timeout=self.timeout,
                text=True,
                cwd=working_directory,
                check=not self.can_fail,
            ), None
        except TimeoutExpired as e:
            return None, Error(f'Command "{e.cmd}" did not complete in {e.timeout:.2f}s')
        except CalledProcessError as e:
            return None, Error(f'Command "{e.cmd}" failed with exit code {e.returncode}\nstderr:\n{e.stderr}')


@dataclass(frozen=True)
class OutputAsserts:
    stream: Literal['stdout', 'stderr']
    empty: bool | None = None
    expected: Path | None = None
    match_whole: Collection[re.Pattern] = ()
    match_lines: Collection[re.Pattern] = ()

    @classmethod
    def from_dict(cls, d: dict, stream: Literal['stdout', 'stderr']) -> OutputAsserts:
        return cls(
            stream=stream,
            empty=d.get('empty', None),
            expected=Path(d['expected']) if 'expected' in d else None,
            match_whole=list(map(parse_pattern, d.get('match_whole', ()))),
            match_lines=list(map(parse_pattern, d.get('match_lines', ())))
        )

    def check(self, process: CompletedProcess) -> Error | None:
        out = remove_terminal_escape_sequences(getattr(process, self.stream))
        is_empty = not bool(out.strip())

        if self.empty is not None:
            if self.empty and not is_empty:
                return Error(f'Expected {self.stream} to be empty\n{self.stream}:\n{out}')

            if not self.empty and is_empty:
                return Error(f'Expected {self.stream} to be not empty\n{self.stream}:\n{out}')

        if self.expected is not None:
            expected = self.expected.read_text(encoding='utf-8')
            if out != expected:
                return Error(f'Expected {self.stream} to match contents of "{self.expected}"')

        for pattern in self.match_whole:
            if pattern.search(out):
                continue

            return Error(f'Expected "{pattern.pattern}" to match {self.stream}')

        out_lines = out.split('\n')
        for pattern in self.match_lines:
            if any(map(pattern.search, out_lines)):
                continue

            return Error(f'Expected "{pattern.pattern}" to match some line of {self.stream}')

        return None


def load_config(test_dir: Path) -> dict | None:
    test_config_file_path = test_dir / TEST_CONFIG_FILE_NAME

    try:
        with test_config_file_path.open(mode='rt') as test_config_file:
            return json.load(test_config_file)
    except FileNotFoundError:
        print(f'"{TEST_CONFIG_FILE_NAME}" does not exist in "{test_dir}"')


def remove_terminal_escape_sequences(s: str) -> str:
    return re.sub(ESCAPE_CODE_REGEX, '', s)


def save_output(text: str, directory: Path, file_name: str) -> None:
    with (directory / file_name).open(mode='wt') as file:
        file.write(text)


EMPTY_LINE: Final[str] = ' ' * 120
TEST_CONFIG_FILE_NAME: Final[str] = 'test_config.json'

STDOUT_FILE_NAME: Final[str] = 'stdout.txt'
STDERR_FILE_NAME: Final[str] = 'stderr.txt'

OK: Final[str] = '\u001b[1m\u001b[32mOK\033[0m'
FAILED: Final[str] = '\u001b[1m\u001b[31mFAILED\033[0m'


class InlinePrinter:
    def __init__(self) -> None:
        self.printed_char_count = 0

    def print(self, it: str) -> None:
        print(' ' * self.printed_char_count, end='')
        print('\r' + it, end='\r')
        self.printed_char_count = len(it)

    def println(self, it: str) -> None:
        self.print(it)
        print()


def run_test(test_name: str, test_dir: Path) -> bool:
    if (config := load_config(test_dir)) is None:
        return False

    prefix = f'{test_name} - '

    printer = InlinePrinter()

    def fail(error_: Error) -> Literal[False]:
        printer.println(f'{prefix}{FAILED}\n{error_}')
        return False

    def success() -> Literal[True]:
        printer.println(f'{prefix}{OK}')
        return True

    arrange_commands: list[Command] = [Command.from_dict(it) for it in config.get('arrange', [])]
    act_command: Command = Command.from_dict(config['act'])

    exit_code: int = config['assert'].get('exit_code')
    stdout_assert: OutputAsserts = OutputAsserts.from_dict(config['assert'].get('stdout', {}), 'stdout')
    stderr_assert: OutputAsserts = OutputAsserts.from_dict(config['assert'].get('stderr', {}), 'stderr')
    save: list[str] = config.get('save', [])

    for command in arrange_commands:
        if not command.available:
            continue

        printer.print(f'{prefix}Running "{command.command}"')
        _, error = command.execute(test_dir)
        if error:
            return fail(error)

    printer.print(f'{prefix}Running "{act_command.command}"')
    process, error = act_command.execute(test_dir)
    if error:
        return fail(error)
    if exit_code is not None and exit_code != process.returncode:
        return fail(Error(f'Expected exit code to be {exit_code}, but was {process.returncode}'))

    if 'stdout' in save:
        save_output(process.stdout, test_dir, STDOUT_FILE_NAME)
    if 'stderr' in save:
        save_output(process.stderr, test_dir, STDERR_FILE_NAME)

    if error := stdout_assert.check(process):
        if 'stdout' not in save:
            save_output(process.stdout, test_dir, STDOUT_FILE_NAME)

        return fail(error)

    if error := stderr_assert.check(process):
        if 'stderr' not in save:
            save_output(process.stderr, test_dir, STDERR_FILE_NAME)

        return fail(error)

    return success()


def collect_tests(
        test_root: Path,
        test_name_pattern: re.Pattern,
        name_prefix: str = ''
) -> list[tuple[str, Path]]:
    if (test_root / TEST_CONFIG_FILE_NAME).exists():
        return [(f'{name_prefix}{test_root.name}', test_root)]

    tests: list[tuple[str, Path]] = []

    for path in sorted(test_root.iterdir()):
        if not path.is_dir():
            continue

        if not test_name_pattern.fullmatch(path.name):
            continue

        if (path / TEST_CONFIG_FILE_NAME).exists():
            tests.append((f'{name_prefix}{path.name}', path))
            continue

        tests.extend(collect_tests(
            path,
            test_name_pattern,
            name_prefix=f'{name_prefix}{path.name}/'
        ))

    return tests


@click.command()
@click.argument(
    'tests-root',
    type=Path,
)
@click.option(
    '--test-name-pattern',
    required=False,
    help='Regex used to find test folders.',
    type=str,
    default=r'[^.(__)]\w+(_\w+)+'
)
def main(
        tests_root: Path,
        test_name_pattern: str
) -> None:
    for test_name, test_path in collect_tests(tests_root, re.compile(test_name_pattern)):
        run_test(test_name, test_path)


if __name__ == '__main__':
    main()
