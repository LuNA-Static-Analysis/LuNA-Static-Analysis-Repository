import os
import shutil
import subprocess
import sys
from enum import StrEnum
from pathlib import Path
from typing import Final, Literal, Iterable

import click

if adapt_home_str := os.getenv('ADAPT_HOME'):
    ADAPT_HOME = Path(adapt_home_str)
else:
    ADAPT_HOME = Path(__file__).parent.resolve()

CPP_SEPARATOR: Final[str] = ':'


def collect_cpp_files_using_luna_rules(
        luna_src: Path,
        cpp_dir: str | None,
        cpp_source: str | None,
) -> list[Path]:
    cpp_paths = []

    luna_src = luna_src.resolve()

    def find_cpp_files_in_dir(path: Path) -> list[Path]:
        return [it for it in path.iterdir() if it.suffix == '.cpp']

    if cpp_dir:
        for dir_name in cpp_dir.split(CPP_SEPARATOR):
            cpp_paths.extend(find_cpp_files_in_dir(Path(dir_name).resolve()))
    else:
        cpp_paths.extend(find_cpp_files_in_dir(luna_src.parent))

    if cpp_source:
        for source_name in cpp_source.split(CPP_SEPARATOR):
            cpp_paths.append(Path(source_name).resolve())

    return cpp_paths


def print_running(what: str, verbose: bool) -> None:
    if verbose:
        print(f'Running {what}:')
        return

    print(f'\rRunning {what}' + ' ' * 40 + '\r', end='')


def print_process_output(p: subprocess.CompletedProcess, verbose: bool) -> None:
    if not verbose:
        return

    print(p.stdout.decode('utf-8'))
    print(p.stderr.decode('utf-8'))


def report_subprocess_error(e: subprocess.CalledProcessError, severity: Literal['error', 'warning']) -> None:
    print('\r' + ' ' * 80 + '\r', end='')
    print(
        f'{severity.upper()}: command "{" ".join(map(str, e.cmd))}" '
        f'failed with exit code {e.returncode}:\n'
        f'---- BEGIN PROCESS STDOUT ----\n'
        f'{e.stdout.decode("utf-8")}\n'
        f'----  END PROCESS STDOUT  ----\n'
        f'---- BEGIN PROCESS STDERR ----\n'
        f'{e.stderr.decode("utf-8")}'
        f'----  END PROCESS STDERR  ----\n',
        file=sys.stderr
    )


class RunChoice(StrEnum):
    AST_ANALYZER = 'ast'
    DEGSA = 'degsa'
    PROLOG_ANALYZER = 'prolog'
    MC_ANALYZER = 'mc'
    BILANGIR = 'bilangir'
    HYBRID_DEBUGGER = 'hybrid'


@click.command()
@click.option(
    '--run',
    multiple=True,
    default=list(RunChoice),
    required=False,
    show_default=True,
    help='Only run specified analyzers.',
    type=click.Choice(RunChoice, case_sensitive=False)
)
@click.option(
    '--no-cleanup',
    is_flag=True,
    required=False,
    show_default=True,
    default=False,
    help='Do not delete generated files.'
)
@click.option(
    '-v', '--verbose',
    is_flag=True,
    required=False,
    show_default=True,
    default=False,
    help='Show output of analyzers.'
)
@click.argument(
    'luna-src',
    type=click.Path(
        exists=True,
        dir_okay=False,
        path_type=Path,
        resolve_path=True,
    )
)
@click.argument(
    'luna-args',
    nargs=-1,
    type=click.UNPROCESSED
)
@click.option(
    '--cpp-dir',
    required=False,
    type=str,
    help='Specify source directories for *.cpp files (same as luna).'
)
@click.option(
    '--cpp-source',
    required=False,
    type=str,
    help='Specify additional *.cpp files (same as luna).'
)
def main(
        run: Iterable[RunChoice],
        no_cleanup: bool,
        verbose: bool,
        luna_src: Path,
        luna_args: list[str],
        cpp_dir: str | None,
        cpp_source: str | None
) -> None:
    if not run:
        print('Nothing to run')
        return

    cpp_files = collect_cpp_files_using_luna_rules(luna_src, cpp_dir, cpp_source)

    if RunChoice.BILANGIR in run and not cpp_files:
        print('WARNING: bilangir analyzer requires C++ files, skipping', file=sys.stderr)
        run = set(run) - {RunChoice.BILANGIR}

    if not run:
        print('Nothing to run after filtering analyzers that require C++ files')
        return

    project_dir = luna_src.resolve().parent

    output_dir = project_dir / '.adapt'
    output_dir.mkdir(parents=True, exist_ok=True)

    prolog_analyzer_output_dir = project_dir / '.prolog-analyzer'
    prolog_analyzer_output_dir.mkdir(parents=True, exist_ok=True)

    mc_analyzer_output_dir = project_dir / '.mc-analyzer'
    mc_analyzer_output_dir.mkdir(parents=True, exist_ok=True)

    # TODO run build once and pass the build directory to all components that might need it (prolog-analyzer, luna-hybrid-debugger)
    build_dir = project_dir / '.luna-build'
    build_dir.mkdir(parents=True, exist_ok=True)

    bilangir_dir = project_dir / '.BiLangIR'
    bilangir_dir.mkdir(parents=True, exist_ok=True)

    hybrid_debugger_output_dir = project_dir / '.hybrid-debugger'
    hybrid_debugger_output_dir.mkdir(parents=True, exist_ok=True)

    preprocessed_file = output_dir / 'preprocessed.fa'
    preprocessed_file.unlink(missing_ok=True)

    # TODO './reporter/found_errors.json' is hard-coded in ast-ddg-analyzer
    errors_file = output_dir / 'reporter' / 'found_errors.json'
    errors_file.parent.mkdir(exist_ok=True)
    errors_file.unlink(missing_ok=True)

    exit_code = 0
    try:
        print_running('preprocessor', verbose)
        pp_process = subprocess.run(
            args=[
                'python3', ADAPT_HOME / 'parser' / 'pp.py', luna_src.name,
                '-o', preprocessed_file,
                f'--text-info={str(build_dir / "preprocessed.fa.ti")}'
            ],
            cwd=project_dir,
            capture_output=True,
            check=True
        )
        print_process_output(pp_process, verbose)

        if RunChoice.AST_ANALYZER in run or RunChoice.DEGSA in run or RunChoice.BILANGIR in run:
            args = [
                ADAPT_HOME / 'bin' / 'ast-ddg-analyzer',
                preprocessed_file.resolve(),
                luna_src.resolve()
            ]

            analyzers = []
            if RunChoice.AST_ANALYZER in run or RunChoice.BILANGIR in run:
                args += ['-ast']
                analyzers.append('ast-analyzer')
            if RunChoice.DEGSA in run:
                args += ['-degsa']
                analyzers.append('DeGSA')

            try:
                print_running(', '.join(analyzers), verbose)
                ast_ddg_analyzer_process = subprocess.run(
                    args=args,
                    cwd=output_dir,
                    capture_output=True,
                    check=True,
                )
                print_process_output(ast_ddg_analyzer_process, verbose)
            except subprocess.CalledProcessError as e:
                report_subprocess_error(e, severity='warning')

            if RunChoice.BILANGIR in run:
                try:
                    shutil.copyfile(src=output_dir / 'ast.json', dst=bilangir_dir / 'ast.json')
                except:
                    print(f'WARNING: the ast was not copied to BiLangIR')

        if RunChoice.PROLOG_ANALYZER in run:
            try:
                print_running('prolog-analyzer', verbose)
                prolog_analyzer_process = subprocess.run(
                    args=[
                        ADAPT_HOME / 'prolog-analyzer' / 'bin' / 'prolog-analyzer',
                        '--build-dir', build_dir,
                        '--errors-file', errors_file,
                        '--output-dir', prolog_analyzer_output_dir,
                        '--no-cleanup',
                        *os.environ.get('PROLOG_ANALYZER_OPTIONS', '').split(),
                        luna_src
                    ],
                    env=os.environ | {'PROLOG_ANALYZER_HOME': ADAPT_HOME / 'prolog-analyzer'},
                    capture_output=True,
                    check=True
                )
                print_process_output(prolog_analyzer_process, verbose)
            except subprocess.CalledProcessError as e:
                report_subprocess_error(e, severity='warning')

        # TODO pass cpp_files as --cpp-source explicitly?
        if RunChoice.HYBRID_DEBUGGER in run:
            try:
                print_running('luna-hybrid-debugger', verbose)
                luna_hybrid_debugger_process = subprocess.run(
                    args=[
                        ADAPT_HOME / 'hybrid-debugger' / 'luna-hybrid-debugger',
                        '--output-dir', hybrid_debugger_output_dir,
                        '--found-errors', errors_file,
                        '--errors-file', errors_file,
                        '--no-cleanup',
                        luna_src,
                        *luna_args
                    ],
                    env=os.environ | {'LUNA_HYBRID_DEBUGGER_HOME': ADAPT_HOME / 'hybrid-debugger'},
                    capture_output=True,
                    check=True
                )
                print_process_output(luna_hybrid_debugger_process, verbose)
            except subprocess.CalledProcessError as e:
                report_subprocess_error(e, severity='warning')

        if RunChoice.BILANGIR in run:
            try:
                print_running('BiLangIR', verbose)
                ll_files = []

                for cpp_file in cpp_files:
                    cpp_ll_file = bilangir_dir / f'{cpp_file.stem}.ll'
                    clang_process = subprocess.run(
                        args=[
                            'clang++',
                            '-S',
                            '-emit-llvm',
                            '-g',
                            cpp_file.resolve(),
                            '-o',
                            cpp_ll_file
                        ],
                        capture_output=True,
                        check=True
                    )
                    print_process_output(clang_process, verbose)
                    ll_files.append(cpp_ll_file)

                bilangir_process = subprocess.run(
                    args=[
                        ADAPT_HOME / 'llvm-manager' / 'build' / 'bin' / 'BiLangIR',
                        '-o', errors_file,
                        '-p', project_dir,
                        bilangir_dir / 'ast.json',
                        *ll_files
                    ],
                    capture_output=True,
                    check=True
                )
                print_process_output(bilangir_process, verbose)

                phasar_advisor_process = subprocess.run(
                    args=[
                        ADAPT_HOME / 'llvm-manager' / 'build' / 'bin' / 'PhASAR-advisor',
                        '--error-checking',
                        bilangir_dir / 'definitions.txt',
                        bilangir_dir / 'output.ll',
                        errors_file
                    ],
                    capture_output=True,
                    check=True
                )
                print_process_output(phasar_advisor_process, verbose)
            except subprocess.CalledProcessError as e:
                report_subprocess_error(e, severity='warning')

        if RunChoice.MC_ANALYZER in run:
            try:
                print_running('mc-analyzer', verbose)
                mc_analyzer_process = subprocess.run(
                    args=[
                        'npx',
                        '--prefix', ADAPT_HOME / 'mc-analyzer',
                        'ts-node', ADAPT_HOME / 'mc-analyzer' / 'src' / 'app' / 'main.ts',
                        '--project-dir', project_dir,
                        '--build-dir', build_dir,
                        '--errors-file', errors_file,
                        '--output-dir', mc_analyzer_output_dir,
                        '--no-cleanup',
                        '--luna-src', luna_src,
                        preprocessed_file
                    ],
                    env=os.environ | {'MC_ANALYZER_HOME': ADAPT_HOME / 'mc-analyzer'},
                    capture_output=True,
                    check=True
                )
                print_process_output(mc_analyzer_process, verbose)
            except subprocess.CalledProcessError as e:
                report_subprocess_error(e, severity='warning')

        if not errors_file.exists():
            errors_file.write_text('[]\n')

        print_running('output generator', verbose)
        output_generator_process = subprocess.run(
            args=[
                'python3', ADAPT_HOME / 'scripts' / 'reporter' / 'adapt_output_generator.py',
                errors_file,
            ],
            capture_output=True,
            check=True
        )
        print('\r' + ' ' * 80 + '\r', end='')
        print(output_generator_process.stdout.decode('utf-8'))
    except subprocess.CalledProcessError as e:
        report_subprocess_error(e, severity='error')
        exit_code = 1
    except FileNotFoundError as e:
        print(f'ERROR: file "{e.filename}" does not exist')
        exit_code = 1
    finally:
        if not no_cleanup:
            shutil.rmtree(bilangir_dir)
            shutil.rmtree(output_dir)
            shutil.rmtree(prolog_analyzer_output_dir)
            shutil.rmtree(hybrid_debugger_output_dir)
            shutil.rmtree(mc_analyzer_output_dir)
            shutil.rmtree(build_dir)

    sys.exit(exit_code)


if __name__ == '__main__':
    main()
