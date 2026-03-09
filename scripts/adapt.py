import os
import shutil
import subprocess
import sys
from pathlib import Path
from typing import List

import click

if adapt_home_str := os.getenv('ADAPT_HOME'):
    ADAPT_HOME = Path(adapt_home_str)
else:
    ADAPT_HOME = Path(__file__).parent.resolve()


def collect_cpp_files(paths: List[Path]) -> List[Path]:
    cpp_files = []
    for path in paths:
        if path.is_file():
            if path.suffix in ['.cpp', '.cc', '.cxx', '.c++']:
                cpp_files.append(path)
        elif path.is_dir():
            # Recursively find all .cpp files in directory
            for ext in ['.cpp', '.cc', '.cxx', '.c++']:
                cpp_files.extend(path.rglob(f'*{ext}'))
    return cpp_files


@click.command()
@click.option(
    '--run',
    multiple=True,
    default=['ast', 'degsa', 'prolog', 'mc', 'bilangir'],
    required=False,
    show_default=True,
    help='Only run specified analyzers.',
    type=click.Choice(['ast', 'degsa', 'prolog', 'mc', 'bilangir'], case_sensitive=False)
)
@click.option(
    '--no-cleanup',
    is_flag=True,
    required=False,
    show_default=True,
    default=False,
    help='Do not delete generated files.'
)
@click.argument(
    'luna-src',
    type=click.Path(
        exists=True,
        dir_okay=False,
        path_type=Path
    )
)
@click.argument(
    'cpp-src',
    nargs=-1,
    required=False,
    type=click.Path(
        exists=True,
        path_type=Path
    )
)

def main(
        run: list[str],
        luna_src: Path,
        no_cleanup: bool,
        cpp_src: tuple[Path]
) -> None:
    if not run:
        print('Nothing to run')
        exit()

    # Collect all cpp files from provided paths/directories
    cpp_files = collect_cpp_files(list(cpp_src)) if cpp_src else []

    if 'bilangir' in run and not cpp_files:
        print('WARNING: bilangir analyzer requires C++ files, skipping', file=sys.stderr)
        run = [r for r in run if r != 'bilangir']

    if not run:
        print('Nothing to run after filtering analyzers that require C++ files')
        exit()

    project_dir = luna_src.resolve().parent

    output_dir = project_dir / '.adapt'
    output_dir.mkdir(parents=True, exist_ok=True)

    prolog_analyzer_output_dir = project_dir / '.prolog-analyzer'
    prolog_analyzer_output_dir.mkdir(parents=True, exist_ok=True)

    mc_analyzer_output_dir = project_dir / '.mc-analyzer'
    mc_analyzer_output_dir.mkdir(parents=True, exist_ok=True)

    build_dir = project_dir / '.luna-build'
    build_dir.mkdir(parents=True, exist_ok=True)
    
    bilangir_dir = project_dir / '.BiLangIR'
    bilangir_dir.mkdir(parents=True, exist_ok=True)

    preprocessed_file = output_dir / 'preprocessed.fa'
    preprocessed_file.unlink(missing_ok=True)

    # TODO './reporter/found_errors.json' is hard-coded in ast-ddg-analyzer
    errors_file = output_dir / 'reporter' / 'found_errors.json'
    errors_file.parent.mkdir(exist_ok=True)
    errors_file.unlink(missing_ok=True)

    exit_code = 0
    try:
        print(f'\rRunning preprocessor\r', end='')
        subprocess.run(
            args=[
                'python3', ADAPT_HOME / 'parser' / 'pp.py', luna_src.name,
                '-o', preprocessed_file,
                f'--text-info={str(build_dir / "preprocessed.fa.ti")}'
            ],
            cwd=project_dir,
            capture_output=True,
            check=True
        )

        if 'ast' in run or 'degsa' in run or 'bilangir' in run:
            args = [
                ADAPT_HOME / 'bin' / 'ast-ddg-analyzer',
                preprocessed_file.resolve(),
                luna_src.resolve()
            ]

            if 'ast' in run or 'bilangir' in run:
                args += ['-ast']
            if 'degsa' in run:
                args += ['-degsa']

            try:
                print(f'\rRunning {", ".join([it for it in run if it != "prolog"])}\r', end='')
                subprocess.run(
                    args=args,
                    cwd=output_dir,
                    capture_output=True,
                    check=True,
                )
            except subprocess.CalledProcessError as e:
                print(
                    f'WARNING: {" ".join(map(str, e.cmd))} '
                    f'failed with exit code {e.returncode}:\n'
                    f'{e.stderr.decode("utf-8")}',
                    file=sys.stderr
                )
            
            if 'bilangir' in run:
                try:
                    shutil.copyfile(src=output_dir / 'ast.json', dst=bilangir_dir / 'ast.json')
                except:
                    print(f'WARNING: the ast was not copied to BiLangIR')

        if 'prolog' in run:
            try:
                print(f'\rRunning prolog-analyzer         \r', end='')
                subprocess.run(
                    args=[
                        ADAPT_HOME / 'prolog-analyzer' / 'bin' / 'prolog-analyzer',
                        '--project-dir', project_dir,
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
            except subprocess.CalledProcessError as e:
                print(
                    f'WARNING: {" ".join(map(str, e.cmd))} '
                    f'failed with exit code {e.returncode}:\n'
                    f'{e.stderr.decode("utf-8")}',
                    file=sys.stderr
                )
                
        if 'bilangir' in run:
            try:
                print(f'\rRunning BiLangIR                 \r', end='')
                ll_files = []
                
                for cpp_file in cpp_files:
                    cpp_ll_file = bilangir_dir / f'{cpp_file.stem}.ll'
                    subprocess.run(
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
                    ll_files.append(cpp_ll_file)
                
                subprocess.run(
                    args=[
                        ADAPT_HOME / 'llvm-manager' / 'build' / 'bin' /'BiLangIR',
                        '-o', errors_file,
                        '-p', project_dir,
                        bilangir_dir / 'ast.json',
                        *ll_files
                    ],
                    capture_output=True,
                    check=True
                )
                
                subprocess.run(
                    args=[
                        ADAPT_HOME / 'llvm-manager' / 'build' / 'bin' /'PhASAR-advisor',
                        '--error-checking',
                        bilangir_dir / 'definitions.txt',
                        bilangir_dir / 'output.ll',
                        errors_file
                    ],
                    capture_output=True,
                    check=True
                )
                    
            except subprocess.CalledProcessError as e:
                print(
                    f'WARNING: {" ".join(map(str, e.cmd))} '
                    f'failed with exit code {e.returncode}:\n'
                    f'{e.stderr.decode("utf-8")}',
                    file=sys.stderr
                )
            
        if 'mc' in run:
            try:
                print(f'\rRunning mc-analyzer                 \r', end='')
                subprocess.run(
                    args=[
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
            except subprocess.CalledProcessError as e:
                print(
                    f'WARNING: {" ".join(map(str, e.cmd))} '
                    f'failed with exit code {e.returncode}:\n'
                    f'{e.stderr.decode("utf-8")}',
                    file=sys.stderr
                )
        # exit_code = 1

        if not errors_file.exists():
            errors_file.write_text('[]\n')

        print('\rGenerating output\r', end='')
        print('\r                                \r', end='')
        output_generator_process = subprocess.run(
            args=[
                'python3', ADAPT_HOME / 'scripts' / 'reporter' / 'adapt_output_generator.py',
                errors_file,
                '--project-dir', project_dir,
                '--build-dir', build_dir
            ],
            cwd=ADAPT_HOME / 'scripts' / 'reporter',
            capture_output=True,
            check=True
        )
        print(output_generator_process.stdout.decode('utf-8'))
    except subprocess.CalledProcessError as e:
        print(
            f'ERROR: {" ".join(map(str, e.cmd))} '
            f'failed with exit code {e.returncode}:\n'
            f'{e.stderr.decode("utf-8")}',
            file=sys.stderr
        )
        exit_code = 1
    except FileNotFoundError as e:
        print(f'ERROR: file "{e.filename}" does not exist')
        exit_code = 1
    finally:
        if not no_cleanup:
            shutil.rmtree(mc_analyzer_output_dir)
            shutil.rmtree(bilangir_dir)
            shutil.rmtree(output_dir)
            shutil.rmtree(prolog_analyzer_output_dir)
            shutil.rmtree(build_dir)

    exit(exit_code)


if __name__ == '__main__':
    exit(main())
