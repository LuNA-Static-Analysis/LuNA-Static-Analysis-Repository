import json
import os
import shutil
import subprocess
import sys
from pathlib import Path

import click


@click.command()
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
    default=Path('build'),
    help='LuNA project build directory.',
    type=click.Path(
        file_okay=False,
        resolve_path=True,
        path_type=Path,
    )
)
@click.option(
    '--output-dir',
    required=False,
    show_default=True,
    default=Path('.prolog-analyzer'),
    help='Directory for program output.',
    type=click.Path(
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
    help='Append errors list to existing file or create a new errors file.',
    type=click.Path(
        dir_okay=False,
        resolve_path=True,
        path_type=Path,
    )
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
    '--ignore',
    type=str,
    required=False,
    multiple=True
)
@click.argument(
    'luna-src',
    nargs=-1,
    type=click.Path(
        dir_okay=False,
        resolve_path=True,
        path_type=Path,
    )
)
def main(
        project_dir: Path,
        build_dir: Path,
        output_dir: Path,
        errors_file: Path | None,
        no_cleanup: bool,
        ignore: list[str],
        luna_src: tuple[Path, ...]
) -> None:
    prolog_analyzer_home = Path(os.environ.get('PROLOG_ANALYZER_HOME', str(Path.cwd())))

    output_dir.mkdir(exist_ok=True, parents=True)

    if errors_file is None:
        errors_file = output_dir / 'errors.json'
    errors_file.parent.mkdir(exist_ok=True)

    facts_file = output_dir / 'program_facts.pro'
    new_errors_file = output_dir / 'errors.json'

    base_errors = []
    if errors_file is None:
        errors_file = new_errors_file
    elif errors_file.exists() and errors_file.is_file():
        with errors_file.open('rt') as base_errors_file:
            base_errors = json.load(base_errors_file)
    else:
        new_errors_file = errors_file

    exit_code = 0
    try:
        print('\rCompiling LuNA program\r', end='')
        try:
            subprocess.run(
                args=[
                    'python3',
                    prolog_analyzer_home / 'src' / 'py' / 'luna-ja-only.py',
                    '--no-cleanup',
                    f'--build-dir={str(build_dir.absolute())}',
                    *map(lambda it: it.resolve().name, luna_src)
                ],
                cwd=project_dir,
                check=True
            )
        except subprocess.CalledProcessError:
            subprocess.run(
                args=[
                    'luna',
                    '-q',
                    '--compile-only',
                    f'--build-dir={str(build_dir)}',
                    *map(lambda it: it.resolve().name, luna_src)
                ],
                cwd=project_dir,
                capture_output=True,
                check=True
            )

        print('\rGenerating facts      \r', end='')
        subprocess.run(
            args=[
                'python3',
                prolog_analyzer_home / 'src' / 'py' / 'luna2pro.py',
                '--json-algorithm', build_dir / 'program_recom.ja',
                '--text-info', build_dir / 'preprocessed.fa.ti',
                '-o', facts_file
            ],
            capture_output=True,
            check=True
        )

        print('\rAnalyzing             \r', end='')
        subprocess.run(
            args=[
                'swipl',
                '-q',
                '--on-error=halt',
                '-t', 'main',
                'src/pro/run.pro',
                '--',
                facts_file, new_errors_file
            ],
            cwd=str(prolog_analyzer_home),
            capture_output=True,
            check=True
        )

        with new_errors_file.open('rt') as new_errors_file_:
            new_errors = json.load(new_errors_file_)
            # FIXME filtered errors must not be detected in the first place
            new_errors = [it for it in new_errors if it['error_code'] not in ignore]
        with errors_file.open('wt') as dst_errors_file_:
            json.dump(base_errors + new_errors, dst_errors_file_)

        print(f'\rResults have been written to {errors_file}')
    except subprocess.CalledProcessError as e:
        print(
            f'ERROR: {" ".join(map(str, e.cmd))} '
            f'failed with exit code {e.returncode}:\n'
            f'{e.stderr.decode("utf-8")}',
            file=sys.stderr
        )

        exit_code = 1
    finally:
        if not no_cleanup:
            shutil.rmtree(build_dir, ignore_errors=True)

            if output_dir != errors_file.parent:
                shutil.rmtree(output_dir, ignore_errors=True)
            else:
                facts_file.unlink(missing_ok=True)

    exit(exit_code)


if __name__ == '__main__':
    main()
