import json
import os
import shutil
import subprocess
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
    '--output-dir',
    required=False,
    default=None,
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
    default=False,
    help='Do not delete generated files.'
)
@click.argument('luna-src', nargs=-1)
def main(
        project_dir: Path,
        output_dir: Path,
        errors_file: Path | None,
        no_cleanup: bool,
        luna_src: tuple[str]
) -> None:
    prolog_analyzer_home = Path(os.environ.get('PROLOG_ANALYZER_HOME', str(Path.cwd())))

    build_dir = project_dir / 'build'
    if output_dir is None:
        output_dir = project_dir / '.prolog-analyzer'
    output_dir.mkdir(exist_ok=True, parents=True)

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

    print('\rCompiling LuNA program', end='')

    subprocess.run(
        args=[
            'luna',
            '-q',
            '--compile-only',
            f'--build-dir={str(build_dir)}',
            *luna_src
        ],
        capture_output=True,
        check=True,
    )

    print('\rGenerating facts      \r', end='')

    subprocess.run(
        args=[
            'python3',
            str(prolog_analyzer_home / 'src' / 'py' / 'luna2pro.py'),
            '--project-dir', str(project_dir),
            '--build-dir', str(build_dir),
            '-o', str(facts_file)
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
            str(facts_file), str(new_errors_file)
        ],
        cwd=str(prolog_analyzer_home),
        capture_output=True,
        check=True
    )

    with new_errors_file.open('rt') as new_errors_file_:
        new_errors = json.load(new_errors_file_)
    with errors_file.open('wt') as dst_errors_file_:
        json.dump(base_errors + new_errors, dst_errors_file_)

    if not no_cleanup:
        print('\rCleaning up           \r', end='')

        shutil.rmtree(build_dir)

        if output_dir != errors_file.parent:
            shutil.rmtree(output_dir)
        else:
            facts_file.unlink(missing_ok=True)

    print(f'\rResults have been written to {errors_file}')


if __name__ == '__main__':
    main()
