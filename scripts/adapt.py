import os
import shutil
import subprocess
import sys
from pathlib import Path

import click

if adapt_home_str := os.getenv('ADAPT_HOME'):
    ADAPT_HOME = Path(adapt_home_str)
else:
    ADAPT_HOME = Path(__file__).parent.resolve()


@click.command()
@click.option(
    '--run',
    multiple=True,
    default=['ast', 'degsa', 'prolog'],
    required=False,
    show_default=True,
    help='Only run specified analyzers.',
    type=click.Choice(['ast', 'degsa', 'prolog'], case_sensitive=False)
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
def main(
        run: list[str],
        luna_src: Path,
        no_cleanup: bool
) -> None:
    if not run:
        print('Nothing to run')
        exit()

    project_dir = luna_src.resolve().parent

    output_dir = project_dir / '.adapt'
    output_dir.mkdir(parents=True, exist_ok=True)

    prolog_analyzer_output_dir = project_dir / '.prolog-analyzer'
    prolog_analyzer_output_dir.mkdir(parents=True, exist_ok=True)

    build_dir = project_dir / '.luna-build'
    build_dir.mkdir(parents=True, exist_ok=True)

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

        if 'ast' in run or 'degsa' in run:
            args = [
                ADAPT_HOME / 'bin' / 'ast-ddg-analyzer',
                preprocessed_file.resolve(),
                luna_src.resolve()
            ]

            if 'ast' in run:
                args += ['-ast']
            if 'degsa' in run:
                args += ['-degsa']

            print(f'\rRunning {", ".join([it for it in run if it != "prolog"])}\r', end='')
            subprocess.run(
                args=args,
                cwd=output_dir,
                capture_output=True,
                check=True,
            )

        if 'prolog' in run:
            print(f'\rRunning prolog-analyzer         \r', end='')
            subprocess.run(
                args=[
                    ADAPT_HOME / 'prolog-analyzer' / 'bin' / 'prolog-analyzer',
                    '--project-dir', project_dir,
                    '--build-dir', build_dir,
                    '--errors-file', errors_file,
                    '--no-cleanup',
                    luna_src
                ],
                env=os.environ | {'PROLOG_ANALYZER_HOME': ADAPT_HOME / 'prolog-analyzer'},
                capture_output=True,
                check=True
            )

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
            shutil.rmtree(output_dir)
            shutil.rmtree(prolog_analyzer_output_dir)
            shutil.rmtree(build_dir)

    exit(exit_code)


if __name__ == '__main__':
    exit(main())
