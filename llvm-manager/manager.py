#!/usr/bin/env python3

import click
import subprocess
import os
import shutil
import sys

from pathlib import Path
from typing import Optional, List, Union

class Colors:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKCYAN = '\033[96m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'


def print_info(msg: str):
    click.echo(f"{Colors.OKBLUE}ℹ {msg}{Colors.ENDC}")


def print_success(msg: str):
    click.echo(f"{Colors.OKGREEN}✓ {msg}{Colors.ENDC}")


def print_error(msg: str):
    click.echo(f"{Colors.FAIL}✗ {msg}{Colors.ENDC}", err=True)


def print_warning(msg: str):
    click.echo(f"{Colors.WARNING}⚠ {msg}{Colors.ENDC}")


def run_command(cmd: List[str], cwd: Optional[Path] = None, check: bool = True) -> Union[subprocess.CompletedProcess, subprocess.CalledProcessError]:
    cmd_str = ' '.join(str(c) for c in cmd)
    print_info(f"Выполняю: {cmd_str}")
    
    try:
        result = subprocess.run(
            cmd,
            cwd=cwd,
            check=check,
            capture_output=False,
            text=True
        )
        return result
    except subprocess.CalledProcessError as e:
        print_error(f"Команда завершилась с ошибкой (код {e.returncode})")
        if check:
            sys.exit(e.returncode)
        return e


class ProjectManager:
    
    def __init__(self):
        self.root = Path(__file__).parent.absolute()
        self.build_dir = self.root / "build"
        self.bilangir_dir = self.root / "BiLangIR"
        self.phasar_advisor_dir = self.root / "PhASAR-advisor"
    
    def ensure_build_dir(self):
        self.build_dir.mkdir(exist_ok=True)
    
    def configure(self, build_type: str = "Debug", bilangir: bool = True, 
                  phasar_advisor: bool = True, llvm_dir: Optional[str] = None, 
                  extra_args: Optional[List[str]] = None):
        self.ensure_build_dir()
        
        cmake_args = [
            "cmake",
            f"-DCMAKE_BUILD_TYPE={build_type}",
            f"-DBUILD_BILANGIR={'ON' if bilangir else 'OFF'}",
            f"-DBUILD_PHASAR_ADVISOR={'ON' if phasar_advisor else 'OFF'}",
            "-GNinja",
        ]
        
        if llvm_dir:
            cmake_args.append(f"-DLLVM_DIR={llvm_dir}")
        
        if extra_args:
            cmake_args.extend(extra_args)
        
        cmake_args.append(str(self.root))
        
        run_command(cmake_args, cwd=self.build_dir)
        print_success("Конфигурация завершена успешно")
    
    def build(self, target: Optional[str] = None, jobs: Optional[int] = None):
        if not self.build_dir.exists():
            print_error("Директория build не найдена. Сначала выполните configure.")
            sys.exit(1)
        
        cmake_args = ["cmake", "--build", str(self.build_dir)]
        
        if target:
            cmake_args.extend(["--target", target])
        
        if jobs:
            cmake_args.extend(["-j", str(jobs)])
        
        run_command(cmake_args)
        print_success("Сборка завершена успешно")
    
    def clean(self, full: bool = False):
        if full:
            if self.build_dir.exists():
                print_warning(f"Удаляю директорию {self.build_dir}")
                shutil.rmtree(self.build_dir)
                print_success("Полная очистка завершена")
            else:
                print_warning("Директория build не найдена")
        else:
            if self.build_dir.exists():
                run_command(["cmake", "--build", str(self.build_dir), "--target", "clean"])
                print_success("Очистка завершена")
            else:
                print_warning("Директория build не найдена")
    
    def run_bilangir(self, args: Optional[List[str]] = None):
        binary = self.build_dir / "bin" / "BiLangIR"
        if not binary.exists():
            print_error(f"BiLangIR не найден. Сначала выполните сборку.")
            sys.exit(1)
        
        cmd = [str(binary)]
        if args:
            cmd.extend(args)
        
        run_command(cmd)
    
    def run_phasar_advisor(self, args: Optional[List[str]] = None):
        binary = self.build_dir / "bin" / "PhASAR-advisor"
        if not binary.exists():
            print_error(f"PhASAR-advisor не найден. Сначала выполните сборку.")
            sys.exit(1)
        
        cmd = [str(binary)]
        if args:
            cmd.extend(args)
        
        run_command(cmd)
    
    def test_bilangir(self):
        test_script = self.bilangir_dir / "test.sh"
        if not test_script.exists():
            print_error("test.sh не найден")
            sys.exit(1)
        
        run_command(["bash", str(test_script)], cwd=self.bilangir_dir)
    
    def format_code(self, project: Optional[str] = None):
        if not self.build_dir.exists():
            print_error("Директория build не найдена. Сначала выполните configure.")
            sys.exit(1)
        
        if project == "bilangir":
            run_command(["cmake", "--build", str(self.build_dir), "--target", "bilangir-format"])
        elif project is None:
            # Форматируем всё
            run_command(["cmake", "--build", str(self.build_dir), "--target", "bilangir-format"], check=False)
        
        print_success("Форматирование завершено")


manager = ProjectManager()


@click.group()
@click.version_option(version="1.0.0", prog_name="Manager of BiLangIR and PhASAR-advisor")
def cli():
    """
    Менеджер проектов LLVM
    
    Управление подпроектами BiLangIR и PhASAR-advisor
    """
    pass


@cli.command()
@click.option('--build-type', '-b', type=click.Choice(['Debug', 'Release', 'RelWithDebInfo', 'MinSizeRel']),
              default='Debug', help='Тип сборки')
@click.option('--bilangir/--no-bilangir', default=True, help='Включить BiLangIR')
@click.option('--phasar-advisor/--no-phasar-advisor', default=True, help='Включить PhASAR-advisor')
@click.option('--llvm-dir', '-l', type=click.Path(exists=True), help='Путь к LLVM installation')
@click.argument('cmake_args', nargs=-1)
def configure(build_type, bilangir, phasar_advisor, llvm_dir, cmake_args):
    """Конфигурация проекта с помощью CMake"""
    manager.configure(
        build_type=build_type,
        bilangir=bilangir,
        phasar_advisor=phasar_advisor,
        llvm_dir=llvm_dir,
        extra_args=list(cmake_args) if cmake_args else None
    )


@cli.command()
@click.option('--target', '-t', help='Конкретная цель для сборки')
@click.option('--jobs', '-j', type=int, help='Количество параллельных задач')
def build(target, jobs):
    """Сборка проекта"""
    manager.build(target=target, jobs=jobs)


@cli.command()
@click.option('--full', '-f', is_flag=True, help='Полная очистка (удаление build/)')
def clean(full):
    """Очистка проекта"""
    manager.clean(full=full)


@cli.command()
@click.option('--build-type', '-b', type=click.Choice(['Debug', 'Release']), default='Debug')
@click.option('--jobs', '-j', type=int, help='Количество параллельных задач')
def rebuild(build_type, jobs):
    """Полная пересборка проекта (clean + configure + build)"""
    print_info("Шаг 1/3: Очистка")
    manager.clean(full=True)
    
    print_info("Шаг 2/3: Конфигурация")
    manager.configure(build_type=build_type)
    
    print_info("Шаг 3/3: Сборка")
    manager.build(jobs=jobs)
    
    print_success("Пересборка завершена успешно!")


@cli.group()
def run():
    """Запуск подпроектов"""
    pass


@run.command(name='bilangir', context_settings=dict(ignore_unknown_options=True, allow_extra_args=True))
@click.pass_context
def run_bilangir(ctx):
    """Запуск BiLangIR"""
    args = ctx.args
    manager.run_bilangir(args=args if args else None)


@run.command(name='phasar-advisor', context_settings=dict(ignore_unknown_options=True, allow_extra_args=True))
@click.pass_context
def run_phasar_advisor(ctx):
    """Запуск PhASAR-advisor"""
    args = ctx.args
    manager.run_phasar_advisor(args=args if args else None)


@cli.command()
def info():
    """Информация о проекте и окружении"""
    click.echo(f"\n=== Project Info ===\n")
    
    click.echo(f"{Colors.OKCYAN}Корневая директория:{Colors.ENDC} {manager.root}")
    click.echo(f"{Colors.OKCYAN}Директория сборки:{Colors.ENDC} {manager.build_dir}")
    click.echo(f"{Colors.OKCYAN}BiLangIR:{Colors.ENDC} {manager.bilangir_dir}")
    click.echo(f"{Colors.OKCYAN}PhASAR-advisor:{Colors.ENDC} {manager.phasar_advisor_dir}")
    
    # Проверка наличия собранных бинарников
    click.echo(f"\n{Colors.BOLD}Статус сборки:{Colors.ENDC}")
    
    bilangir_binary = manager.build_dir / "bin" / "BiLangIR"
    phasar_binary = manager.build_dir / "bin" / "PhASAR-advisor"
    
    if bilangir_binary.exists():
        print_success(f"BiLangIR собран: {bilangir_binary}")
    else:
        print_warning("BiLangIR не собран")
    
    if phasar_binary.exists():
        print_success(f"PhASAR-advisor собран: {phasar_binary}")
    else:
        print_warning("PhASAR-advisor не собран")
    
    # Проверка CMake
    click.echo(f"\n{Colors.BOLD}Инструменты:{Colors.ENDC}")
    try:
        result = subprocess.run(["cmake", "--version"], capture_output=True, text=True)
        if result.returncode == 0:
            version = result.stdout.split('\n')[0]
            print_success(f"CMake: {version}")
    except FileNotFoundError:
        print_error("CMake не найден")
    
    # Проверка Ninja
    try:
        result = subprocess.run(["ninja", "--version"], capture_output=True, text=True)
        if result.returncode == 0:
            print_success(f"Ninja: {result.stdout.strip()}")
    except FileNotFoundError:
        print_warning("Ninja не найден (опционально)")
    
    click.echo()


@cli.command()
def targets():
    """Список доступных целей сборки"""
    click.echo(f"\n=== Доступные цели сборки ===\n")
    
    click.echo(f"{Colors.OKCYAN}Основные цели:{Colors.ENDC}")
    click.echo("  • BiLangIR - генератор LLVM IR")
    click.echo("  • PhASAR-advisor - анализатор LLVM IR")
    
    click.echo(f"\n{Colors.OKCYAN}Дополнительные цели BiLangIR:{Colors.ENDC}")
    click.echo("  • bilangir-clean-all - полная очистка BiLangIR")
    click.echo("  • bilangir-run - запуск BiLangIR")
    
    click.echo()


if __name__ == '__main__':
    cli()
