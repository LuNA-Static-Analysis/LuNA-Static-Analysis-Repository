# Сборка запуск проекта (на линуксе)
  - git clone https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository.git
  - cd ast_analyzer
  - make
  - ./a.out <file_name>

## Например:
  ./a.out ./tests/test_analyzer/test_import.fa

## Важно иметь установленные flex и bison
  - sudo apt install flex bison

## Визуалция абстрактного синтаксического дерева
  При каждом запуске в текущей рабочей директории появляется файл ast.json.
  Удобный визуализатор: https://jsoncrack.com/
  Или аналог в виде его расширения в VSCode: JSON Crack
