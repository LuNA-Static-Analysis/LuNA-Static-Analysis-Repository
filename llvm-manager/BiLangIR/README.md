# BiLangIR

Проект для создания LLVM IR (Intermediate Representation) из AST (Abstract Syntax Tree) и ll файлов.

## Требования

- **CMake** 3.16 или выше
- **LLVM 15** с development headers
- **Clang++ 15**
- **Luna framework**

## Быстрый старт

### Использование скрипта сборки (рекомендуется)

```bash
# Debug сборка (по умолчанию)
./build.sh

# Release сборка (оптимизированная)
./build.sh --release

# Чистая пересборка
./build.sh --clean

# Справка по опциям
./build.sh --help
```

### Ручная сборка через CMake

```bash
mkdir build && cd build
cmake .. -DCMAKE_BUILD_TYPE=Release
make -j$(nproc)
```

## Использование

```bash
# Запуск программы
./build/BiLangIR <json_file> <ll_file|ll_dir> [<ll_file|ll_dir> ...]
```

## Доступные команды CMake

```bash
# Сборка проекта
cmake --build build

# Запуск программы (без аргументов)
cmake --build build --target run

# Форматирование кода (если установлен clang-format)
cmake --build build --target format

# Полная очистка
cmake --build build --target clean-all
```
