# ADAPT - Комплекс автоматизированной отладки программ на языке LuNA

## Требования / Зависимости

- ОС: **Ubuntu / Debian**
- **LuNA** — [репозиторий LuNA](https://gitlab.ssd.sscc.ru/luna/luna)
- [JSON for Modern C++](https://json.nlohmann.me/)
- **PhASAR** — [сайт PhASAR](https://phasar.org/)
- **LLVM 15**
- Компилятор: **g++** с поддержкой **C++20** или новее
- **Node.js** и **npm**
- **Python** 3.13+
- **flex** и **bison**
- **SWI-Prolog**
- **SPIN**
- **graphviz**

## Установка

### 0. Установка LuNA

Склонируйте и соберите **LuNA** согласно инструкции из репозитория

### 1. Клонирование данного репозитория

```bash
git clone [URL данного репозитория]
cd [директория клонирования]
```

### 2. Использование Python3.13 или выше

Пользователь может выбрать один из следующих вариантов:

- Нативная установка [Python3.13+](https://www.python.org/downloads/)
- Использование проектного менеджера, к примеру, [uv](https://docs.astral.sh/uv/)

### 3. Установка python-зависимостей

#### Нативный Python

```bash
python3.13 -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
```

#### С использованием uv

```bash
uv venv --python 3.13 --seed
source .venv/bin/activate
pip install -r requirements.txt
```

### 4. Установка системных пакетов

```bash
sudo apt update # <-- рекомендуется перед установкой
sudo apt install         \
    nlohmann-json3-dev   \
    build-essential      \
    g++                  \
    flex                 \
    bison                \
    swi-prolog           \
    spin                 \
    nodejs               \
    npm                  \
    graphviz
```

### 5. Установка node-зависимостей

```bash
npm install --prefix ./mc-analyzer
```

### 6. Установка PhASAR и LLVM 15

#### Установка LLVM 15

```bash
sudo apt update # <-- рекомендуется перед установкой
sudo apt install                          \
                 libllvm-15-ocaml-dev     \
                 libllvm15t64             \
                 llvm-15                  \
                 llvm-15-dev              \
                 llvm-15-doc              \
                 llvm-15-examples         \
                 llvm-15-runtime          \
                 clang-15                 \
                 clang-tools-15           \
                 clang-15-doc             \
                 libclang-common-15-dev   \
                 libclang-15-dev          \
                 libclang1-15t64          \
                 libclang-rt-15-dev       \
                 lld-15
```

Если нет LLVM 15 в apt, то можно установить его с помощью официального [сайта](https://llvm.org/)

#### Установка PhASAR

1. скачать в удобную для пользователя директорию исходники PhASAR из [GitHub](https://github.com/secure-software-engineering/phasar/releases/tag/v2510)
2. перейти в директорию исходников
3. выполнить `git clone https://github.com/nlohmann/json.git external/json`
4. выполнить `git clone https://github.com/pboettch/json-schema-validator.git external/json-schema-validator`
5. найти и закомментировать строку `git submodule update --init` в скрипте `bootstrap.sh`
6. выполнить `llvm-config-15 --prefix` -- так пользователь получит путь к LLVM 15
7. выполнить `sudo ./bootstrap.sh --install -DLLVM_INSTALL_DIR=<path/to/llvm>`

### 7. Сборка проекта

В директории проекта ADAPT выполнить:

```bash
make
python3 llvm-manager/manager.py rebuild
```

### 8. Настройка переменных окружения

Одно из решений: редактирование файла `.bashrc` / `.zshrc` в домашней директории

0. Переменные окружения **LuNA** должны устанавливаться во время установки **LuNA**;
1. Установить `ADAPT_HOME` в путь к проекту;
2. Добавить `$ADAPT_HOME/bin` в `PATH`.

## Использование

### Чтобы искать ошибки в LuNA-программах

```bash
adapt [ОПЦИИ] FA_FILE [CPP_FILE_1 CPP_FILE_2 ...]
```

Опции:

- `--run [ast|degsa|prolog|mc|bilangir]`
  - Запустить только указанные анализаторы. Если опция не указана, будут запущены все анализаторы.
- `--no-cleanup`
  - Не удалять сгенерированные файлы.
- `--help`
  - Напечатать сообщение с информацией об опциях запуска и выйти.

Пример запуска:

```bash
adapt src/main.fa src/main.cpp --run degsa --run prolog --no-cleanup
```

### Чтобы получить отчёт по LuNA-программам (в LLVM IR представлении)

```bash
llvm-manager report <path/to/llvm-ir> [analyses...]
```

Если не указывать конкретные виды анализов, выполнятся все, а посмотреть доступные виды анализов можно с помощью команды:

```bash
llvm-manager report -- --help
```

Для получения LLVM IR нужно воспользоваться программой adapt с использованием BiLangIR и флагом `--no-cleanup`
