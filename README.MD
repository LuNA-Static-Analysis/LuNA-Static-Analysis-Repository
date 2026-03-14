# ADAPT - Комплекс автоматизированной отладки программ на языке LuNA

## Требования / Зависимости

- ОС: **Ubuntu / Debian**
- **LuNA** — [репозиторий LuNA](https://gitlab.ssd.sscc.ru/luna/luna)
- [JSON for Modern C++](https://json.nlohmann.me/)
- **PhASAR** — [сайт PhASAR](https://phasar.org/)
- **LLVM 16** (будет установлен с **PhASAR**)
- Компилятор: **g++** с поддержкой **C++20** или новее
- **Node.js** и **npm**
- **Python** 3.13+
- **flex** и **bison**
- **SWI-Prolog**
- **SPIN**

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
    npm
```

### 5. Установка node-зависимостей

```bash
npm install --prefix ./mc-analyzer
sudo npm install -g ts-node typescript
```

### 6. Установка PhASAR и LLVM 16

Инструкция по установке есть на [сайте](https://phasar.org/tutorial/) и в [репозитории](https://github.com/secure-software-engineering/phasar/blob/development/BUILD.md) **PhASAR**

### 7. Сборка проекта 

```bash
make
python3 llvm-manager/manager.py rebuild
```

### 7. Настройка переменных окружения

Одно из решений: редактирование файла `.bashrc` / `.zshrc` в домашней директории

0. Переменные окружения **LuNA** должны устанавливаться во время установки **LuNA**;
1. Установить `ADAPT_HOME` в путь к проекту;
2. Добавить `$ADAPT_HOME/bin` в `PATH`.

## Использование

```bash
adapt [ОПЦИИ] LUNA_ПРОГРАММА
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
adapt src/main.fa --run degsa --run prolog --no-cleanup
```
