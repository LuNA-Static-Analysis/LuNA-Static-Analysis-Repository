# ADAPT - Комплекс автоматизированной отладки программ на языке LuNA

## Установка

#### 1. Клонирование репозитория
   1. `git clone [URL данного репозитория]`

#### 2. Установка зависимостей
   0. `apt-get update` (рекомендуется)
   1. flex (`sudo apt-get install flex`)
   2. bison (`sudo apt-get install bison`)
   3. `g++` с поддержкой `C++20` или выше (`sudo apt-get install g++`)
   4. [SWI-Prolog](https://www.swi-prolog.org/download/stable) (`sudo apt-get install swi-prolog`)
   5. [LuNA](https://gitlab.ssd.sscc.ru/luna/luna)
   6. [Python3.13+](https://www.python.org/downloads/)
   7. [requirements.txt](requirements.txt) (`python3 -m pip install -r requirements.txt`)
   8. [SPIN] (`sudo apt install spin`)
   9. [Node.js] (`sudo apt install nodejs npm -y`)
  10. [package.json] (`npm install --prefix ./mc-analyzer`)
  11. [TypeScript] (`sudo npm install -g ts-node typescript`) 

#### 3. Сборка проекта 

В корне проекта выполнить `make`.

#### 4. Переменные окружения
Одно из решений: редактирование файла .bashrc в домашней директории.
   1. Установить `ADAPT_HOME` в путь к проекту;
   2. Добавить `$ADAPT_HOME/bin` в `PATH`;
   3. Установить `LUNA_HOME` в коренную директорию проекта;
   4. Добавить `$LUNA_HOME/bin` в `PATH` (по умолчанию там должен находиться исполняемый файл `parser`)  

## Использование

```bash
adapt [ОПЦИИ] LUNA_ПРОГРАММА
```

Опции:
* `--run [ast|degsa|prolog|mc]`

    Запустить только указанные 
    анализаторы. Если опция не указана, будут запущены все
    анализаторы.
* `--no-cleanup`
    
    Не удалять сгенерированные файлы.
* `--help`

    Напечатать сообщение с информацией об опциях запуска и выйти.

Пример запуска:
```bash
adapt src/main.fa --run degsa --run prolog --no-cleanup
```
