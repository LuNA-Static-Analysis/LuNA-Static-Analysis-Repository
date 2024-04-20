# ADAPT - Комплекс автоматизированной отладки программ на языке LuNA

## Установка

#### 1. Установка зависимостей
   1. flex (`sudo apt-get install flex`)
   2. bison (`sudo apt-get install bison`)
   3. `g++` с поддержкой `C++20` или выше (`sudo apt-get install g++`)
   4. [SWI-Prolog](https://www.swi-prolog.org/download/stable)
   5. [LuNA](https://gitlab.ssd.sscc.ru/luna/luna)
   6. [Python3.10+](https://www.python.org/downloads/)
   7. [requirements.txt](requirements.txt) (`python3 -m pip install -r requirements.txt`) 

#### 2. Сборка проекта 

В корне проекта выполнить `make`.

#### 3. Переменные окружения
   1. Установить `ADAPT_HOME` в путь к проекту;
   2. Добавить `$ADAPT_HOME/bin` в `PATH`.

## Использование

```bash
adapt [ОПЦИИ] LUNA_ПРОГРАММА
```

Опции:
* `--run [ast|degsa|prolog]`

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
