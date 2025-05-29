# Model Checking Analyzer

Инструмент предназначен для проверки программ, написанных на языке LuNA,
методом верификации на моделях при помощи системы SPIN.

## Как работает
Приложение выполняет верификацию моделей для `LuNA-программ` следующим образом:

1. Парсит `.fa` файл в `AST` (через `parser`).
2. Преобразует AST в Promela-код.
3. Компилирует `Promela` с помощью `SPIN` + `GCC`.
4. Проверяет `LTL-свойства`, указанные в модели.
5. Если свойство нарушено, то извлекает трассу и локализует ошибку.
6. Генерирует список ошибок в JSON-формате, совместимым с `ADAPT`.

## Зависимости
- Node.js 16+ (`node`)
- SPIN (`spin`)
- GCC (`gcc`)s
- Парсер LuNA (`parser`)

## Установка
```bash
    sudo apt install gcc spin
    npm install
    npm install -g ts-node typescript
```

## Пример запуска
```
ts-node src/app/main.ts \
  --project-dir path/to/project \
  --build-dir path/to/build \
  --errors-file path/to/errors.json \
  --output-dir path/to/out \
  --no-cleanup \
  path/to/main.fa

```
## Пример запуска в составе ADAPT
```bash
  "$ADAPT_HOME/bin/adapt mc-analyzer/test/sem6.fa" --run=mc --no-cleanup
```
