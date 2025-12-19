# Manager of BiLangIR and PhASAR-advisor

Удобный Python-скрипт для управления подпроектами BiLangIR и PhASAR-advisor.

## Установка

```bash
# Установка зависимостей
pip install -r requirements.txt

# Сделать скрипт исполняемым
chmod +x manager.py
```

## Использование

### Базовые команды

```bash
# Показать справку
./manager.py --help

# Показать информацию о проекте
./manager.py info

# Показать доступные цели сборки
./manager.py targets
```

### Конфигурация проекта

```bash
# Базовая конфигурация (Debug режим)
./manager.py configure

# Release конфигурация
./manager.py configure --build-type Release

# Конфигурация с указанием пути к LLVM
./manager.py configure --llvm-dir /usr/local/llvm-15
```

### Сборка

```bash
# Сборка всего проекта
./manager.py build

# Сборка конкретной цели
./manager.py build --target BiLangIR

# Параллельная сборка (8 потоков)
./manager.py build --jobs 8
```

### Очистка

```bash
# Обычная очистка
./manager.py clean

# Полная очистка (удаление build/)
./manager.py clean --full
```

### Полная пересборка

```bash
# Пересборка в Debug режиме
./manager.py rebuild

# Пересборка в Release режиме
./manager.py rebuild --build-type Release

# С указанием количества потоков
./manager.py rebuild --jobs 8
```

### Запуск подпроектов

```bash
# Запуск BiLangIR
./manager.py run BiLangIR

# Запуск PhASAR-advisor 
./manager.py run PhASAR-advisor
```

## Требования

- Python 3.6+
- CMake 3.16+
- Ninja (рекомендуется)
- LLVM 15
- PhASAR 
