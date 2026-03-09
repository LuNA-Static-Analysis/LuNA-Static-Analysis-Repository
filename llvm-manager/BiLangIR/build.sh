#!/bin/bash

# Скрипт для сборки проекта с помощью CMake

set -e  # Остановиться при ошибке

# Цвета для вывода
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Функция для вывода сообщений
print_status() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Переменные
BUILD_DIR="build"
BUILD_TYPE="Debug"
CLEAN_BUILD=false

# Обработка аргументов
while [[ $# -gt 0 ]]; do
    case $1 in
        --release)
            BUILD_TYPE="Release"
            shift
            ;;
        --clean)
            CLEAN_BUILD=true
            shift
            ;;
        --help)
            echo "Использование: $0 [ОПЦИИ]"
            echo "ОПЦИИ:"
            echo "  --release    Сборка в режиме Release (по умолчанию Debug)"
            echo "  --clean      Очистить сборку перед компиляцией"
            echo "  --help       Показать эту справку"
            exit 0
            ;;
        *)
            print_error "Неизвестный аргумент: $1"
            echo "Используйте --help для справки"
            exit 1
            ;;
    esac
done

# Проверка наличия CMake
if ! command -v cmake &> /dev/null; then
    print_error "CMake не найден. Установите CMake для сборки проекта."
    exit 1
fi

# Проверка наличия LLVM
if ! command -v llvm-config-16 &> /dev/null; then
    print_error "LLVM 16 не найден. Убедитесь, что LLVM 16 установлен."
    exit 1
fi

print_status "Начинаем сборку проекта в режиме ${BUILD_TYPE}..."

# Очистка если нужно
if [ "$CLEAN_BUILD" = true ] || [ ! -d "$BUILD_DIR" ]; then
    if [ -d "$BUILD_DIR" ]; then
        print_status "Очищаем предыдущую сборку..."
        rm -rf "$BUILD_DIR"
    fi
fi

# Создание директории сборки
mkdir -p "$BUILD_DIR"
cd "$BUILD_DIR"

# Конфигурация
print_status "Конфигурируем проект..."
cmake .. -DCMAKE_BUILD_TYPE="$BUILD_TYPE"

# Сборка
print_status "Собираем проект..."
make -j$(nproc)

print_status "Сборка завершена успешно!"
print_status "Исполняемый файл: ${BUILD_DIR}/BiLangIR"

# Возврат в исходную директорию
cd ..
