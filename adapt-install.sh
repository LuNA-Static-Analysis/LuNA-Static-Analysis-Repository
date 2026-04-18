#!/usr/bin/bash

echo "Добро пожаловать в установщик проекта ADAPT!"

echo ""
echo "Для сборки проекта требуются системные зависимости (build-essential, g++, flex, bison, cmake, swi-prolog, spin, nodejs, npm, graphviz, nlohmann-json3-dev, curl)"
read -p "Хотите установить их автоматически через apt-get? (y/n, по умолчанию y): " install_sys_choice
install_sys_choice=${install_sys_choice:-y}

if [[ "$install_sys_choice" == "y" || "$install_sys_choice" == "Y" || "$install_sys_choice" == "д" || "$install_sys_choice" == "Д" ]]; then
    if command -v apt-get &> /dev/null; then
        echo ""

        echo "Обновление списка пакетов apt..."
        if ! sudo apt-get update; then
            echo "-> Ошибка: Не удалось обновить список пакетов. Проверьте вывод ошибок выше."
            exit 1
        fi

        echo "Установка системных пакетов..."
        if ! sudo apt-get install -y build-essential g++ flex bison cmake swi-prolog spin nodejs npm graphviz nlohmann-json3-dev curl; then
            echo "-> Ошибка: Не удалось установить некоторые пакеты. Проверьте вывод ошибок выше."
            exit 1
        fi
        
        echo "Системные зависимости успешно установлены!"
    else
        echo "-> Утилита apt-get не найдена. Этот шаг поддерживается только для Debian/Ubuntu. Пожалуйста, установите зависимости вручную."
    fi
else
    echo "Пропуск автоматической установки системных зависимостей."
fi

echo ""
echo "Выберите инструмент для управления пакетами Python:"
echo "1) Нативный python3.13"
echo "2) Проектный менеджер uv"
read -p "Ваш выбор (1 или 2, по умолчанию 1): " python_choice
python_choice=${python_choice:-1} 

if [[ "$python_choice" == "2" ]]; then
    PYTHON_TOOL="uv"
    echo "-> Выбран uv"
else
    PYTHON_TOOL="python3.13"
    echo "-> Выбран python3.13"
fi

if ! command -v "$PYTHON_TOOL" &> /dev/null; then
    echo "Утилита $PYTHON_TOOL не найдена."
    read -p "Хотите попытаться установить $PYTHON_TOOL автоматически? (y/n, по умолчанию y): " install_choice
    install_choice=${install_choice:-y}
    
    if [[ "$install_choice" != "y" && "$install_choice" != "Y" && "$install_choice" != "д" && "$install_choice" != "Д" ]]; then
        echo "Установка отменена. Пожалуйста, установите $PYTHON_TOOL вручную."
        exit 1
    fi
    
    echo "Попытка установки $PYTHON_TOOL..."
    
    if [[ "$PYTHON_TOOL" == "uv" ]]; then
        # Установка uv (официальный скрипт установки)
        curl -LsSf https://astral.sh/uv/install.sh | sh

        echo ""
        echo "Установка uv завершена. Необходимо обновить переменные окружения."
        echo "Пожалуйста, выполните команду 'source ~/.bashrc' (или 'source ~/.zshrc', если используете Zsh), либо перезапустите терминал."
        echo "Затем запустите этот скрипт заново."
        exit 0
    elif [[ "$PYTHON_TOOL" == "python3.13" ]]; then
        # Попытка установки python3.13 для Debian/Ubuntu систем
        if command -v apt-get &> /dev/null && command -v apt-cache &> /dev/null; then
            echo "Используется apt-get для установки python3.13..."

            if ! sudo apt-get update; then
                echo "-> Ошибка: Не удалось обновить список пакетов. Проверьте вывод ошибок выше."
                exit 1
            fi
            
            if apt-cache show python3.13 &> /dev/null; then
                if ! sudo apt-get install -y python3.13 python3.13-venv python3.13-dev; then
                    echo "-> Ошибка: Не удалось установить пакеты python3.13. Проверьте вывод ошибок выше."
                    exit 1
                fi
            else
                echo "-> Ошибка: Пакет python3.13 не найден в ваших текущих репозиториях apt."
                echo "Возможно, вам требуется добавить сторонний репозиторий (например, ppa:deadsnakes/ppa на Ubuntu) или установить Python из исходников."
                exit 1
            fi
        else
            echo "-> Ошибка: Автоматическая установка $PYTHON_TOOL поддерживается только для apt. Установите python3.13 вручную."
            exit 1
        fi
    fi

    # Повторная проверка после попытки установки
    if ! command -v "$PYTHON_TOOL" &> /dev/null; then
        echo "-> Ошибка: Не удалось найти или установить $PYTHON_TOOL. Пожалуйста, установите его вручную."
        exit 1
    else
        echo "-> $PYTHON_TOOL успешно установлен!"
    fi
else
    echo "Проверка пройдена: $PYTHON_TOOL уже установлен."
fi

echo ""
echo "Создание виртуального окружения и установка зависимостей..."

if [[ "$PYTHON_TOOL" == "uv" ]]; then
    if [ ! -d ".venv" ]; then
        if ! uv venv --python 3.13 --seed; then
            echo "-> Ошибка: Не удалось создать виртуальное окружение (uv)."
            exit 1
        fi
    else
        echo "-> Виртуальное окружение (.venv) уже существует. Пропускаем создание."
    fi

    source .venv/bin/activate

    if ! pip install -r requirements.txt; then
        echo "-> Ошибка: Не удалось установить зависимости Python. Проверьте вывод ошибок выше."
        exit 1
    fi

    echo "Зависимости Python через uv установлены!"
else
    if [ ! -d ".venv" ]; then
        if ! python3.13 -m venv .venv; then
            echo "-> Ошибка: Не удалось создать виртуальное окружение (python3.13)."
            exit 1
        fi
    else
        echo "-> Виртуальное окружение (.venv) уже существует. Пропускаем создание."
    fi

    source .venv/bin/activate

    if ! pip install -r requirements.txt; then
        echo "-> Ошибка: Не удалось установить зависимости Python. Проверьте вывод ошибок выше."
        exit 1
    fi

    echo "Зависимости Python через python3.13 установлены!"
fi

echo ""
echo "Установка зависимостей nodejs..."

if ! npm install --prefix ./mc-analyzer; then
    echo "-> Ошибка: Не удалось установить зависимости nodejs. Проверьте вывод ошибок выше."
    exit 1
fi
echo "Зависимости nodejs установлены!"

echo ""
echo "Для сборки проекта требуется LLVM 15."
read -p "Хотите установить его автоматически через apt-get? (y/n, по умолчанию y): " llvm_choice
llvm_choice=${llvm_choice:-y}

if [[ "$llvm_choice" == "y" || "$llvm_choice" == "Y" || "$llvm_choice" == "д" || "$llvm_choice" == "Д" ]]; then
    if command -v apt-get &> /dev/null; then
        echo "Установка LLVM 15..."

        if ! sudo apt-get update; then
            echo "-> Ошибка: Не удалось обновить список пакетов. Проверьте вывод ошибок выше."
            exit 1
        fi

        if ! sudo apt-get install -y libllvm-15-ocaml-dev libllvm15t64 llvm-15 llvm-15-dev llvm-15-doc llvm-15-examples llvm-15-runtime clang-15 clang-tools-15 clang-15-doc libclang-common-15-dev libclang-15-dev libclang1-15t64 libclang-rt-15-dev lld-15; then
            echo "-> Ошибка: Не удалось установить LLVM 15. Проверьте вывод ошибок выше. Возможны проблемы с репозиториями."
            echo "-> Если пакетов нет в apt, установите их вручную с официального сайта: https://llvm.org/"
            exit 1
        fi

        echo "LLVM 15 успешно установлен!"
    else
        echo "-> Утилита apt-get не найдена. Этот шаг поддерживается только для Debian/Ubuntu. Пожалуйста, установите LLVM 15 вручную."
    fi
else
    echo "Пропуск автоматической установки LLVM 15."
fi

echo ""
echo "Для сборки проекта требуется фреймворк PhASAR."
read -p "Хотите установить его автоматически? (y/n, по умолчанию y): " phasar_choice
phasar_choice=${phasar_choice:-y}

if [[ "$phasar_choice" == "y" || "$phasar_choice" == "Y" || "$phasar_choice" == "д" || "$phasar_choice" == "Д" ]]; then
    echo "Установка PhASAR..."

    if ! command -v git &> /dev/null; then
        echo "-> Ошибка: Утилита git не найдена. Установите git для продолжения."
        exit 1
    fi

    if ! command -v llvm-config-15 &> /dev/null; then
        echo "-> Ошибка: Утилита llvm-config-15 не найдена. Убедитесь, что LLVM 15 был установлен корректно."
        exit 1
    fi

    echo "Создание директории external_libs для внешних библиотек..."
    mkdir -p external_libs
    cd external_libs || exit 1

    if [ ! -d "phasar" ]; then
        echo "Скачивание исходников PhASAR (релиз v2510)..."
        if ! curl -L https://github.com/secure-software-engineering/phasar/archive/refs/tags/v2510.tar.gz -o phasar.tar.gz; then
            echo "-> Ошибка: Не удалось скачать релиз PhASAR."
            exit 1
        fi
        
        echo "Распаковка исходников..."
        if ! tar -xzf phasar.tar.gz; then
            echo "-> Ошибка: Не удалось распаковать архив PhASAR."
            exit 1
        fi
        
        # Переименовываем папку после распаковки для удобства
        mv phasar-2510 phasar
        rm phasar.tar.gz
    else
        echo "-> Директория phasar уже существует. Пропускаем скачивание."
    fi

    # Переходим в директорию
    cd phasar || exit 1

    echo "Клонирование зависимостей (json, json-schema-validator)..."
    if [ ! -d "external/json" ]; then
        if ! git clone https://github.com/nlohmann/json.git external/json; then
            echo "-> Ошибка: Не удалось склонировать external/json."
            exit 1
        fi
    fi

    if [ ! -d "external/json-schema-validator" ]; then
        if ! git clone https://github.com/pboettch/json-schema-validator.git external/json-schema-validator; then
            echo "-> Ошибка: Не удалось склонировать external/json-schema-validator."
            exit 1
        fi
    fi

    echo "Подготовка bootstrap.sh (отключение git submodule update --init)..."
    sed -i 's/git submodule update --init/# git submodule update --init/g' bootstrap.sh

    LLVM_DIR=$(llvm-config-15 --prefix)
    echo "Получен путь к LLVM 15: $LLVM_DIR"

    echo "Сборка и установка PhASAR... (это может занять продолжительное время)"
    if ! sudo ./bootstrap.sh --install -DLLVM_INSTALL_DIR="$LLVM_DIR"; then
        echo "-> Ошибка: Не удалось собрать и установить PhASAR. Проверьте вывод ошибок выше."
        cd ../..
        exit 1
    fi

    # Возвращаемся обратно в корень проекта
    cd ../..
    echo "PhASAR успешно установлен!"
    
    echo ""
    echo "Необходимо обновить переменные окружения."
    echo "Пожалуйста, выполните команду 'source ~/.bashrc' (или 'source ~/.zshrc', если используете Zsh), либо перезапустите терминал."
    exit 0
else
    echo "Пропуск автоматической установки PhASAR."
fi

echo ""
echo "Все шаги первоначальной настройки завершены."

echo ""
echo "Сборка проекта..."
if ! make; then
    echo "-> Ошибка: Сборка проекта (make) завершилась с ошибкой. Проверьте вывод выше."
    exit 1
fi

echo "Сборка llvm-manager..."
if ! python3 llvm-manager/manager.py rebuild; then
    echo "-> Ошибка: Сборка llvm-manager завершилась с ошибкой. Проверьте вывод выше."
    exit 1
fi

echo ""
echo "Установка завершена!"

echo ""
echo "Для корректной работы проекта требуются переменные окружения (ADAPT_HOME и обновление PATH)."
read -p "Хотите автоматически добавить их в ваш ~/.bashrc / ~/.zshrc? (y/n, по умолчанию y): " env_choice
env_choice=${env_choice:-y}

if [[ "$env_choice" == "y" || "$env_choice" == "Y" || "$env_choice" == "д" || "$env_choice" == "Д" ]]; then
    CURRENT_DIR=$(pwd)
    
    PROFILES=()
    [[ -f "$HOME/.bashrc" ]] && PROFILES+=("$HOME/.bashrc")
    [[ -f "$HOME/.zshrc" ]] && PROFILES+=("$HOME/.zshrc")

    if [ ${#PROFILES[@]} -eq 0 ]; then
        echo "-> Не найдено стандартных файлов профиля (.bashrc или .zshrc)."
        echo "Пожалуйста, добавьте следующие строки в ваш профиль вручную:"
        echo "export ADAPT_HOME=\"$CURRENT_DIR\""
        echo "export PATH=\"\$ADAPT_HOME/bin:\$PATH\""
    else
        for PROFILE_FILE in "${PROFILES[@]}"; do
            if grep -q "ADAPT_HOME=" "$PROFILE_FILE"; then
                echo "-> Переменная ADAPT_HOME уже найдена в $PROFILE_FILE. Пропускаем добавление."
            else
                echo "" >> "$PROFILE_FILE"
                echo "# ADAPT Environment Variables" >> "$PROFILE_FILE"
                echo "export ADAPT_HOME=\"$CURRENT_DIR\"" >> "$PROFILE_FILE"
                echo "export PATH=\"\$ADAPT_HOME/bin:\$PATH\"" >> "$PROFILE_FILE"
                echo "-> Переменные успешно добавлены в $PROFILE_FILE!"
            fi
        done

        echo ""
        echo "Важно: Чтобы применить изменения, перезапустите терминал или выполните команду для вашего текущего профиля, например:"
        echo "source ~/.bashrc  # (или source ~/.zshrc)"
    fi
else
    echo "Пропуск настройки переменных окружения. Пожалуйста, проверьте, что в вашем профиле есть следующие переменные:"
    echo "export ADAPT_HOME=\"$(pwd)\""
    echo "export PATH=\"\$ADAPT_HOME/bin:\$PATH\""
fi
