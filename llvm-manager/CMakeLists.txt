cmake_minimum_required(VERSION 3.16)

# Опция для использования LLVM компилятора (должна быть ДО project())
option(USE_LLVM_COMPILER "Use LLVM/Clang compiler" ON)
set(LLVM_INSTALL_DIR "/usr/local/llvm-16" CACHE PATH "LLVM installation directory")

# Установка компилятора ДО вызова project() если требуется
if(USE_LLVM_COMPILER)
    if(EXISTS "${LLVM_INSTALL_DIR}/bin/clang++")
        set(CMAKE_CXX_COMPILER "${LLVM_INSTALL_DIR}/bin/clang++")
        set(CMAKE_C_COMPILER "${LLVM_INSTALL_DIR}/bin/clang")
        message(STATUS "Using LLVM compiler from ${LLVM_INSTALL_DIR}")
    else()
        message(WARNING "LLVM compiler not found at ${LLVM_INSTALL_DIR}, using system default")
    endif()
endif()

project(llvm-manager VERSION 1.0.0 LANGUAGES C CXX)

# Настройки проекта
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Настройки сборки по умолчанию
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Debug CACHE STRING "Build type" FORCE)
endif()

# Опции проекта
option(BUILD_BILANGIR "Build BiLangIR" ON)
option(BUILD_PHASAR_ADVISOR "Build PhASAR-advisor" ON)
option(HIDE_WARNINGS "Hide compiler warnings" ON)

# Добавляем подпроекты
if(BUILD_BILANGIR)
    add_subdirectory(BiLangIR)
endif()

if(BUILD_PHASAR_ADVISOR)
    add_subdirectory(PhASAR-advisor)
endif()

# Вывод информации о конфигурации
message(STATUS "=== llvm-manager Configuration ===")
message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")
message(STATUS "C++ compiler: ${CMAKE_CXX_COMPILER}")
message(STATUS "C++ standard: ${CMAKE_CXX_STANDARD}")
message(STATUS "Build BiLangIR: ${BUILD_BILANGIR}")
message(STATUS "Build PhASAR-advisor: ${BUILD_PHASAR_ADVISOR}")
message(STATUS "Hide warnings: ${HIDE_WARNINGS}")
message(STATUS "====================================")
