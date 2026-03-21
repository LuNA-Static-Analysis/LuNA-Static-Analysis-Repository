cmake_minimum_required(VERSION 3.16)

project(BiLangIR VERSION 1.0.0 LANGUAGES C CXX)

# Устанавливаем стандарт C++
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Настройки сборки
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Debug CACHE STRING "Build type" FORCE)
endif()
set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "MinSizeRel" "RelWithDebInfo")

# Поиск LLVM
find_package(LLVM 16 REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

# Настройки для LLVM
include_directories(${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})

# Находим необходимые компоненты LLVM
llvm_map_components_to_libnames(llvm_libs 
    Analysis
    Core
    ExecutionEngine
    InstCombine
    Object
    RuntimeDyld
    ScalarOpts
    Support
    IRReader
    Linker
    BitReader
    BitWriter
    TransformUtils
    native
)

# Настройки компилятора
if(CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    add_compile_options(-g)
    if(NOT HIDE_WARNINGS)
        add_compile_options(-Wall)
        add_compile_options(-Wextra)
    endif()
endif()

# Исходные файлы
set(SOURCES
    src/main.cpp
    src/codegen.cpp
    src/creaters.cpp
)

# Заголовочные файлы
set(HEADERS
    src/codegen.hpp
    src/creaters.hpp
    src/depends.hpp
    src/nodes.hpp
    src/debug.hpp
    src/error_handler.hpp
    src/error_nodes.hpp
)

# Создаем исполняемый файл
add_executable(BiLangIR ${SOURCES} ${HEADERS})

# Связываем с библиотеками LLVM
target_link_libraries(BiLangIR ${llvm_libs})

# Устанавливаем свойства целевого файла
set_target_properties(BiLangIR PROPERTIES
    OUTPUT_NAME "BiLangIR"
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin"
)

# Добавляем include директории
target_include_directories(BiLangIR PRIVATE 
    src
)

# Опциональные цели
add_custom_target(bilangir-clean-all
    COMMAND ${CMAKE_COMMAND} -E remove_directory ${CMAKE_BINARY_DIR}
    COMMENT "Cleaning all BiLangIR build files"
)

# Цель для запуска
add_custom_target(bilangir-run
    COMMAND $<TARGET_FILE:BiLangIR>
    DEPENDS BiLangIR
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    COMMENT "Running BiLangIR"
)

# Цель для тестирования
add_custom_target(bilangir-test
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/test.sh
    DEPENDS BiLangIR
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    COMMENT "Running BiLangIR tests with test script"
)

# Цель для форматирования кода (если установлен clang-format)
find_program(CLANG_FORMAT clang-format)
if(CLANG_FORMAT)
    add_custom_target(bilangir-format
        COMMAND ${CLANG_FORMAT} -i ${SOURCES} ${HEADERS}
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        COMMENT "Formatting BiLangIR source code with clang-format"
    )
endif()

# Вывод информации о сборке
message(STATUS "=== BiLangIR Configuration ===")
message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")
message(STATUS "C++ compiler: ${CMAKE_CXX_COMPILER}")
message(STATUS "C++ standard: ${CMAKE_CXX_STANDARD}")
message(STATUS "LLVM version: ${LLVM_PACKAGE_VERSION}")
message(STATUS "==============================")
