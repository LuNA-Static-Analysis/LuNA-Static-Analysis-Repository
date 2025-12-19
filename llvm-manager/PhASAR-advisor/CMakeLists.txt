cmake_minimum_required(VERSION 3.16)

project(PhASAR-advisor VERSION 1.0.0 LANGUAGES C CXX)

# Устанавливаем стандарт C++
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Настройки сборки
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Debug CACHE STRING "Build type" FORCE)
endif()
set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "MinSizeRel" "RelWithDebInfo")

# Поиск PhASAR
find_package(phasar REQUIRED)

message(STATUS "Found PhASAR")

# Настройки компилятора
if(CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    add_compile_options(-g)
    add_compile_options(-Wall)
    add_compile_options(-Wextra)
endif()

# Исходные файлы
set(SOURCES
    src/main.cpp
    src/analyses/BasicInfo.cpp
    src/analyses/TaintAnalysis.cpp
    src/analyses/ConstantPropagation.cpp
    src/analyses/UninitializedVariables.cpp
    src/analyses/TypeAnalysis.cpp
    src/analyses/CallGraph.cpp
    src/analyses/AliasInfo.cpp
    src/analyses/CodeComplexity.cpp
)

# Заголовочные файлы
set(HEADERS
    src/dependes.hpp
)

# Создаем исполняемый файл
add_executable(PhASAR-advisor ${SOURCES} ${HEADERS})

# Связываем с библиотеками PhASAR
target_link_libraries(PhASAR-advisor phasar)

# Устанавливаем свойства целевого файла
set_target_properties(PhASAR-advisor PROPERTIES
    OUTPUT_NAME "PhASAR-advisor"
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin"
)

# Добавляем include директории
target_include_directories(PhASAR-advisor PRIVATE 
    src
)

# Опциональные цели
add_custom_target(phasar-advisor-clean-all
    COMMAND ${CMAKE_COMMAND} -E remove_directory ${CMAKE_BINARY_DIR}
    COMMENT "Cleaning all PhASAR-advisor build files"
)

# Цель для запуска
add_custom_target(phasar-advisor-run
    COMMAND $<TARGET_FILE:PhASAR-advisor>
    DEPENDS PhASAR-advisor
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    COMMENT "Running PhASAR-advisor"
)

# Цель для форматирования кода (если установлен clang-format)
find_program(CLANG_FORMAT clang-format)
if(CLANG_FORMAT)
    add_custom_target(phasar-advisor-format
        COMMAND ${CLANG_FORMAT} -i ${SOURCES} ${HEADERS}
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        COMMENT "Formatting PhASAR-advisor source code with clang-format"
    )
endif()

# Установка
install(TARGETS PhASAR-advisor
    RUNTIME DESTINATION bin
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
)

# Вывод информации о сборке
message(STATUS "=== PhASAR-advisor Configuration ===")
message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")
message(STATUS "C++ compiler: ${CMAKE_CXX_COMPILER}")
message(STATUS "C++ standard: ${CMAKE_CXX_STANDARD}")
message(STATUS "====================================")
