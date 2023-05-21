cmake_minimum_required(VERSION 3.10)

# set the project name
project(test_ast VERSION 0.1)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# set(CMAKE_CXX_FLAGS "${CMAKE_C_FLAGS} -fsanitize=address")

add_executable(
    main 
    lex.yy.cpp
    grammar.tab.cpp 
    ast_analyzer.cpp 
    main.cpp
)

include(FetchContent)
FetchContent_Declare(
  googletest
  URL https://github.com/google/googletest/archive/609281088cfefc76f9d0ce82e1ff6c30cc3591e5.zip
)
# For Windows: Prevent overriding the parent project's compiler/linker settings
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(googletest)

enable_testing()

add_executable(
    test_ast
    lex.yy.cpp
    grammar.tab.cpp 
    ast_analyzer.cpp 
    ast_test.cpp
)

target_link_libraries(
  test_ast
  gtest_main
)

if ( CMAKE_COMPILER_IS_GNUCC )
  target_compile_options(test_ast PRIVATE "-Wall" "-Wextra")
endif()
if ( MSVC )
  target_compile_options(test_ast PRIVATE "/W4")
endif()

include(GoogleTest)
gtest_discover_tests(test_ast)
