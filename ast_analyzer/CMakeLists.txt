cmake_minimum_required(VERSION 3.10)

# set the project name
project(test_ast VERSION 0.1)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED True)

set(LLVM_LINK_COMPONENTS
  Support
 )

add_executable(main cpp_func_analyzer.hpp)

target_link_libraries(main
  PRIVATE
  clangAST
  clangBasic
  clangFrontend
  clangSerialization
  clangTooling
  )