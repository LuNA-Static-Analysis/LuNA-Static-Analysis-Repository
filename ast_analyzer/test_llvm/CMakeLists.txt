cmake_minimum_required(VERSION 3.22)

include("/home/maxwell/clang-llvm/llvm-project/clang/cmake/modules/AddClang.cmake")

set(LLVM_LINK_COMPONENTS
  Support
  )

add_clang_executable(find-class-decls FindClassDecls.cpp)

target_link_libraries(find-class-decls
  PRIVATE
  clangAST
  clangBasic
  clangFrontend
  clangSerialization
  clangTooling
  )