#include <catch/catch.hpp>

#include "../../cppparser/pub/cppparser.h"

#include <boost/filesystem.hpp>

namespace fs = boost::filesystem;

int main() {
  CppParser  parser;
  const auto testFilePath = fs::path(__FILE__).parent_path() / "test-files/hello-world.cpp";
  const auto ast          = parser.parseFile(testFilePath.string());
}
