#pragma once

#include <map>
#include <string>
#include <vector>

#include "enums.hpp"
#include "../../parser/ast.hpp"

// this structure represents a declared argument of a sub or an import
// every argument has a name and a type in LuNA
struct DeclaredArg {
    std::string name;
    ValueType type;

    DeclaredArg(std::string iName, ValueType iType) : name(iName), type(iType) {};
};

struct CFDeclaration {
    std::string name;
    CFType type;
    std::vector<DeclaredArg> declaredArgs;
    block* cfBlock;
    int line;
    bool isUsed;

    CFDeclaration(std::string name, CFType type, std::vector<DeclaredArg> declaredArgs, block* cfBlock, int line) :
          name(name), type(type), declaredArgs(declaredArgs), cfBlock(cfBlock), line(line), isUsed(false) {};
};
