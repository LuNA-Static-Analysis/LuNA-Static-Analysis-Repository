#pragma once

#include <vector>
#include <string>
#include <optional>
#include <set>

#include "declaration.hpp"

class Vertex;

class Identifier;

class BaseDFName;

class Expression;

std::vector<std::string> REPORTS = {};
std::set<BaseDFName*> BASENAMES = {}; // all names declared after the keyword "df"
std::vector<Vertex*> VERTICES = {};
std::map<std::string, CFDeclaration*> CFDECLARATIONS = {};

void logInternalError(std::string message)
{
    std::cout << "INTERNAL ERROR: " << message << std::endl;
}

// yes, this is lazy
struct TrueIndexedDFData {
    public:
        BaseDFName* trueBaseName = nullptr;
        std::vector<Expression*> indicesExpressions = {};

        TrueIndexedDFData(BaseDFName* iTrueBaseName, std::vector<Expression*> iIndicesExpressions) :
            trueBaseName(iTrueBaseName), indicesExpressions(iIndicesExpressions) {
            indicesExpressions = {};
            for (auto exp : iIndicesExpressions) {
                indicesExpressions.push_back(exp);
            }
        };
};
