#pragma once

#include <vector>
#include <string>

#include "declaration.hpp"

class Vertex;

class Identifier;

class BaseDFName;

std::vector<std::string> REPORTS = {};
std::set<BaseDFName*> BASENAMES = {}; // all names declared after the keyword "df"
std::vector<Vertex*> VERTICES = {};
std::map<std::string, CFDeclaration*> CFDECLARATIONS = {};

void logInternalError(std::string message)
{
    std::cout << "INTERNAL ERROR: " << message << std::endl;
}
