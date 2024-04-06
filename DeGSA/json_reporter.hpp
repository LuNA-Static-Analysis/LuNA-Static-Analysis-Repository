#pragma once

#include "enums.hpp"

#include <vector>
#include <map>
#include <string>

class JsonReporter {

public:

    std::string createJson(
        std::map<std::string, std::string> jsonObjects
    );

    std::string createArray(
        std::vector<std::string> jsonObjects
    );

    std::string createReport(
        std::string lunaCode,
        std::string details
    );

    std::string createCallStackEntry(
        std::string fileName,
        std::string line,
        std::string cfName
    );

    std::string createCallStack(
        std::vector<std::string> callStackEntries
    );

    std::string createDF(
        std::string name,
        std::vector<std::string> callStackDeclarations,
        std::vector<std::string> callStackDefs,
        std::vector<std::string> callStackUses
    );

    std::string createExpression(ExpressionType expressionType,
        std::string name, // name = base name
        std::vector<std::string> indices, // expressions in indices
        std::string value // value = value of the constant
    );

    std::string createDFRef(
        std::string df, // df as json
        std::string forLoop, // "for" that initializes DF (for as json)
        std::string step, // "for" step; expression as json
        std::string callStack // callstack as json
    );

    std::string createIndexRange( // also serves as indices range
        std::string dfRef,
        std::string forLoop, // "for" that creates indices range
        std::string step, // "for" step; expression as json
        std::string offset // "for" offset; expression as json
    );

    std::string createCF(
        std::string name, // cf name
        std::string type, // "struct" (sub) or "extern" // import
        std::string fileName,
        std::string line
    );

    std::string createFor(
        std::string iteratorName,
        std::string startExpression,
        std::string endExpression,
        std::string callStack
    );

    JsonReporter();

};
