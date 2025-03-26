#pragma once

#include "enums.hpp"
#include "vertices.hpp"

#include <vector>
#include <map>
#include <string>

class JsonReporter {

public:

    static std::string createJson(
        std::map<std::string, std::string> jsonObjects
    ){
        if (jsonObjects.size() == 0)
            return "{}";
        std::string result = "{";
        for (auto l: jsonObjects){
            if (l.second[0] == '{' || l.second[0] == '['){ // complex object
                result += 
                    "\"" + l.first + "\": " + l.second + ",";
            } else { // simple object
                result += 
                    "\"" + l.first + "\": \"" + l.second + "\",";
            }  
        }
        return result.substr(0, result.size() - 1) + "}";
    }

    static std::string createArray(
        std::vector<std::string> jsonObjects
    ){
        std::string result = "[";
        for (auto jo: jsonObjects){
            result += (jo + ",");
        }
        if (jsonObjects.size() != 0)
            result[result.size() - 1] = ']';
        else
            result += "]";
        return result;
        
    }

    static std::string createReport(std::string lunaCode, std::string details){
        std::map<std::string, std::string> map = {};
        map.insert( { "error_code", lunaCode } );
        map.insert( { "details", details } );
        return createJson(map);
    }

    static std::string createCallStackEntry(
        std::string fileName,
        std::string line,
        std::string cfName
    )
    {
        std::map<std::string, std::string> map = {};
        map.insert( { "file", fileName } );
        map.insert( { "line", line } );
        map.insert( { "name", cfName } );
        return createJson(map);
    }

    static std::string createCallStack(
            std::vector<std::string> callStackEntries
    ){
        return createArray(callStackEntries);
    }

    static std::string createCallstackFromVertex(
        Vertex* const ipVertex
    ){
        if (ipVertex == nullptr)
            return "[]";
        
        std::vector<std::string> callstackEntries = {};
        Vertex* vertex = ipVertex;
        while(vertex != nullptr){
            callstackEntries.push_back(JsonReporter::createCallStackEntry(
                vertex->getFileName(),
                std::to_string(vertex->getLine()),
                vertex->getName()
            ));
            vertex = vertex->getParent();
        }
        return JsonReporter::createCallStack(callstackEntries);
    }

    static std::string createDF(
        Identifier* const identifier
    ){
        std::map<std::string, std::string> map = {};
        map.insert( { "name", identifier->getName() } );
        // decl
        std::vector<std::string> declCallstack = {};
        declCallstack.push_back(createCallstackFromVertex(identifier->getVertex()));
        // def
        std::vector<std::string> defCallstack = {};
        for (auto def: identifier->getDefSet()){
            defCallstack.push_back(createCallstackFromVertex(def));
        }
        // use
        std::vector<std::string> useCallstack = {};
        for (auto use: identifier->getUseSet()){
            useCallstack.push_back(createCallstackFromVertex(use));
        }

        map.insert( { "declared", createArray(declCallstack) } );
        map.insert( { "initialized", createArray(defCallstack) } );
        map.insert( { "used", createArray(useCallstack) } );
        return createJson(map);
    }

    static std::string createDFRef(
            std::string df, // df as json
            std::string localExpression, // local expression
            std::string trueExpression, // real expression
            std::string callStack // callstack as json
    ){
        std::map<std::string, std::string> map = {};
        map.insert( { "df", df } );
        map.insert( { "local", localExpression } );
        map.insert( { "true", trueExpression } );
        map.insert( { "where", callStack } );
        return createJson(map);
    }

    static std::string createIndexRange(
            std::string dfRef,
            std::string forLoop, // "for" that creates indices range
            std::string step, // "for" step; expression as json
            std::string offset // "for" offset; expression as json
    ){
        std::map<std::string, std::string> map = {};
        map.insert( { "df_ref", dfRef } );
        map.insert( { "loop", forLoop } );
        map.insert( { "step", step } );
        map.insert( { "offset", offset } );
        return createJson(map);
    }

    static std::string createCF(
        CFDeclaration* const cfDeclaration
    ){
        std::map<std::string, std::string> map = {};
        if (cfDeclaration == nullptr) {
            map.insert( { "name", "INTERNAL_ERROR" } );
            map.insert( { "type", "INTERNAL_ERROR" } );
            map.insert( { "file", "INTERNAL_ERROR" } );
            map.insert( { "line", "INTERNAL_ERROR" } );
        } else {
            map.insert( { "name", cfDeclaration->name } );
            map.insert( { "type", cfDeclaration->type == subCF ? "struct" : "extern" } );
            map.insert( { "file", cfDeclaration->fileName } );
            map.insert( { "line", std::to_string(cfDeclaration->line) } );
        }
        return createJson(map);
    }

    static std::string createFor(
            std::string iteratorName,
            std::string startExpression,
            std::string endExpression,
            std::string callStack
    ){
        std::map<std::string, std::string> map = {};
        map.insert( { "var", iteratorName } );
        map.insert( { "first", startExpression } );
        map.insert( { "last", endExpression } );
        map.insert( { "where", callStack } );
        return createJson(map);
    }

    static std::string createIdentifier(
        Identifier* identifier
    ){
        std::map<std::string, std::string> map = {};
        map.insert( { "name", identifier->getName() } );
        auto vertex = identifier->getVertex();
        map.insert( { "call_stack_entry", createCallStackEntry(vertex->getFileName(), std::to_string(vertex->getLine()), vertex->getName()) } );
        return createJson(map);
    }

    static std::string createIdentifierLazy(
        std::string identifierName,
        Vertex* vertex
    ){
        std::map<std::string, std::string> map = {};
        map.insert( { "name", identifierName } );
        map.insert( { "call_stack_entry", createCallStackEntry(vertex->getFileName(), std::to_string(vertex->getLine()), vertex->getName()) } );
        return createJson(map);
    }

    //================================= ERROR REPORTS =============================================

    // ******************************** SYNTAX **************************************************

    static std::string createSYN1(
        std::string expression,
        Vertex* const vertex,
        CFDeclaration* const cfDeclaration
    ){
        std::map<std::string, std::string> map = {};
        std::replace( expression.begin(), expression.end(), '\"', '\'');
        map.insert( { "expression", expression } );
        std::string cfType = vertex->getVertexType() == importVF ? "extern" : "struct";
        map.insert( { "cf", createCF(cfDeclaration) } );
        map.insert( { "call_stack_entry", createCallStackEntry(vertex->getFileName(), std::to_string(vertex->getLine()), vertex->getName()) } );
        return createReport("SYN1", createJson(map));
    }

    static std::string createSYN2(
        std::string fileName,
        int line,
        std::string cfName
    ){
        std::map<std::string, std::string> map = {};
        map.insert( { "call_stack_entry", createCallStackEntry(fileName, std::to_string(line), cfName) } );
        return createReport("SYN2", createJson(map));
    }

    static std::string createSYN3(
        Vertex* const vertex,
        CFDeclaration* const cfDeclaration
    ){
        std::map<std::string, std::string> map = {};
        std::string cfType = vertex->getVertexType() == importVF ? "extern" : "struct";
        map.insert( { "cf", createCF(cfDeclaration) } );
        map.insert( { "call_stack_entry", createCallStackEntry(vertex->getFileName(), std::to_string(vertex->getLine()), vertex->getName()) } );
        return createReport("SYN3", createJson(map));
    }

    static std::string createSYN5_2(
        Identifier* const identifier
    ){
        if (identifier->getClass() != letNameClass)
            std::cout << "INTERNAL ERROR: SYN5.2 got wrong type" << std::endl;
        std::map<std::string, std::string> map = {};
        map.insert( { "identifier", createIdentifier(identifier) } );
        return createReport("SYN5.2", createJson(map));
    }

    static std::string createSYN5_3(
        Identifier* const identifier
    ){
        if (identifier->getClass() != baseDFNameClass)
            std::cout << "INTERNAL ERROR: SYN5.3 got wrong type" << std::endl;
        std::map<std::string, std::string> map = {};
        map.insert( { "identifier", createIdentifier(identifier) } );
        return createReport("SYN5.3", createJson(map));
    }

    static std::string createSYN5_4(
        CFDeclaration* const cfDeclaration
    ){
        if (cfDeclaration->type != importCF)
            std::cout << "INTERNAL ERROR: SYN5.4 got wrong type" << std::endl;
        std::map<std::string, std::string> map = {};
        map.insert( { "cf", createCF(cfDeclaration) } );
        return createReport("SYN5.4", createJson(map));
    }

    static std::string createSYN5_5(
        CFDeclaration* const cfDeclaration
    ){
        if (cfDeclaration->type != subCF)
            std::cout << "INTERNAL ERROR: SYN5.5 got wrong type" << std::endl;
        std::map<std::string, std::string> map = {};
        map.insert( { "cf", createCF(cfDeclaration) } );
        return createReport("SYN5.5", createJson(map));
    }

    static std::string createSYN5_6(
        Identifier* const identifier
    ){
        if (identifier->getClass() != mutableArgNameClass && identifier->getClass() != immutableArgNameClass)
            std::cout << "INTERNAL ERROR: SYN5.6 got wrong type" << std::endl;
        std::map<std::string, std::string> map = {};
        map.insert( { "identifier", createIdentifier(identifier) } );
        return createReport("SYN5.6", createJson(map));
    }

    static std::string createSYN5_7(
        Identifier* const identifier
    ){
        if (identifier->getClass() != forIteratorNameClass)
            std::cout << "INTERNAL ERROR: SYN5.7 got wrong type" << std::endl;
        std::map<std::string, std::string> map = {};
        map.insert( { "identifier", createIdentifier(identifier) } );
        return createReport("SYN5.7", createJson(map));
    }

    static std::string createSYN5_8(
        Identifier* const identifier
    ){
        if (identifier->getClass() != whileIteratorNameClass)
            std::cout << "INTERNAL ERROR: SYN5.8 got wrong type" << std::endl;
        std::map<std::string, std::string> map = {};
        map.insert( { "identifier", createIdentifier(identifier) } );
        return createReport("SYN5.8", createJson(map));
    }

    static std::string createSYN6_1(
        CFDeclaration* const cfDeclarationOrigin,
        CFDeclaration* const cfDeclarationDuplicate
    ){
        if (cfDeclarationOrigin->type != importCF)
            std::cout << "INTERNAL ERROR: SYN6.1 got wrong type" << std::endl;
        std::map<std::string, std::string> map = {};
        map.insert( { "cfs", createArray({ createCF(cfDeclarationOrigin), createCF(cfDeclarationDuplicate) }) } );
        return createReport("SYN6.1", createJson(map));
    }

    static std::string createSYN6_2(
        CFDeclaration* const cfDeclarationOrigin,
        CFDeclaration* const cfDeclarationDuplicate
    ){
        if (cfDeclarationOrigin->type != subCF)
            std::cout << "INTERNAL ERROR: SYN6.2 got wrong type" << std::endl;
        std::map<std::string, std::string> map = {};
        map.insert( { "cfs", createArray({ createCF(cfDeclarationOrigin), createCF(cfDeclarationDuplicate) }) } );
        return createReport("SYN6.2", createJson(map));
    }

    static std::string createSYN7(){
        std::map<std::string, std::string> map = {};
        return createReport("SYN7", createJson(map));
    }

    static std::string createSYN8_1(
        std::string identifierName,
        Vertex* vertex
    ){
        std::map<std::string, std::string> map = {};
        map.insert( { "identifier", createIdentifierLazy(identifierName, vertex) } );
        return createReport("SYN8.1", createJson(map));
    }

    static std::string createSYN8_2(
        std::string identifierName,
        Vertex* vertex
    ){
        std::map<std::string, std::string> map = {};
        map.insert( { "identifier", createIdentifierLazy(identifierName, vertex) } );
        return createReport("SYN8.2", createJson(map));
    }

    static std::string createSYN8_3(
        std::string identifierName,
        Vertex* vertex
    ){
        std::map<std::string, std::string> map = {};
        map.insert( { "identifier", createIdentifierLazy(identifierName, vertex) } );
        return createReport("SYN8.3", createJson(map));
    }

    static std::string createSYN8_4(
        std::string identifierName,
        Vertex* vertex
    ){
        std::map<std::string, std::string> map = {};
        map.insert( { "identifier", createIdentifierLazy(identifierName, vertex) } );
        return createReport("SYN8.4", createJson(map));
    }

    static std::string createSYN8_5(
        std::string identifierName,
        Vertex* vertex
    ){
        std::map<std::string, std::string> map = {};
        map.insert( { "identifier", createIdentifierLazy(identifierName, vertex) } );
        return createReport("SYN8.5", createJson(map));
    }

    static std::string createSYN9(
        Identifier* const identifier
    ){
        std::map<std::string, std::string> map = {};
        map.insert( { "identifier", createIdentifier(identifier) } );
        return createReport("SYN9", createJson(map));
    }

    static std::string createSYN11(
        std::string expression,
        Vertex* const vertex
    ){
        std::map<std::string, std::string> map = {};
        map.insert( { "expression", expression } );
        std::replace( expression.begin(), expression.end(), '\"', '\'');
        map.insert( { "call_stack", createCallstackFromVertex(vertex) } );
        return createReport("SYN11", createJson(map));
    }

    // ***************************************** SEMANTIC **************************************************

    static std::string createSEM2_1(
        Identifier* const identifier
    ){
        std::map<std::string, std::string> map = {};
        map.insert( { "initialized", createDFRef("todo", "todo", "todo", "todo") } );//todo
        map.insert( { "other_initializations", createArray( { } )});//todo
        return createReport("SEM2.1", createJson(map));
    }

    // attempt to use uninitialized DF (outside of a loop TODO this is weird, but we'll see)
    static std::string createSEM3_1(
        Identifier* const identifier
    ){
        std::map<std::string, std::string> map = {};
        map.insert( { "df", createDF(identifier) } );
        return createReport("SEM3.1", createJson(map));
    }

    static std::string createSEM3_2(
        std::vector<Vertex*> loop
    ){
        std::map<std::string, std::string> map = {};
        for (int i = 0; i < loop.size(); i++) {
            map.insert( { "vertex" + std::to_string(i + 1), createCallStackEntry(loop[i]->getFileName(), std::to_string(loop[i]->getLine()), loop[i]->getName()) } );
        }
        return createReport("SEM3.2", createJson(map));
    }

    static std::string createSEM4(
        Identifier* const identifier
    ){
        std::map<std::string, std::string> map = {};
        map.insert( { "df", createDF(identifier) } );
        return createReport("SEM4", createJson(map));
    }

    static std::string createSEM5(
        bool type,
        std::string condition,
        std::string fileName,
        int line,
        std::string cfName
    ){
        std::map<std::string, std::string> map = {};
        if (type)
            map.insert( { "type", "true" } );
        else
            map.insert( { "type", "false" } );
        map.insert( { "condition", condition } );
        map.insert( { "where", createCallStackEntry(fileName, std::to_string(line), cfName) } );
        return createReport("SEM5", createJson(map));
    }

    static std::string createSEM8(
        Vertex* const firstCaller,
        Vertex* const secondCaller
    ){
        //NOT SYNCHRONIZED TODO
        std::map<std::string, std::string> map = {};
        map.insert( { "first", createCallStackEntry(firstCaller->getFileName(), std::to_string(firstCaller->getLine()), firstCaller->getName()) } );
        map.insert( { "second", createCallStackEntry(secondCaller->getFileName(), std::to_string(secondCaller->getLine()), secondCaller->getName()) } );
        return createReport("SEM8", createJson(map));
    }

private:

    JsonReporter(){}

};
