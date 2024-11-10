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
        map.insert(std::make_pair("error_code", lunaCode));
        map.insert(std::make_pair("details", details));
        return createJson(map);
    }

    static std::string createCallStackEntry(
        std::string fileName,
        std::string line,
        std::string cfName
    )
    {
        std::map<std::string, std::string> map = {};
        map.insert(std::make_pair("file", fileName));
        map.insert(std::make_pair("line", line));
        map.insert(std::make_pair("name", cfName));
        return createJson(map);
    }

    static std::string createCallStack(
            std::vector<std::string> callStackEntries
    ){
        return createArray(callStackEntries);
    }

    static std::string createCallstackFromVertex(
        Vertex* vertex
    ){
        if (vertex == nullptr)
            return "[]";
        
        std::vector<std::string> callstackEntries = {};
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
            Identifier* identifier
    ){
        std::map<std::string, std::string> map = {};
        map.insert(std::make_pair("name", identifier->getName()));
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

        map.insert(std::make_pair("declared", createArray(declCallstack)));
        map.insert(std::make_pair("initialized", createArray(defCallstack)));
        map.insert(std::make_pair("used", createArray(useCallstack)));
        return createJson(map);
    }

    static std::string createDFRef(
            std::string df, // df as json
            std::string localExpression, // local expression
            std::string trueExpression, // real expression
            std::string callStack // callstack as json
    ){
        std::map<std::string, std::string> map = {};
        map.insert(std::make_pair("df", df));
        map.insert(std::make_pair("local", localExpression));
        map.insert(std::make_pair("true", trueExpression));
        map.insert(std::make_pair("where", callStack));
        return createJson(map);
    }

    static std::string createIndexRange(
            std::string dfRef,
            std::string forLoop, // "for" that creates indices range
            std::string step, // "for" step; expression as json
            std::string offset // "for" offset; expression as json
    ){
        std::map<std::string, std::string> map = {};
        map.insert(std::make_pair("df_ref", dfRef));
        map.insert(std::make_pair("loop", forLoop));
        map.insert(std::make_pair("step", step));
        map.insert(std::make_pair("offset", offset));
        return createJson(map);
    }

    static std::string createCF(
            std::string name, // cf name
            std::string type, // "struct" (sub) or "extern" // import
            std::string fileName,
            int line
    ){
        std::map<std::string, std::string> map = {};
        map.insert(std::make_pair("name", name));
        map.insert(std::make_pair("type", type));
        map.insert(std::make_pair("file", fileName));
        map.insert(std::make_pair("line", std::to_string(line)));
        return createJson(map);
    }

    static std::string createFor(
            std::string iteratorName,
            std::string startExpression,
            std::string endExpression,
            std::string callStack
    ){
        std::map<std::string, std::string> map = {};
        map.insert(std::make_pair("var", iteratorName));
        map.insert(std::make_pair("first", startExpression));
        map.insert(std::make_pair("lase", endExpression));
        map.insert(std::make_pair("where", callStack));
        return createJson(map);
    }

    //================================= ERROR REPORTS =============================================

    // ******************************** SYNTAX **************************************************

    // compile-time wrong types on CF call
    static std::string createSYN1(
        std::string expression,
        Vertex* vertex
    ){
        std::map<std::string, std::string> map = {};
        map.insert(std::make_pair("expression", expression));
        if(vertex->getVertexType() == importVF)
            map.insert(std::make_pair("cf", createCF(
                vertex->getName(),
                "extern",
                vertex->getFileName(),
                vertex->getLine()
            )));
        else
            map.insert(std::make_pair("cf", createCF(
                vertex->getName(),
                "struct",
                vertex->getFileName(),
                vertex->getLine()
            )));
        map.insert(std::make_pair("callstack", createCallstackFromVertex(vertex)));
        return createReport("SYN1", createJson(map));
    }

    // non-existing CF called
    static std::string createSYN2(
        std::string fileName,
        int line,
        std::string cfName
    ){
        // details: df list
        std::map<std::string, std::string> map = {};
        map.insert(std::make_pair("call_stack_entry", createCallStackEntry(fileName, std::to_string(line), cfName)));
        return createReport("SYN2", createJson(map));
    }

    // wrong amount of args on CF call
    static std::string createSYN3(
        Vertex* vertex
    ){
        std::map<std::string, std::string> map = {};
        map.insert(std::make_pair("cf", createCF(
                vertex->getName(),
                "extern",
                vertex->getFileName(),
                vertex->getLine()
            )));
        map.insert(std::make_pair("call_stack_entry", createCallStackEntry(vertex->getFileName(), std::to_string(vertex->getLine()), vertex->getName())));
        return createReport("SYN3", createJson(map));
    }

    // duplicate base names declared TODO what about other names? counters while, counters if, vars let
    static std::string createSYN8(
        std::vector<std::string> dfs
    ){
        // details: df list
        std::map<std::string, std::string> map = {};
        map.insert(std::make_pair("dfs", createArray(dfs)));
        return createReport("SYN8", createJson(map));
    }

    // attempt to use undeclared identifier
    static std::string createSYN9(
        Identifier* identifier
    ){
        // details: df
        std::map<std::string, std::string> map = {};
        map.insert(std::make_pair("df", createDF(identifier)));
        return createReport("SYN9", createJson(map));
    }

    // attempt to index non-Name
    static std::string createSYN11(
        std::string expression,
        Vertex* vertex
    ){
        std::map<std::string, std::string> map = {};
        map.insert(std::make_pair("expression", expression));
        map.insert(std::make_pair("callstack", createCallstackFromVertex(vertex)));
        return createReport("SYN11", createJson(map));
    }

    // ***************************************** SEMANTIC **************************************************

    // multiple DF initialization
    static std::string createSEM2(
        Identifier* identifier
    ){
        // details: df
        std::map<std::string, std::string> map = {};
        map.insert(std::make_pair("df", createDF(
            identifier
        )));
        return createReport("SEM2", createJson(map));
    }

    // attempt to use uninitialized DF (outside of a loop TODO this is weird, but we'll see)
    static std::string createSEM3_1(
        Identifier* identifier
    ){
        // details: df
        std::map<std::string, std::string> map = {};
        map.insert(std::make_pair("df", createDF(identifier)));
        return createReport("SEM3_1", createJson(map));
    }

    // DF is initialized, but not used
    static std::string createSEM4(
        Identifier* identifier
    ){
        // details: df
        std::map<std::string, std::string> map = {};
        map.insert(std::make_pair("df", createDF(identifier)));
        return createReport("SEM4", createJson(map));
    }

    // "if" condition is constant
    static std::string createSEM5(
        bool type,
        std::string condition,
        std::string fileName,
        int line,
        std::string cfName
    ){
        std::map<std::string, std::string> map = {};
        if (type)
            map.insert(std::make_pair("type", "true"));
        else
            map.insert(std::make_pair("type", "false"));
        map.insert(std::make_pair("condition", condition));
        map.insert(std::make_pair("where", createCallStackEntry(fileName, std::to_string(line), cfName)));
        return createReport("SEM5", createJson(map));
    }

private:

    JsonReporter(){}

};
