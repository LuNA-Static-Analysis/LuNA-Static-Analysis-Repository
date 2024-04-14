#pragma once

#include "enums.hpp"

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

    static std::string createDF(
            std::string name,
            std::string callStackDeclarations,
            std::string callStackInitializations,
            std::string callStackUses
    ){
        std::map<std::string, std::string> map = {};
        map.insert(std::make_pair("name", name));
        map.insert(std::make_pair("declared", callStackDeclarations));
        map.insert(std::make_pair("initialized", callStackInitializations));
        map.insert(std::make_pair("used", callStackUses));
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
    //todo all of the above should be private

    static std::string create2(
        std::string fileName,
        int line,
        std::string cfName
    ){
        // details: df list
        std::map<std::string, std::string> map = {};
        map.insert(std::make_pair("call_stack_entry", createCallStackEntry(fileName, std::to_string(line), cfName)));
        return createReport("LUNA02", createJson(map));
    }

    static std::string create3(
        std::string dfName,
        std::string declCallstack,
        std::string defCallstack,
        std::string useCallstack
    ){
        // details: df
        std::map<std::string, std::string> map = {};
        map.insert(std::make_pair("df", createDF(dfName, declCallstack, defCallstack, useCallstack)));
        return createReport("LUNA03", createJson(map));
    }

    static std::string create5(
        std::string dfName,
        std::string declCallstack,
        std::string defCallstack,
        std::string useCallstack
    ){
        // details: df
        std::map<std::string, std::string> map = {};
        map.insert(std::make_pair("df", createDF(dfName, declCallstack, defCallstack, useCallstack)));
        return createReport("LUNA05", createJson(map));
    }

    static std::string create10(
        std::string dfName,
        std::string declCallstack,
        std::string defCallstack,
        std::string useCallstack
    ){
        // details: df
        std::map<std::string, std::string> map = {};
        map.insert(std::make_pair("df", createDF(dfName, declCallstack, defCallstack, useCallstack)));
        return createReport("LUNA10", createJson(map));
    }

    static std::string create13(
        std::vector<std::string> dfs
    ){
        // details: df list
        std::map<std::string, std::string> map = {};
        map.insert(std::make_pair("dfs", createArray(dfs)));
        return createReport("LUNA13", createJson(map));
    }

    static std::string create14(
        std::string dfName,
        std::string declCallstack,
        std::string defCallstack,
        std::string useCallstack
    ){
        // details: df
        std::map<std::string, std::string> map = {};
        map.insert(std::make_pair("df", createDF(dfName, declCallstack, defCallstack, useCallstack)));
        return createReport("LUNA14", createJson(map));
    }

    static std::string create23(
        bool type,
        std::string condition,
        std::string fileName,
        int line,
        std::string cfName
    ){
        std::map<std::string, std::string> map = {};
        map.insert(std::make_pair("type", std::to_string(type)));//todo it works?
        map.insert(std::make_pair("condition", condition));
        map.insert(std::make_pair("where", createCallStackEntry(fileName, std::to_string(line), cfName)));
        return createReport("LUNA23", createJson(map));
    }

    static std::string create26(
        std::string expression,
        std::string cfName,
        std::string cfType,
        std::string fileName,
        int line,
        std::string callstack
    ){
        std::map<std::string, std::string> map = {};
        map.insert(std::make_pair("expression", expression));
        map.insert(std::make_pair("cf", createCF(cfName, cfType, fileName, line)));
        map.insert(std::make_pair("callstack", callstack));
        return createReport("LUNA26", createJson(map));
    }

    static std::string create36(
        std::string expression,
        std::string callstack
    ){
        std::map<std::string, std::string> map = {};
        map.insert(std::make_pair("expression", expression));
        map.insert(std::make_pair("callstack", callstack));
        return createReport("LUNA36", createJson(map));
    }

private:

    JsonReporter(){}

};
