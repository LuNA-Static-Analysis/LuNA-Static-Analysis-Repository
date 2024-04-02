#include "json_reporter.hpp"

std::string JsonReporter::createJson(
        std::map<std::string, std::string> jsonObjects
){
    std::string result = "{";
    for (auto l: jsonObjects){
        result += 
        "\"" + l.first + "\": \"" + l.second + "\",";  
    }
    return result.substr(0, result.size() - 1) + "}";
}

std::string JsonReporter::createArray(std::vector<std::string> jsonObjects){
    std::string result = "[";
    for (auto jo: jsonObjects){
        result += (jo + ",");
    }
    result[result.size() - 1] = ']';
    return result;
    
}

std::string JsonReporter::createReport(std::string lunaCode, std::string details){
    std::map<std::string, std::string> map = {};
    map.insert(std::make_pair("error_code", lunaCode));
    map.insert(std::make_pair("details", details));
    return createJson(map);
}

/*
{
  "file": "main.fa",
  "line": 42,
  "name": "set_int" (или, например, "for")
}
*/
std::string JsonReporter::createCallStackEntry(
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
std::string JsonReporter::createCallStack(
        std::vector<std::string> callStackEntries
){
    return "";//todo
}

JsonReporter::JsonReporter(){}
