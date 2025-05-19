#pragma once
#include <iostream>
#include <string>
#include <vector>
#include <utility>


namespace AstErrorReporter { 

enum ERROR_LEVEL {
   ERROR,
   WARNING,
   NO_ERROR
};

struct CodeLocation {
    std::string file;
    uint line;
    std::string name;

    std::string toJson() const {
        return R"({"file": ")" + file + R"(", "line": )" + std::to_string(line) + R"(, "name": ")" + name + R"("})";
    }
};

struct CallStackEntry {
    std::string file;
    uint line;
    std::string name;

    std::string toJson() const {
        return R"({"file": ")" + file + R"(", "line": )" + std::to_string(line) + R"(, "name": ")" + name + R"("})";
    }
};

struct CF {
    std::string name;
    std::string type;
    std::string file;
    uint line;

    std::string toJson() const {
        return R"({"name": ")" + name + R"(", "type": ")" + type + 
               R"(", "file": ")" + file + R"(", "line": )" + std::to_string(line) + R"(})";
    }
};

struct Identifier {
    std::string name;
    CodeLocation declared;

    std::string toJson() const {
        return R"({"name": ")" + name + R"(", "declared": )" + declared.toJson() + "}";
    }
};

class ErrorReporter {
    std::vector<std::string> errors;

    std::string createError(
        const std::string& error_code,
        const std::vector<std::pair<std::string, std::string>>& details = {}) {
        
        std::string detailsJson = "{}";
        
        if (!details.empty()) {
            detailsJson = "{";
            bool first = true;
            
            for (const auto& [key, value] : details) {
                if (!first) detailsJson += ", ";
                first = false;
                detailsJson += "\"" + key + "\": " + value;
            }
            
            detailsJson += "}";
        }
        
        return R"({"error_code": ")" + error_code + R"(", "details": )" + detailsJson + "}";
    }

public:
    void addSYN1(const CallStackEntry& call_stack, const CF& cf) {
        errors.push_back(createError("SYN1", {
            {"call_stack_entry", call_stack.toJson()},
            {"cf", cf.toJson()}
        }));
    }

    void addSYN2(const CallStackEntry& call_stack) {
        errors.push_back(createError("SYN2", {
            {"call_stack_entry", call_stack.toJson()}
        }));
    }

    void addSYN3(const CallStackEntry& call_stack, const CF& cf) {
        errors.push_back(createError("SYN3", {
            {"cf", cf.toJson()},
            {"call_stack_entry", call_stack.toJson()}
        }));
    }

    void addSYN6_1(const std::vector<CF>& cfs) {
        std::string cfsJson = "[";
        for (size_t i = 0; i < cfs.size(); ++i) {
            if (i != 0) cfsJson += ", ";
            cfsJson += cfs[i].toJson();
        }
        cfsJson += "]";

        errors.push_back(createError("SYN6.1", {
            {"cfs", cfsJson}
        }));
    }

    void addSYN6_2(const std::vector<CF>& cfs) {
        std::string cfsJson = "[";
        for (size_t i = 0; i < cfs.size(); ++i) {
            if (i != 0) cfsJson += ", ";
            cfsJson += cfs[i].toJson();
        }
        cfsJson += "]";

        errors.push_back(createError("SYN6.2", {
            {"cfs", cfsJson}
        }));
    }

    void addSYN7() {
        errors.push_back(createError("SYN7"));
    }

    void addSYN8_1(const Identifier& identifier) {
        errors.push_back(createError("SYN8.1", {
            {"identifier", identifier.toJson()}
        }));
    }

    void addSYN8_2(const Identifier& identifier) {
        errors.push_back(createError("SYN8.2", {
            {"identifier", identifier.toJson()}
        }));
    }

    void addSYN9(const Identifier& identifier) {
        errors.push_back(createError("SYN9", {
            {"identifier", identifier.toJson()}
        }));
    }

    void addSEM4(const Identifier& identifier) {
        errors.push_back(createError("SEM4", {
            {"identifier", identifier.toJson()}
        }));
    }

    void addSEM8(CallStackEntry& where) {
        std::string cfsJson = "[";
        cfsJson += where.toJson();
        cfsJson += "]";

        errors.push_back(createError("SEM8", {
            {"where", cfsJson}
        }));
    }

    std::string get_all_errors_json() const {
        std::string result = "[";
        for (size_t i = 0; i < errors.size(); ++i) {
            if (i != 0) result += ", ";
            result += errors[i];
        }
        result += "]";
        return result;
    }

    void clear() {
        errors.clear();
    }

    bool hasErrors() const {
        return !errors.empty();
    }
};
}
