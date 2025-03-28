#pragma once

#include <cstring>
#include <iostream>
#include <vector>
#include "../error_message/error_entry.hpp" 


enum ERROR_LEVEL {
   ERROR,
   WARNING,
   NO_ERROR
};

class error_reporter {
public:
    void report(ERROR_LEVEL level,
        const std::string& error_msg,
        const std::string& error_line,
        unsigned int line,
        const std::string& expectation = "")
    {

        if (errors_number < LIMIT_ERRORS) {
            std::string lvl = level == ERROR ? "Error" : "Warning";

            if (error_line == "" ) {
                fprintf(stderr, "%s. %s\n", lvl.c_str(), error_msg.c_str());
                return;
            }

            if (line == 0) {
                fprintf(stderr, "%s. %s\n %s\n", lvl.c_str(), error_msg.c_str(), error_line.c_str());
                return;
            }

            fprintf(stderr, "%s. Line %d: %s\n\t%s\n", lvl.c_str(), line, error_msg.c_str(), error_line.c_str());

            if (expectation != "") {
                fprintf(stderr, "Expected: %s\n\n",  expectation.c_str());
            }

            std::cerr << std::endl;
        }

        if (level == ERROR) {
            errors_number++;
            return;
        }
    }

    bool has_errors() const {
        return errors_number != 0;
    }

    void report_json(const details details) {
        std::string s = std::string("{ \"error_code\" : \"" + details.error_code + "\", " + details.to_json() + "}" + "\n");
        errors_msg.push_back(s);
    }

    std::string get_errors() {
        std::stringstream ss;

        ss << "[";

        size_t len = errors_msg.size();

        for (int j = 0; j < len; j++) {
            auto i = errors_msg.at(j);
            ss << i << (j == len - 1 ? "" : ",");
        }

        ss << "]";

        return ss.str();
    }

private:
    std::vector<std::string> errors_msg; 
    unsigned int errors_number = 0;
    const int LIMIT_ERRORS = 1000;
};
