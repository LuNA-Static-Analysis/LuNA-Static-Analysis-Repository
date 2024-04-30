#pragma once
#include "../parser/ast.hpp"
#include "error_reporter.hpp"
#include "../error_message/error_entry.hpp"

#include <vector>
#include <map>
#include <string>
#include <set>
#include <assert.h>

class base_analyzer {
public:
    virtual bool analyze() = 0;
    virtual ~base_analyzer() {}
    virtual std::string get_name() = 0;

protected:
    static const int FSEEK_ERROR = -1;
    FILE* file_;
    ast* ast_;
    error_reporter* reporter_;
    std::string real_luna_source_;

    std::string current_cf;

    std::string get_file() {
        return real_luna_source_;
    }

    std::string get_line_from_file(uint num) {
        // return "";

        int fseek_res = fseek(file_, 0, SEEK_SET);
        if (fseek_res == FSEEK_ERROR) {
            perror("fseek");
            throw std::runtime_error("fseek");
        }

        int i = 1;
        while (i != num) {
            char c = fgetc(file_);
            assert(c != EOF);

            if (c == '\n') {
                i++;
            }
        }

        size_t len = 0;
        char* line = NULL;
        ssize_t string_len = getline(&line, &len, file_);

        if (string_len == -1) {
            throw std::runtime_error("Couldn't allocate memory");
        }

        int start = 0;
        for (int i = 0; i < string_len; i++) {
            if (line[i] != ' '){
                start = i;
                break;
            }
        }

        std::string l;
        for (int i = start; i < string_len - 1; i++) {
            l.push_back(line[i]);
        }

        delete line;
        return l;
    }

    
};