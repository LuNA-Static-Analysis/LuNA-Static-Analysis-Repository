#pragma once 
#include <regex>
#include <string>

bool is_int(std::string s) {
    return std::regex_match(s, std::regex("[0-9]+"));
}

bool is_real(std::string s) {
    return std::regex_match(s, std::regex("[0-9]+.[0-9]+"));
}

bool is_string(std::string s) {
    return std::regex_match(s, std::regex("\"[^\"]*\""));
}

static std::string print_type(luna_type type) {
    switch (type) {
        case LUNA_INT: return "int"; 
        case LUNA_REAL: return "real"; 
        case LUNA_STRING: return "string"; 
        case LUNA_ERROR_TYPE: return "error_type"; 
        case LUNA_VALUE: return "value"; 
        case LUNA_NAME: return "name"; 
        case LUNA_UNDEFINED: return "undefined"; 
    }
    throw new std::runtime_error("invalid type");
}
    