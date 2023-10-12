#include <nlohmann/json.hpp>
#include <string.h>
#include <iostream>
#include "../../parser/ast.hpp"


void serialize_luna_string() {
    std::string s = "hello";
    luna_string l = luna_string(&s, 10);
    nlohmann::json j{};

    j["value_"] = l.to_string();
    j["line_"] = l.line_;

    std::cerr << j << std::endl;
}


void serialize_param() {
    luna_string l1 = luna_string(new std::string("type"));
    luna_string l2 = luna_string(new std::string("name"));

    param p = param(&l1, &l2);

    nlohmann::json j{};

    j["type_"] = p.type_->to_string();
    j["name_"] = p.name_->to_string();

    std::cerr << j << std::endl;
}

void serialize_param_seq() {
    luna_string l1 = luna_string(new std::string("type"));
    luna_string l2 = luna_string(new std::string("name"));

    param p1 = param(&l1, &l2);
    param p2 = param(&l1, &l2);
    param p3 = param(&l1, &l2);

    std::vector<param *> v = std::vector<param *> {&p1, &p2, &p3};
    std::cerr << "--------\n";

    std::vector<int> c_vector {1, 2, 3, 4};

    nlohmann::json j(c_vector);
    std::cerr << j << std::endl;
}

int main () {
    serialize_param_seq();
}