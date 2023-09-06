#ifndef AST_TESTER
#define AST_TESTER

#include <algorithm>
#include <string>
#include <iostream>
#include "../parser/ast.hpp"

bool is_redudant(unsigned char c) {
    return c == ' ' || c == '\t' || c == '\n';
}

bool compare(FILE* file, ast* ast) {
    std::string expect;
    expect = ast->to_string();
    // std::cerr << "ast:\n" << expect << std::endl;

    int fseek_res = fseek(file, 0, SEEK_SET);

    // expect.erase(std::remove_if(expect.begin(), expect.end(), is_redudant), expect.end());

    char c;
    std::string res;

    while (!feof(file)) {
        c = fgetc(file);
        // if (is_redudant(c)) continue;
        res += c;
    }
    res.pop_back();

    std::cout << "source code: \n" << res << std::endl << std::endl;
    std::cout << "ast: \n" << expect << std::endl;

    return res == expect;
}
#endif