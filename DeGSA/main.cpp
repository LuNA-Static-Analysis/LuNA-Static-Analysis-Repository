// for "no fileno" problem (this problem was on Windows at least):
// 1. use g++ -std=gnu++0x lex.yy.c grammar.tab.cpp main.cpp ast_analyzer.cpp -o a.out (g++11 does not work for some reason)
// 2. use _fileno() instead for Windows (did not try though)

#include "../parser/ast.hpp"
#include "grammar.tab.hpp"
#include <fstream>
#include "ddg.cpp"

const int EXIT_ERROR = 1;

extern int yyparse();
extern FILE *yyin;
int line_num = 1;
std::string line, prev_line;
uint tokens = 0;

ast* ast_ = new ast();

int main(int argc, char** argv) {

    std::ofstream outputFile("output.txt");

    auto astBuildStart = std::chrono::steady_clock::now();

    if (argc != 2) {
        std::cout << "INTERNAL ERROR: Bad number of args. Usage: ./a.out [LuNA program]" << std::endl;
        return EXIT_ERROR;
    }

    yyin = fopen(argv[1], "r");

    if (!yyin) {
        std::cout << "INTERNAL ERROR: couldn't open the file" << std::endl;
        return EXIT_ERROR;
    }

    yyparse();

    auto astBuildEnd = std::chrono::steady_clock::now();
    auto astBuildTotal = std::chrono::duration_cast<ns>(astBuildEnd - astBuildStart).count();

    // &std::cout for console output
    DDG ddg(ast_, &outputFile);
    outputFile << "\nTime to build AST: " << (double)astBuildTotal / 1000000000 << " seconds" << std::endl;

    delete ast_;
    fclose(yyin);
    outputFile.close();

    std::cout << "DeGSA finished successfully" << std::endl;

    return EXIT_SUCCESS;
}
