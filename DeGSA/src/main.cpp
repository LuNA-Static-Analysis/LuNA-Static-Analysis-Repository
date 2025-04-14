#include "../../parser/ast.hpp"
#include "../grammar.tab.hpp"
#include <fstream>
#include "ddg.cpp"

const int EXIT_ERROR = 1;
const int NANOSECONDS_IN_SECOND = 1000000000;

extern int yyparse();
extern FILE *yyin;
int line_num = 1;
std::string line, prev_line;
uint tokens = 0;

ast* ast_ = new ast();

int main(int argc, char** argv) {

    std::ofstream outputFile("debug_launch_degsa_output.txt");

    auto astBuildStart = std::chrono::steady_clock::now();
    auto astBuildStartSystem = std::chrono::system_clock::now();

    if (argc < 2) {
        std::cout << "INTERNAL ERROR: bad number of args. Usage: ./[executable] [LuNA program]" << std::endl;
        return EXIT_ERROR;
    }

    yyin = fopen(argv[1], "r");

    if (!yyin) {
        std::cout << "INTERNAL ERROR: couldn't open the file" << std::endl;
        return EXIT_ERROR;
    }

    yyparse();

    auto astBuildEnd = std::chrono::steady_clock::now();
    auto astBuildEndSystem = std::chrono::system_clock::now();
    auto astBuildTotal = std::chrono::duration_cast<ns>(astBuildEnd - astBuildStart).count();
    auto astBuildTotalSystem = std::chrono::duration_cast<ns>(astBuildEndSystem - astBuildStartSystem).count();

    std::string fileName = argv[2];
    DDG ddg(ast_, &outputFile, fileName);
    outputFile << "\nTime to build AST: " << (double)astBuildTotal / NANOSECONDS_IN_SECOND << " seconds" << std::endl;
    outputFile << "\nTime to build AST (system): " << (double)astBuildTotalSystem / NANOSECONDS_IN_SECOND << " seconds" << std::endl;

    delete ast_;
    fclose(yyin);
    outputFile.close();

    std::cout << "DeGSA finished successfully" << std::endl;

    return EXIT_SUCCESS;
}
