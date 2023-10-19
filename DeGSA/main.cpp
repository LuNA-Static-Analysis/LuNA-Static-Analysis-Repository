
// All credit for this program goes to Maxim Kurbatov; version of 21.05.2023 (as of 20.09.2023)
// My modifications:
// 1. ast.hpp in headers zone
// 2. ast_analyzer.cpp, 290 line (20.09.2023: this is obsolete, perhaps?)
// 3. for "no fileno" problem:
// 3.1. use g++ -std=gnu++0x lex.yy.c grammar.tab.cpp main.cpp ast_analyzer.cpp -o a.out (g++11 does not work for some reason)
// 3.2. use _fileno() instead, as it is Windows (did not try though)
// Also implemented small convenience-related/natural language changes

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

    if (argc != 2) {
        std::cerr << "Bad number of args. Usage: ./a.out [LuNA program]" << std::endl;
        return EXIT_ERROR;
    }

    yyin = fopen(argv[1], "r");

    if (!yyin) {
        std::cerr << "Couldn't open the file" << std::endl;
        return EXIT_ERROR;
    }

    std::cout << "================= Parsing ============\n";

    yyparse();

    std::cout << "================= Done ===============\n";

    //std::cerr << ast_->to_string() << std::endl;
    //std::cout << ast_->to_string() << std::endl;

    //ast_->printTokens();

    //ast_analyzer analyzer = ast_analyzer(ast_, yyin);

    //std::cout << "analyze\n";
    //analyzer.analyze();

    DDG ddg(ast_);
    //std::cout << cfg->to_string() << std::endl;
    std::cout << std::flush;

    delete ast_;
    fclose(yyin);
    return EXIT_SUCCESS;
}