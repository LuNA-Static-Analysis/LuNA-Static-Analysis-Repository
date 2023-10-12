#include "../parser/ast.hpp"
#include "grammar.tab.hpp"
#include "undecl_names_analyzer.hpp"
#include "unused_names_analyzer.hpp"
#include "shadow_import_analyzer.hpp"

#include <fstream>

const int EXIT_ERROR = 1;

extern int yyparse();
extern FILE *yyin;
int line_num = 1;
std::string line, prev_line;
extern error_reporter reporter;
uint tokens = 0;
ast* ast_ = new ast();

int main(int argc, char** argv) {
    struct timespec start_parse, end_parse, start_analyze, end_analyze, start_all, end_all;

    if (argc != 2) {
        std::cerr << "Bad number of args. Usage: ./a.out test.fa" << std::endl;
        return EXIT_ERROR;
    }

    yyin = fopen(argv[1], "r");

    if (!yyin) {
        std::cerr << "Couldn'e open file" << std::endl;
        return EXIT_ERROR;
    }

    std::cerr << "parse\n";

    yyparse();

    base_analyzer* analyzer = new undeclarated_names_analyzer(ast_, yyin, &reporter);

    base_analyzer* analyzer1 = new unused_names_analyzer(ast_, yyin, &reporter);

    base_analyzer* analyzer2 = new shadow_import_analyzer(ast_, yyin, &reporter);




    std::cerr << "\n--------undeclarated_names_analyzer------------\n\n";
    analyzer->analyze();

    std::cerr << "\n-------------unused_names_analyzer-------------\n\n" ;
    analyzer1->analyze();

    std::cerr << "\n--------shadow_import_analyzer----------\n\n";
    analyzer2->analyze();



    delete analyzer1;
    delete analyzer2;
    delete analyzer;

    delete ast_;
    fclose(yyin);
    return EXIT_SUCCESS;
}
