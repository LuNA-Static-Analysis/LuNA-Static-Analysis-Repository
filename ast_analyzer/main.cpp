#include "../parser/ast.hpp"
#include "grammar.tab.hpp"
#include "ast_analyzer.hpp"
#include "error_reporter.hpp"
#include "ast_tester.hpp"
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
    clock_gettime(CLOCK_MONOTONIC_RAW, &start_all);

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


    clock_gettime(CLOCK_MONOTONIC_RAW, &start_parse);
    yyparse();
    clock_gettime(CLOCK_MONOTONIC_RAW, &end_parse);

    // std::cerr << ast_->to_string() << std::endl;

    // std::cerr << compare(yyin, ast_) << std::endl;

    ast_analyzer analyzer = ast_analyzer(ast_, yyin);

    // std::cerr << "analyze\n";

    clock_gettime(CLOCK_MONOTONIC_RAW, &start_analyze);
    analyzer.analyze();
    clock_gettime(CLOCK_MONOTONIC_RAW, &end_analyze);


    std::cerr << "Всего токенов: "<< tokens << std::endl;
    std::cerr << "Токенов в AST: " << ast_->get_tokens_count() << std::endl;

    delete ast_;
    fclose(yyin);

    clock_gettime(CLOCK_MONOTONIC_RAW, &end_all);
    printf("Program time: %lf sec.\n", end_all.tv_sec-start_all.tv_sec + 0.000000001*(end_all.tv_nsec-start_all.tv_nsec));
    printf("Analyze time: %lf sec.\n", end_analyze.tv_sec-start_analyze.tv_sec + 0.000000001*(end_analyze.tv_nsec-start_analyze.tv_nsec));
    printf("Parse time: %lf sec.\n", end_parse.tv_sec-start_parse.tv_sec + 0.000000001*(end_parse.tv_nsec-start_parse.tv_nsec));
    return EXIT_SUCCESS;
}
