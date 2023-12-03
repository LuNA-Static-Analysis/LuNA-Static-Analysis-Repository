// #include <gtest/gtest.h>

// #include "../parser/ast.hpp"
// #include "grammar.tab.hpp"
// #include "ast_analyzer.hpp"
// #include "error_reporter.hpp"
// #include "ast_tester.hpp"
// #include <fstream>

// const int EXIT_ERROR = 1;

// extern int yyparse();
// extern FILE *yyin;
// int line_num = 1;
// std::string line, prev_line;
// extern error_reporter reporter;

// ast* ast_ = new ast();

// TEST(test_ast, no_parameters){   
//     yyin = fopen("/home/ub/Desktop/luna-static-anayzer/tests/test_import/no_parameters.fa", "r");

//     if (!yyin) {
//         std::cerr << "Couldn'e open file" << std::endl;
//         return;
//     }

//     yyparse();

//     EXPECT_TRUE(compare(yyin, ast_));  
//     delete ast_;
//     fclose(yyin);
// }

// TEST(test_ast, no_) {   
//     yyin = fopen("/home/ub/Desktop/luna-static-anayzer/tests/test_import/no_comma.fa", "r");

//     if (!yyin) {
//         std::cerr << "Couldn'e open file" << std::endl;
//         return;
//     }
//     char c;
//     std::string res;

//     while (!feof(yyin)) {
//         c = fgetc(yyin);
//         // if (is_redudant(c)) continue;
//         res += c;
//     }
//     res.pop_back();

//     std::cout << "source code: \n" << res << std::endl << std::endl;

//     yyparse();

//     EXPECT_TRUE(compare(yyin, ast_));  
//     delete ast_;
//     fclose(yyin);
// }