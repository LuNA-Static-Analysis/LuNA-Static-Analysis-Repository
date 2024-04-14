#include <condition_variable>
#include <semaphore>
#include <fstream>

#include "../parser/ast.hpp"
#include "grammar.tab.hpp"
#include "undecl_names_analyzer.hpp"
#include "unused_names_analyzer.hpp"
#include "shadow_import_analyzer.hpp"
#include "existance_main_analyzer.hpp"
#include "cf_redecl_analyzer.hpp"
#include "undecl_func_analyzer.hpp"
#include "df_redecl_analyzer.hpp"
#include "threadpool/threadpool.h"

const int EXIT_ERROR = 1;

extern int yyparse();
extern FILE *yyin;
int line_num = 1;
std::string line, prev_line;
uint tokens = 0;
ast *ast_ = new ast();


int main(int argc, char **argv) {
  if (argc != 2) {
    std::cerr << "Bad number of args. Usage: ./a.out test.fa" << std::endl;
    return EXIT_ERROR;
  }

  yyin = fopen(argv[1], "r");

  if (!yyin) {
    std::cerr << "Couldn'e open file" << std::endl;
    return EXIT_ERROR;
  }

  yyparse();

  std::cerr << "Parse successfully completed" << std::endl;

  std::ofstream out;          
  out.open("ast.json");

  if (out.is_open()) {
    out << ast_->to_json();
  }

  out.close(); 

  error_reporter reporter = error_reporter();

  std::vector<base_analyzer *> analyzers = {
      new undeclarated_names_analyzer(ast_, yyin, &reporter),
      new unused_names_analyzer(ast_, yyin, &reporter),
      new shadow_import_analyzer(ast_, yyin, &reporter),
      new existance_main_analyzer(ast_, yyin, &reporter),
      new cf_redecl_analyzer(ast_, yyin, &reporter),
      new undecl_func_analyzer(ast_, yyin, &reporter),
      new df_redecl_analyzer(ast_, yyin, &reporter)
  };

  for (auto a : analyzers) {
      std::cerr << a->get_name() << std::endl;
      a->analyze();
      delete a;
  }

  std::cout << reporter.get_errors();

  delete ast_;
  fclose(yyin);

  return EXIT_SUCCESS;
}