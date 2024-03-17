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
extern error_reporter reporter;
uint tokens = 0;
ast *ast_ = new ast();

int main(int argc, char **argv)
{
  if (argc != 2)
  {
    std::cerr << "Bad number of args. Usage: ./a.out test.fa" << std::endl;
    return EXIT_ERROR;
  }

  yyin = fopen(argv[1], "r");

  if (!yyin)
  {
    std::cerr << "Couldn'e open file" << std::endl;
    return EXIT_ERROR;
  }

  yyparse();

  std::cerr << "Parse successfully completed" << std::endl;

  std::ofstream out;          
  out.open("ast.json");

  if (out.is_open()){
    out << ast_->to_json();
  }

  out.close(); 

  std::vector<base_analyzer *> analyzers = {
      new undeclarated_names_analyzer(ast_, yyin, &reporter),
      new unused_names_analyzer(ast_, yyin, &reporter),
      new shadow_import_analyzer(ast_, yyin, &reporter),
      new existance_main_analyzer(ast_, yyin, &reporter),
      new cf_redecl_analyzer(ast_, yyin, &reporter),
      new undecl_func_analyzer(ast_, yyin, &reporter),
      new df_redecl_analyzer(ast_, yyin, &reporter)
  };

  std::binary_semaphore sem0{0};
  std::binary_semaphore sem1{0};
  std::binary_semaphore sem2{0};
  std::binary_semaphore sem3{0};
  std::binary_semaphore sem4{0};
  std::binary_semaphore sem5{0};
  std::binary_semaphore sem6{0};

  { // не удалять
  size_t n = 4;

  ThreadPool thread_pool{n};

  thread_pool.add_task(
      [&analyzers, &sem0]()
      {
        analyzers[0]->analyze();
        delete analyzers[0];

        sem0.release();
      });

  thread_pool.add_task(
      [&analyzers, &sem1]()
      {
        analyzers[1]->analyze();
        delete analyzers[1];

        sem1.release();
      });

  thread_pool.add_task(
      [&analyzers, &sem2]()
      {
        analyzers[2]->analyze();
        delete analyzers[2];

        sem2.release();
      });

  thread_pool.add_task(
      [&analyzers, &sem3]()
      {
        analyzers[3]->analyze();
        delete analyzers[3];

        sem3.release();
      });

  thread_pool.add_task(
      [&analyzers, &sem4]()
      {
        analyzers[4]->analyze();
        delete analyzers[4];

        sem4.release();
      });

  thread_pool.add_task(
      [&analyzers, &sem5]()
      {
        analyzers[5]->analyze();
        delete analyzers[5];

        sem5.release();
      });

  thread_pool.add_task(
      [&analyzers, &sem6]()
      {
        analyzers[6]->analyze();
        delete analyzers[6];

        sem6.release();
      });
  }

  sem0.acquire();
  sem1.acquire();
  sem2.acquire();
  sem3.acquire();
  sem4.acquire();
  sem5.acquire();
  sem6.acquire();

  delete ast_;
  fclose(yyin);

  return EXIT_SUCCESS;
}