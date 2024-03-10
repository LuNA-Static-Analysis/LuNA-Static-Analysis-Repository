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
#include "task_scheduler.cpp"

const int EXIT_ERROR = 1;

extern int yyparse();
extern FILE *yyin;
int line_num = 1;
std::string line, prev_line;
extern error_reporter reporter;
uint tokens = 0;
ast *ast_ = new ast();

// class priority_task {
// public:
//   std::binary_semaphore sem{0};
//   base_analyzer *analyzer_;

//   priority_task(base_analyzer *analyzer) : analyzer_(analyzer) {
//   }

//   void getTask()
//   {
//     analyzer_->analyze();
//     delete analyzer_;
//     sem.release();
//   }
// };

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


  // std::vector<base_analyzer *> analyzers = {
  //     new undeclarated_names_analyzer(ast_, yyin, &reporter),
  //     new unused_names_analyzer(ast_, yyin, &reporter),
  //     new shadow_import_analyzer(ast_, yyin, &reporter),
  //     new existance_main_analyzer(ast_, yyin, &reporter),
  //     new cf_redecl_analyzer(ast_, yyin, &reporter),
  //     new undecl_func_analyzer(ast_, yyin, &reporter),
  //     new df_redecl_analyzer(ast_, yyin, &reporter)
  // };

  // std::binary_semaphore sem0{0};
  // std::binary_semaphore sem1{0};
  // std::binary_semaphore sem2{0};
  // std::binary_semaphore sem3{0};
  // std::binary_semaphore sem4{0};
  // std::binary_semaphore sem5{0};
  // std::binary_semaphore sem6{0};

  // {
  // size_t n;
  // std::cerr << "Input threads count: ";
  // std::cin >> n;

  // ThreadPool thread_pool{n};

  // thread_pool.add_task(
  //     [&analyzers, &sem0]()
  //     {
  //       // std::cerr << "start task 0\n";
  //       analyzers[0]->analyze();
  //       // std::cerr << analyzers[0]->get_name() << std::endl;
  //       delete analyzers[0];

  //       sem0.release();
  //       // std::cerr << "task 0 done\n";
  //     });

  // thread_pool.add_task(
  //     [&analyzers, &sem1]()
  //     {
  //       // std::cerr << "start task 1\n";
  //       analyzers[1]->analyze();
  //       // std::cerr << analyzers[1]->get_name() << std::endl;
  //       delete analyzers[1];

  //       sem1.release();
  //       // std::cerr << "task 1 done\n";
  //     });

  // thread_pool.add_task(
  //     [&analyzers, &sem2]()
  //     {
  //       // std::cerr << "start task 2\n";
  //       analyzers[2]->analyze();
  //       // std::cerr << analyzers[2]->get_name() << std::endl;
  //       delete analyzers[2];

  //       sem2.release();
  //       // std::cerr << "task 2 done\n";
  //     });

  // thread_pool.add_task(
  //     [&analyzers, &sem3]()
  //     {
  //       // std::cerr << "start task 3\n";
  //       analyzers[3]->analyze();
  //       // std::cerr << analyzers[3]->get_name() << std::endl;
  //       delete analyzers[3];

  //       sem3.release();
  //       // std::cerr << "task 3 done\n";
  //     });

  // thread_pool.add_task(
  //     [&analyzers, &sem4]()
  //     {
  //       // std::cerr << "start task 4\n";
  //       analyzers[4]->analyze();
  //       // std::cerr << analyzers[4]->get_name() << std::endl;
  //       delete analyzers[4];

  //       sem4.release();
  //       // std::cerr << "task 4 done\n";
  //     });

  // thread_pool.add_task(
  //     [&analyzers, &sem5]()
  //     {
  //       // std::cerr << "start task 5\n";
  //       analyzers[5]->analyze();
  //       // std::cerr << analyzers[5]->get_name() << std::endl;
  //       delete analyzers[5];

  //       sem5.release();
  //       // std::cerr << "task 5 done\n";
  //     });

  // thread_pool.add_task(
  //     [&analyzers, &sem6]()
  //     {
  //       // std::cerr << "start task 6\n";
  //       analyzers[6]->analyze();
  //       // std::cerr << analyzers[6]->get_name() << std::endl;
  //       delete analyzers[6];

  //       sem6.release();
  //       // std::cerr << "task 6 done\n";
  //     });
  // }

  // // std::cerr << "main thread wait\n";
  // sem0.acquire();
  // sem1.acquire();
  // sem2.acquire();
  // sem3.acquire();
  // sem4.acquire();
  // sem5.acquire();
  // sem6.acquire();


  std::cerr << "deleting ast... \n";
  delete ast_;
  fclose(yyin);


  return EXIT_SUCCESS;
}