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
#include <fstream>
#include <semaphore.h>
#include "task_scheduler.cpp"

const int EXIT_ERROR = 1;

extern int yyparse();
extern FILE *yyin;
int line_num = 1;
std::string line, prev_line;
extern error_reporter reporter;
uint tokens = 0;
const int PTHREAD_CREATE_ERROR = 0;
const int PTHREAD_JOIN_SUCCESS = 0;
const int THREADS_ONLY = 0;

// sem_t sem;
pthread_barrier_t barrier;

ast* ast_ = new ast();

// void* execute_task(void* args) {
//   auto* analyzers = (std::vector<base_analyzer*>*) (args);

//   for (auto analyzer : *analyzers) {
//       std::cerr << analyzer->get_name() << std::endl;
//       analyzer->analyze();
//       delete analyzer;
//   }

//   pthread_barrier_wait(&barrier);
//   pthread_exit(NULL);
// }
// int main(int argc, char** argv) {
//     if (argc != 2) {
//         std::cerr << "Bad number of args. Usage: ./a.out test.fa" << std::endl;
//         return EXIT_ERROR;
//     }

//     yyin = fopen(argv[1], "r");

//     if (!yyin) {
//         std::cerr << "Couldn'e open file" << std::endl;
//         return EXIT_ERROR;
//     }

//     yyparse();

//     std::cerr << "Parse successfully completed" << std::endl;

//     std::vector<base_analyzer *> analyzers = {
//         new undeclarated_names_analyzer(ast_, yyin, &reporter),
//         new unused_names_analyzer(ast_, yyin, &reporter),
//         new shadow_import_analyzer(ast_, yyin, &reporter),
//         new existance_main_analyzer(ast_, yyin, &reporter),
//         new cf_redecl_analyzer(ast_, yyin, &reporter),
//         new undecl_func_analyzer(ast_, yyin, &reporter),
//         new df_redecl_analyzer(ast_, yyin, &reporter)
//     };
    
//     int status = pthread_barrier_init(&barrier, NULL, 3);

//     if (status != 0) {
//         printf("main error: can't init barrier, status = %d\n", status);
//         exit(1);
//     }

//     pthread_t threads[2];
//     std::vector<base_analyzer*> first_task;
//     std::vector<base_analyzer*> second_task;

//     first_task.assign(analyzers.begin(), analyzers.begin() + 3);
//     second_task.assign(analyzers.begin() + 3, analyzers.end());

//     int create_result = pthread_create(&threads[0], NULL, execute_task, (void *)(&first_task));

//     if (create_result != PTHREAD_CREATE_ERROR) {
//         printf("pthread_create error: couldn't create thread\n");
//         return ERROR;
//     }

//     create_result = pthread_create(&threads[1], NULL, execute_task, (void *)(&second_task));
//     if (create_result != PTHREAD_CREATE_ERROR) {
//         printf("pthread_create error: couldn't create thread\n");
//         return ERROR;
//     }

//     pthread_barrier_wait(&barrier);
//     std::cerr << "deleting ast....\n";
//     delete ast_;
//     fclose(yyin);

//     pthread_barrier_destroy(&barrier);

//     return EXIT_SUCCESS;
// }


// int main(int argc, char** argv) {
//     if (argc != 2) {
//         std::cerr << "Bad number of args. Usage: ./a.out test.fa" << std::endl;
//         return EXIT_ERROR;
//     }

//     yyin = fopen(argv[1], "r");

//     if (!yyin) {
//         std::cerr << "Couldn'e open file" << std::endl;
//         return EXIT_ERROR;
//     }

//     yyparse();

//     std::cerr << "Parse successfully completed" << std::endl;

//     std::vector<base_analyzer *> analyzers = {
//         new undeclarated_names_analyzer(ast_, yyin, &reporter),
//         new unused_names_analyzer(ast_, yyin, &reporter),
//         new shadow_import_analyzer(ast_, yyin, &reporter),
//         new existance_main_analyzer(ast_, yyin, &reporter),
//         new cf_redecl_analyzer(ast_, yyin, &reporter),
//         new undecl_func_analyzer(ast_, yyin, &reporter),
//         new df_redecl_analyzer(ast_, yyin, &reporter)
//     };
    
//     int status = pthread_barrier_init(&barrier, NULL, 3);

//     if (status != 0) {
//         printf("main error: can't init barrier, status = %d\n", status);
//         exit(1);
//     }

//     task_scheduler scheduler = task_scheduler(analyzers, barrier);
//     scheduler.execute_tasks();

//     pthread_barrier_wait(&barrier);
//     std::cerr << "deleting ast....\n";
//     delete ast_;
//     fclose(yyin);

//     pthread_barrier_destroy(&barrier);

//     return EXIT_SUCCESS;
// }



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

    clock_gettime(CLOCK_MONOTONIC_RAW, &start_parse);
    yyparse();
    clock_gettime(CLOCK_MONOTONIC_RAW, &end_parse);

    std::cerr << "Parse successfully completed" << std::endl;

    std::vector<base_analyzer *> analyzers = {
        new undeclarated_names_analyzer(ast_, yyin, &reporter),
        new unused_names_analyzer(ast_, yyin, &reporter),
        new shadow_import_analyzer(ast_, yyin, &reporter),
        new existance_main_analyzer(ast_, yyin, &reporter),
        new cf_redecl_analyzer(ast_, yyin, &reporter),
        new undecl_func_analyzer(ast_, yyin, &reporter),
        new df_redecl_analyzer(ast_, yyin, &reporter)
    };

    clock_gettime(CLOCK_MONOTONIC_RAW, &start_analyze);
    for (auto analyzer : analyzers) {
        analyzer->analyze();
    }
    clock_gettime(CLOCK_MONOTONIC_RAW, &end_analyze);

    for (auto analyzer : analyzers) {
        delete analyzer;
    }

    std::cerr << "Всего токенов: "<< tokens << std::endl;
    std::cerr << "Токенов в AST: " << ast_->get_tokens_count() << std::endl;


    clock_gettime(CLOCK_MONOTONIC_RAW, &end_all);
    printf("Program time: %lf sec.\n", end_all.tv_sec-start_all.tv_sec + 0.000000001*(end_all.tv_nsec-start_all.tv_nsec));
    printf("Analyze time: %lf sec.\n", end_analyze.tv_sec-start_analyze.tv_sec + 0.000000001*(end_analyze.tv_nsec-start_analyze.tv_nsec));
    printf("Parse time: %lf sec.\n", end_parse.tv_sec-start_parse.tv_sec + 0.000000001*(end_parse.tv_nsec-start_parse.tv_nsec));

    delete ast_;
    fclose(yyin);

    return EXIT_SUCCESS;
}