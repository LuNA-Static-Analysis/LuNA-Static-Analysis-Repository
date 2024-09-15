#include <condition_variable>
#include <semaphore>
#include <fstream>

#include "./parser/ast.hpp"
#include "./parser/grammar.tab.hpp"
#include "./ast_analyzer/undecl_names_analyzer.hpp"
#include "./ast_analyzer/unused_names_analyzer.hpp"
#include "./ast_analyzer/shadow_import_analyzer.hpp"
#include "./ast_analyzer/existance_main_analyzer.hpp"
#include "./ast_analyzer/cf_redecl_analyzer.hpp"
#include "./ast_analyzer/undecl_func_analyzer.hpp"
#include "./ast_analyzer/df_redecl_analyzer.hpp"
#include "./ast_analyzer/threadpool/threadpool.h"
#include "./DeGSA/ddg.cpp"

const int EXIT_ERROR = 1;

extern int yyparse();
extern FILE *yyin;
int line_num = 1;
std::string line, prev_line;
uint tokens = 0;
ast *ast_ = new ast();

int main(int argc, char **argv){

    /*auto realStart = std::chrono::steady_clock::now();

    bool launchASTAnalyzer = false;
    bool launchDeGSA = false;
    char* inputFileName = argv[1];
    std::string outputFileName = "output.txt";

    std::string realLunaSource(argv[2]);

    for (int i = 3; i < argc; i++){

        std::string arg(argv[i]);

        if (arg == "-ast"){
            launchASTAnalyzer = true;
        } else if (arg == "-degsa"){
            launchDeGSA = true;
        } else if (arg == "-o"){//todo unused
            std::cout << arg << std::endl;
            if (i < argc - 1){
                i++;
                outputFileName = std::string(argv[i]);
            } else {
                std::cerr << "No output file name presented, using default name" << std::endl;
            }
        } else {
            std::cerr << "Unidentified parameter" << std::endl;
        }
    }

    if (inputFileName == nullptr){
        std::cerr << "No input file name present" << std::endl;
    }

    if (!launchASTAnalyzer && !launchDeGSA){
        launchASTAnalyzer = true;
        launchDeGSA = true;
    }

    std::ofstream outputFile(outputFileName);//todo perhaps need to add name check

    auto astBuildStart = std::chrono::steady_clock::now();
    auto astBuildStartSystem = std::chrono::system_clock::now();

    yyin = fopen(inputFileName, "r");

    if (!yyin)
    {
        std::cerr << "Couldn't open file" << std::endl;
        return EXIT_ERROR;
    }

    yyparse();

    auto astBuildEnd = std::chrono::steady_clock::now();
    auto astBuildEndSystem = std::chrono::system_clock::now();

    auto astBuildTotal = std::chrono::duration_cast<ns>(astBuildEnd - astBuildStart).count();
    auto astBuildTotalSystem = std::chrono::duration_cast<ns>(astBuildEndSystem - astBuildStartSystem).count();

    std::cerr << "Parse successfully completed" << std::endl;

    std::ofstream out;
    out.open("ast.json");

    if (out.is_open())
    {
        out << ast_->to_json();
    }

    out.close();*/
//wip
    std::ofstream o;
        o.open("./reporter/found_errors.json");
o << "[]";

        o.close();

    /*if (launchASTAnalyzer){
        error_reporter reporter = error_reporter();

        std::vector<base_analyzer *> analyzers = {
            new undeclarated_names_analyzer(ast_, yyin, &reporter, realLunaSource),
            new unused_names_analyzer(ast_, yyin, &reporter, realLunaSource),
            new shadow_import_analyzer(ast_, yyin, &reporter, realLunaSource),
            new existance_main_analyzer(ast_, yyin, &reporter, realLunaSource),
            new cf_redecl_analyzer(ast_, yyin, &reporter, realLunaSource),
            new undecl_func_analyzer(ast_, yyin, &reporter, realLunaSource),
            new df_redecl_analyzer(ast_, yyin, &reporter, realLunaSource)
        };

        for (auto a : analyzers) {
            std::cerr << a->get_name() << std::endl;
            a->analyze();
            delete a;
        }

        std::ofstream o;
        o.open("./reporter/found_errors.json");

        if (o.is_open())
        {
            o << reporter.get_errors();
        }

        o.close();
    }

    if (launchDeGSA){
        std::ofstream degsaOutputFile("adapt_degsa_output.txt");
        DDG ddg(ast_, &degsaOutputFile, realLunaSource);
        degsaOutputFile << "\nTime to build AST: " << (double)astBuildTotal / 1000000000 << " seconds" << std::endl;
        degsaOutputFile << "\nTime to build AST (system): " << (double)astBuildTotalSystem / 1000000000 << " seconds" << std::endl;
        auto realEnd = std::chrono::steady_clock::now();
        auto realTotal = std::chrono::duration_cast<ns>(realEnd - realStart).count();
        degsaOutputFile << "\nTime real: " << (double)realTotal / 1000000000 << " seconds" << std::endl;
    }*/

    //delete ast_;
    //fclose(yyin);

    return EXIT_SUCCESS;
}
