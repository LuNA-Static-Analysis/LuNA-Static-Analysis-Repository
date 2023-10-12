// #pragma once
// #include "../parser/ast.hpp"
// #include "error_reporter.hpp"
// #include <map>
// #include <set>
// #include <assert.h>

// class ast_analyzer {
// public:
//     // ast_analyzer(ast* ast, FILE* file, error_reporter reporter) : ast_(ast), file_(file), reporter_(reporter) {}
//     virtual ~ast_analyzer() {}
//     virtual bool analyze() = 0;

// protected:
//     static const int FSEEK_ERROR = -1;
//     FILE* file_;
//     ast* ast_;
//     error_reporter* reporter_;

//     // bool analyze_shadow_import();
//     // bool have_such_code_id(std::map<std::string, std::string>& map, import* import);
//     // bool analyze_df_double_declaration();
//     // bool analyze_existance_main_cf();
//     // bool analyze_cf_redeclaration();
//     // bool analyze_df_redeclaration();
//     // bool has_df_redeclaration(std::vector<luna_string *> prev_values, block* block_);
//     // bool analyze_calling_undeclarated_func();

//     // std::vector<luna_string *> get_block_values(block* block_);
//     // std::map<std::string, std::set<uint>> find_redecls(std::vector<luna_string* > values);
//     // template <typename T>
//     // std::vector<T> find_pairs(std::vector<T>* v);

//     std::string get_line_from_file(uint num);
// };