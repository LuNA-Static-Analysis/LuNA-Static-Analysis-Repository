#include "../parser/ast.hpp"
#include <map>
#include <set>

// std::string to_string() const {
//     std::string p;
//     switch (t) {
//         case INT: return "int";
//         case REAL: return "real";
//         case STRING : return "string";
//         case ERROR_TYPE: return "error_type";
//         case UNDEFINED: return "undefined";
//     }
// }

// enum luna_type {
//     INT,
//     REAL,
//     STRING,
//     NAME,
//     VALUE,
//     UNDEFINED,
//     ERROR_TYPE,

// };

static std::string print_type(luna_type type) {
    switch (type) {
        case LUNA_INT:  return "int"; 
        case LUNA_REAL:  return "real"; 
        case LUNA_STRING:  return "string"; 
        case LUNA_ERROR_TYPE: return "error_type"; 
        case LUNA_VALUE: return "value"; 
        case LUNA_NAME: return "name"; 
        case LUNA_UNDEFINED: return "undefined"; 
    }
    throw new std::runtime_error("invalid type");
}

class ast_analyzer {
public:
    ast_analyzer(ast* ast, FILE* file) : ast_(ast), file_(file) {}

    ~ast_analyzer() {}

    bool analyze();

    // // класс хранит название ФК на LuNA и его параметры
    // class cf_info {
    // public:

    //     cf_info(luna_string* alias, std::vector<luna_type>* params) : alias_(alias), params_(params) {}
    //     ~cf_info() {}

    //     luna_string* alias_;
    //     std::vector<luna_type>* params_;

    //     std::string to_string() const {
    //         std::string p;
    //         for (auto i : *params_) {
    //             std::string s;
    //             switch (i) {
    //                 case (INT): { p += "int, "; }
    //                 case (REAL): { p += "real, "; }
    //                 case (STRING): { p += "string,"; }
    //                 case (UNDEFINED): { p += "undefined, "; }
    //             }
    //         }
    //         if (p != "") p.pop_back();
    //         return alias_->to_string() + "(" + p + ")";
    //     }
    // };


    // класс хранит название ФК на LuNA и его параметры
    // template <typename T>
    // class cf_info {
    // public:
    //     cf_info(luna_string* alias, std::vector<T>* params) : alias_(alias), params_(params) {}
    //     ~cf_info() {}

    //     luna_string* alias_;
    //     std::vector<T>* params_;

    //     std::string to_string() const {
    //         std::string p;
    //         for (auto i : *params_) {
    //             if (i == nullptr) {
    //                 p += "nullptr ,";
    //             }
    //             else {
    //                 p += i->to_string() + ",";
    //             }
    //         }
    //         if (p != "") p.pop_back();
    //         return alias_->to_string() + "(" + p + ")";
    //     }
    // };

    // template <typename T = luna_type>
    // class cf_info {
    // public:
    //     cf_info(luna_string* alias, std::vector<T>* params) : alias_(alias), params_(params) {}
    //     ~cf_info() {}

    //     luna_string* alias_;
    //     std::vector<T>* params_;

    //     std::string to_string() const {
    //         std::string p;
    //         for (auto i : *params_) {
    //             switch (t) {
    //                 case INT: return p += "int, ";
    //                 case REAL: return p += "real, ";
    //                 case STRING : return p += "string, ";
    //                 case ERROR_TYPE: return p += "error_type, ";
    //                 case UNDEFINED: return p += "undefined, ";
    //             }
    //         }
    //         if (p != "") p.pop_back();
    //         return alias_->to_string() + "(" + p + ")";
    //     }
    // };



private:
    static const int FSEEK_ERROR = -1;
    FILE* file_;
    ast* ast_;


    bool analyze_shadow_import();
    bool have_such_code_id(std::map<std::string, std::string>& map, import* import);
    bool analyze_df_double_declaration();
    bool analyze_existance_main_cf();
    bool analyze_cf_redeclaration();
    bool analyze_df_redeclaration();
    bool has_df_redeclaration(std::vector<luna_string *> prev_values, block* block_);
    bool analyze_calling_undeclarated_func();

    std::multimap<luna_string , std::vector<luna_type> *>* get_types_from_calling(std::multimap<luna_string , std::vector<expr*> *>* cur_cfs);
    std::vector<luna_type>* params_to_types(std::vector<expr *>* params);
    std::multimap<luna_string, std::vector<expr*>*> get_all_calling(block* block);

    std::vector<luna_string *> get_block_values(block* block_);
    std::map<std::string, std::set<uint>> find_redecls(std::vector<luna_string* > values);

    template <typename T>
    std::vector<T> find_pairs(std::vector<T>* v);
    std::string get_line_from_file(uint num);

};