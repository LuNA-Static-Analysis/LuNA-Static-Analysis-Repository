#include "ast.hpp"
#include <map>
#include <set>

class ast_analyzer {
public:
    ast_analyzer(ast* ast, FILE* file) : ast_(ast), file_(file) {}

    ~ast_analyzer() {}

    bool analyze();

    template <typename T>
    class cf_info {
    public:
        cf_info(luna_string* alias, std::vector<T>* params) : alias_(alias), params_(params) {}
        ~cf_info() {}

        luna_string* alias_;
        std::vector<T>* params_;

        std::string to_string() const {
            std::string p;
            for (auto i : *params_) {
                if (i == nullptr) {
                    p += "nullptr ,";
                }
                else {
                    p += i->to_string() + ",";
                }
            }
            if (p != "") p.pop_back();
            return alias_->to_string() + "(" + p + ")";
        }

    };

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
    std::vector<cf_info<expr *> *> get_all_calling(block* block);

    std::vector<luna_string *> get_block_values(block* block_);
    std::map<std::string, std::set<uint>> find_redecls(std::vector<luna_string* > values);
    template <typename T>
    std::vector<T> find_pairs(std::vector<T>* v);
    std::string get_line_from_file(uint num);

};