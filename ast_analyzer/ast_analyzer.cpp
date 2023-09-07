#include "../parser/ast.hpp"
#include "error_reporter.hpp"
#include "ast_analyzer.hpp"

#include <iterator>
#include <set>
#include <map>
#include <assert.h>
#include <regex>

extern error_reporter reporter;
extern std::string line;

bool ast_analyzer::have_such_code_id(std::map<std::string, std::string>& map,
                                    import* import) {
    if (import == nullptr) return false;

    if (map.find(*(import->luna_code_id_->value_)) != map.end()) {
        return true;
    }

    return false;
}

bool ast_analyzer::analyze() {
    assert(ast_->get_program()->sub_defs != nullptr);
    bool has_errors;
    // std::cerr << "1\n";
    // has_errors = analyze_shadow_import();
    // std::cerr << "1\n";
    // has_errors = analyze_df_redeclaration();
    // std::cerr << "1\n";
    // has_errors = analyze_existance_main_cf();
    // std::cerr << "1\n";
    // has_errors = analyze_cf_redeclaration();
    // std::cerr << "1\n";
    has_errors = analyze_calling_undeclarated_func();
    // std::cerr << "1\n";
    return has_errors;
}

std::multimap<std::string, std::vector<expr*>*> ast_analyzer::get_all_calling(block* block) {
    std::multimap<std::string, std::vector<expr*>*> cfs;

    for (auto i : *(block->statement_seq_->statements_)) {
        if (i == nullptr) continue;

        cf_statement* cur_cf = dynamic_cast<cf_statement*> (i);
        if (cur_cf != nullptr) {
            std::vector<expr*>* c = (cur_cf->opt_exprs_->exprs_seq_ == nullptr 
                                        ? new std::vector<expr*>() 
                                        : cur_cf->opt_exprs_->exprs_seq_->expr_);

            cfs.insert(std::pair<std::string , std::vector<expr *> *>(cur_cf->code_id_->to_string(), c));
            continue;
        }

        if_statement* cur_if = dynamic_cast<if_statement*> (i);
        if (cur_if != nullptr) {
            std::multimap<std::string, std::vector<expr*>*> inner_cfs = get_all_calling(cur_if->block_);
            cfs.insert(inner_cfs.begin(), inner_cfs.end());
            continue;
        }

        while_statement* cur_while = dynamic_cast<while_statement*> (i);
        if (cur_while != nullptr) {
            std::multimap<std::string, std::vector<expr*>*> inner_cfs = get_all_calling(cur_while->block_);
            cfs.insert(inner_cfs.begin(), inner_cfs.end());
            continue;
        }

        for_statement* cur_for = dynamic_cast<for_statement*> (i);
        if (cur_for != nullptr) {
            std::multimap<std::string, std::vector<expr*>*> inner_cfs = get_all_calling(cur_for->block_);
            cfs.insert(inner_cfs.begin(), inner_cfs.end());
            continue;
        }
    }

    return cfs;
}

bool is_int(std::string s) {
    return std::regex_match(s, std::regex("[0-9]+"));
}

bool is_real(std::string s) {
    return std::regex_match(s, std::regex("[0-9]+.[0-9]+"));
}

bool is_string(std::string s) {
    return std::regex_match(s, std::regex("\"[^\"]*\""));
}

luna_type get_type(expr* expr) {
    if (is_int(expr->to_string())) return INT;

    if (is_real(expr->to_string())) return REAL;

    if (is_string(expr->to_string())) return STRING;

    to_int* to_int_ = dynamic_cast<to_int*>(expr);
    if (to_int_ != nullptr) {
        luna_type t = get_type(to_int_->expr_);
        switch (t) {
            case INT: return INT;
            case REAL: return INT;
            case STRING : return ERROR_TYPE;
            case ERROR_TYPE: return ERROR_TYPE;
            case UNDEFINED: return UNDEFINED;
        }
        throw new std::runtime_error("unexpected type");
    }

    to_real* to_real_ = dynamic_cast<to_real*>(expr);
    if (to_real_ != nullptr) {
        luna_type t = get_type(to_real_->expr_);
        switch (t) {
            case INT: return REAL;
            case REAL: return REAL;
            case STRING: return ERROR_TYPE;
            case ERROR_TYPE: return ERROR_TYPE;
            case UNDEFINED: return UNDEFINED;
        }
        throw new std::runtime_error("unexpected type");
    }

    to_str* to_str_ = dynamic_cast<to_str*>(expr);
    if (to_str_ != nullptr) {
        luna_type t = get_type(to_str_->expr_);
        switch (t) {
            case INT: return STRING;
            case REAL: return ERROR_TYPE;
            case STRING: return STRING;
            case ERROR_TYPE: return ERROR_TYPE;
            case UNDEFINED: return UNDEFINED;
        }
        throw new std::runtime_error("unexpected type");
    }

    bin_op* bin_op_ = dynamic_cast<bin_op*>(expr);
    if (bin_op_ != nullptr) {
        luna_type left_type = get_type(bin_op_->left_);
        luna_type right_type = get_type(bin_op_->right_);

        if (left_type == ERROR_TYPE || right_type == ERROR_TYPE) return ERROR_TYPE;
        if (left_type == UNDEFINED || right_type == UNDEFINED) return UNDEFINED;

        if (left_type == INT && (right_type == INT && right_type == REAL)) return right_type;
        if (left_type == INT && right_type == STRING) return ERROR_TYPE;

        if (right_type == INT && (left_type == INT && left_type == REAL)) return left_type;
        if (right_type == INT && left_type == STRING) return ERROR_TYPE;

        if (right_type == STRING && left_type == STRING) return INT; // todo

        throw new std::runtime_error("unexpected type");
    }
}


ERROR_LEVEL is_valid_convert(luna_type from, luna_type to) {
    bool is_valid = true;

    if (from == INT && to == INT) return ERROR_LEVEL::NO_ERROR; 
    if (from == INT && to == REAL) return ERROR_LEVEL::WARNING; 
    if (from == REAL && to == INT) return ERROR_LEVEL::WARNING; 
    if (from == REAL && to == REAL) return ERROR_LEVEL::NO_ERROR;  
    if (from == INT && to == STRING) return ERROR_LEVEL::WARNING; 
    if (from == REAL && to == STRING) return ERROR_LEVEL::ERROR;  
    if (from == STRING && to == STRING) return ERROR_LEVEL::NO_ERROR; 
    if (from == STRING && to == INT) return ERROR_LEVEL::ERROR;  
    if (from == STRING && to == REAL) return ERROR_LEVEL::ERROR; 

    if (from == INT && to == VALUE) return ERROR_LEVEL::ERROR; 
    if (from == REAL && to == VALUE) return ERROR_LEVEL::ERROR; 
    if (from == STRING && to == VALUE) return ERROR_LEVEL::ERROR; 

    if (from == INT && to == NAME) return ERROR_LEVEL::ERROR; 
    if (from == REAL && to == NAME) return ERROR_LEVEL::ERROR; 
    if (from == STRING && to == NAME) return ERROR_LEVEL::ERROR; 

    if (from == UNDEFINED) return ERROR_LEVEL::NO_ERROR;
    if (from == ERROR_TYPE) return ERROR_LEVEL::ERROR;
}

std::vector<luna_type>* ast_analyzer::params_to_types(std::vector<expr *>* params) {
    std::vector<luna_type>* types = new std::vector<luna_type>();
    for (auto param : *(params)) {
        types->push_back(get_type(param));
    }
    return types;
}

std::multimap<std::string , std::vector<luna_type> *>* ast_analyzer::get_types_from_calling(std::multimap<std::string , std::vector<expr*> *>* cur_cfs ) {
    std::multimap<std::string , std::vector<luna_type> *>* calls = new std::multimap<std::string , std::vector<luna_type> *>();

    for (auto cur_cf : *cur_cfs) {
        std::vector<luna_type>* types = new std::vector<luna_type>();
        types = params_to_types(cur_cf.second);
        calls->insert(std::pair<std::string , std::vector<luna_type> *>(cur_cf.first, types))
    }
    return calls;
}

bool ast_analyzer::analyze_calling_undeclarated_func() {
    bool has_errors = false;

    std::vector<sub_def *> sub_defs = *(ast_->get_program()->sub_defs);
    std::map<std::string , std::vector<luna_type> *> cf_decls;
    std::multimap<std::string , std::vector<luna_type> *> calls;
    // std::vector<cf_info<luna_type>*>* calls;

    for (auto i : sub_defs) {
        if (i == nullptr) continue;
        std::cerr << "new sub def dec";

        // собираем все import декларации и иx параметры
        import* import_decl = dynamic_cast<import *> (i); 
        if (import_decl != nullptr) { 

            std::vector<luna_type>* params = new std::vector<luna_type>();
            
            if (import_decl->params_->seq_ != nullptr) {
                for (auto param : *(import_decl->params_->seq_->params_)) {
                    params->push_back(param->type_);
                }
            }
            cf_decls.insert(std::pair<std::string , std::vector<luna_type> *>(import_decl->luna_code_id_->to_string(), params));
        }

        luna_sub_def* luna_sub_def_decl = dynamic_cast<luna_sub_def *> (i); 
        if (luna_sub_def_decl != nullptr) {
            std::vector<luna_type>* params = new std::vector<luna_type>();

            if (luna_sub_def_decl->params_->param_seq_ != nullptr) {
                for (auto param : *(luna_sub_def_decl->params_->param_seq_->params_)) {
                    params->push_back(param->type_);
                }
            }
            cf_decls.insert(std::pair<std::string , std::vector<luna_type> *>(import_decl->luna_code_id_->to_string(), params));

            std::cerr << "before luna sub def decl: get all calling\n";

            std::multimap<std::string , std::vector<expr *> *> cur_cfs = get_all_calling(luna_sub_def_decl->block_);
            std::cerr << "after luna sub def decl: get all calling\n";

            calls = get_types_from_calling(&cur_cfs);
        }
    }

    // std::cerr << "------- cf_decls --------\n";
    // for (auto i : cf_decls) {
    //     std::cerr << i->to_string() << std::endl;
    // }

    // std::cerr << "------- calls --------\n";
    // for (auto i : *calls) {
    //     std::cerr << i->to_string() << std::endl;
    // }

    for (auto call : *calls) {
        luna_string* alias = call->alias_;
        bool has_such_cf = false;

        for (auto func_decl : cf_decls) {
            // не та функция
            if (*(func_decl->alias_->get_value()) != *(alias->get_value())) {
                continue;
            } 

            has_such_cf = true;

            if (func_decl->params_->size() != call->params_->size()) {
                reporter.report(ERROR_LEVEL::ERROR,
                    "The number of parameters in call and declaration doesn't match",
                    "\tLine " + std::to_string(call->alias_->line_) + ": " + get_line_from_file(call->alias_->line_) + " // declaration\n" \
                    + "\tLine " + std::to_string(func_decl->alias_->line_)+ ": " + get_line_from_file(func_decl->alias_->line_) + " // calling \n",
                    0
                );
                break;
            }

            for (int k = 0; k < func_decl->params_->size(); k++) {

                luna_type cur_call_param_type = call->params_->at(k);
                luna_type cur_func_delc_call_param_type = func_decl->params_->at(k);

                ERROR_LEVEL level = is_valid_convert(cur_call_param_type, cur_func_delc_call_param_type);

                if (level == ERROR_LEVEL::NO_ERROR) continue;

                reporter.report(level,
                    "Invalid " + std::to_string(k + 1) + "'th parameter type: \"" + print_type(cur_call_param_type) + "\"",
                    get_line_from_file(call->alias_->line_),
                    call->alias_->line_,
                    print_type(cur_func_delc_call_param_type)
                );
            }
        }

        if (!has_such_cf) {
            reporter.report(ERROR_LEVEL::ERROR,
                "Undefined reference: \"" + *(call->alias_->get_value()) + "\"",
                get_line_from_file(call->alias_->line_),
                call->alias_->line_
            );
        }
    }

    // for (auto i : cf_decls) {
        // delete i;
    // }
    // for (auto i : calls) {
        // delete i;
    // }

    return true;
}

template <typename T>
std::vector<T> ast_analyzer::find_pairs(std::vector<T>* v) {
    std::set<std::string> set;
    std::vector<T> duplicated;

    for (auto i : *v) {
        if ((set.count(*(i->get_value())) > 0)) {
            duplicated.push_back(i);
        }
        else {
            set.insert(*(i->get_value()));
        }
    }
    return duplicated;
}

bool ast_analyzer::analyze_existance_main_cf() {
    std::vector<sub_def *> sub_defs = *(ast_->get_program()->sub_defs);

    bool has_main = false;
    for (auto i : sub_defs) {
        if (i == nullptr) continue;

        luna_sub_def* luna_func = dynamic_cast<luna_sub_def *> (i); 
        if (luna_func == nullptr) continue;

        if (*(luna_func->code_id_->value_) == "main") {
            has_main = true;
            break;
        }
    }

    if (has_main) return false;

    reporter.report(ERROR_LEVEL::ERROR,
        "No sub main",
        "",
        0,
        0
    );
    return true;
}
            
std::map<std::string, std::set<uint>> ast_analyzer::find_redecls(std::vector<luna_string* > values) {
    auto map = std::map<std::string, std::set<uint>>();

    for (auto i : values) {
        if (map.count(*(i->get_value())) == 0) {
            std::set<uint> set = std::set<uint>();
            set.insert(i->line_);
            map.insert(std::pair<std::string, std::set<uint>>(*(i->get_value()), set));
        }
        else {
            map.at(*(i->get_value())).insert(i->line_);
        }
    }
    return map;
}

bool ast_analyzer::has_df_redeclaration(std::vector<luna_string* > prev_values, block* block_) {
    bool has_errors = false;

    std::vector<luna_string* > all_values = prev_values;
    std::vector<luna_string* > block_values = get_block_values(block_);

    all_values.insert(all_values.end(), block_values.begin(), block_values.end());

    std::map<std::string, std::set<uint>> map = find_redecls(all_values);

    for (auto i : map) {
        if (i.second.size() <= 1) continue;
        std::string prev_decls;

        for (auto line : i.second) {
            prev_decls += "\tLine " + std::to_string(line) + ": " + get_line_from_file(line) + '\n';
        }

        reporter.report(ERROR_LEVEL::ERROR,
            "Alias \"" + i.first + "\" multiple declarations",
            prev_decls,
            0 
        );
    }

    for (auto i : *(block_->statement_seq_->statements_)) {
        cf_statement* cur_cf = dynamic_cast<cf_statement*> (i);
        if (cur_cf != nullptr) continue; 

        if_statement* cur_if = dynamic_cast<if_statement*> (i);

        if (cur_if != nullptr) {
            has_df_redeclaration(all_values, cur_if->block_);
            continue;
        }

        while_statement* cur_while = dynamic_cast<while_statement*> (i);
        if (cur_while != nullptr) {
            has_df_redeclaration(all_values, cur_while->block_);
            continue;
        }

        for_statement* cur_for = dynamic_cast<for_statement*> (i);
        if (cur_for != nullptr) {
            has_df_redeclaration(all_values, cur_for->block_);
        }
    }
    return has_errors;
}

std::vector<luna_string *> ast_analyzer::get_block_values(block* block_) {

    std::vector<luna_string* > all_values = std::vector<luna_string* >();

    if (block_->opt_dfdecls_->dfdecls_ != nullptr) {
        auto names = block_->opt_dfdecls_->dfdecls_->name_seq_->names_;
        all_values.insert(all_values.end(), names->begin(), names->end());
    } 

    for (auto i : *(block_->statement_seq_->statements_)) {
        cf_statement* cur_cf = dynamic_cast<cf_statement*> (i);
        if (cur_cf == nullptr) continue;
        if (cur_cf->opt_label_->id_ == nullptr) continue;
        all_values.push_back(new luna_string(cur_cf->opt_label_->id_->get_value(), cur_cf->opt_label_->id_->line_));           
    }

    return all_values;
}

bool ast_analyzer::analyze_df_redeclaration() {
    std::vector<sub_def *> sub_defs = *(ast_->get_program()->sub_defs);
    std::vector<luna_sub_def *> luna_funcs;

    for (auto i : sub_defs) {
        if (i == nullptr) continue;
        luna_sub_def* luna_func = dynamic_cast<luna_sub_def *> (i); 
        if (luna_func == nullptr) continue;
        luna_funcs.push_back(luna_func);
    }

    bool has_errors;

    for (auto luna_func : luna_funcs) {
        std::vector<param *>* params = nullptr;
        if (luna_func->params_->param_seq_ != nullptr) {
            params = luna_func->params_->param_seq_->params_;
        }

        std::vector<luna_string* > all_values = std::vector<luna_string* >();

        if (params != nullptr) {
            for (auto i : *params) {
                all_values.push_back(i->name_);
            }
        }
        has_df_redeclaration(all_values, luna_func->block_);
        all_values.clear();
    }
    return false;
}

bool ast_analyzer::analyze_cf_redeclaration() {
    std::vector<sub_def *> sub_defs = *(ast_->get_program()->sub_defs);
    std::vector<luna_sub_def *> luna_funcs;

    for (auto i : sub_defs) {
        if (i == nullptr) continue;
        luna_sub_def* luna_func = dynamic_cast<luna_sub_def *> (i); 
        if (luna_func == nullptr) continue;
        luna_funcs.push_back(luna_func);
    }

    std::vector<luna_sub_def *> duplicated = find_pairs<luna_sub_def *>(&luna_funcs);

    for (auto i : duplicated) {
        reporter.report(ERROR_LEVEL::ERROR,
            "CF \"" + *(i->code_id_->value_) + "\" is aleady defined.",
            get_line_from_file(i->code_id_->line_),
            i->code_id_->line_
        );
    }

    return duplicated.size() > 0;
}

bool ast_analyzer::analyze_shadow_import() {
    bool has_errors;

    std::vector<sub_def *> sub_defs = *(ast_->get_program()->sub_defs);
    std::vector<luna_string* > aliases;

    for (auto i : sub_defs) {
        if (i == nullptr) continue;
        import* import_decl = dynamic_cast<import *> (i); 
        if (import_decl == nullptr) continue;
        aliases.push_back(import_decl->luna_code_id_);
    }

    std::map<std::string, std::set<uint>> map = find_redecls(aliases); // alias : <line1, line2, line3 ... >

    for (auto i : map) {
        if (i.second.size() <= 1) continue;
        std::string prev_decls;

        for (auto line : i.second) {
            prev_decls += "\tLine " + std::to_string(line) + ": " + get_line_from_file(line) + "\n";
        }

        reporter.report(ERROR_LEVEL::WARNING,
            "LuNA alias \"" + i.first + "\" multiple declarations",
            prev_decls,
            0 
        );
        has_errors = true;
    }
    return has_errors;
}

std::string ast_analyzer::get_line_from_file(uint num) {
    // std::cerr << num;
    int fseek_res = fseek(file_, 0, SEEK_SET);
    if (fseek_res == FSEEK_ERROR) {
        perror("fseek");
        throw std::runtime_error("fseek");
    }

    int i = 1;
    while (i != num) {
        char c = fgetc(file_);
        assert(c != EOF);

        if (c == '\n') {
            i++;
        }
    }

    size_t len = 0;
    char* line = NULL;
    ssize_t string_len = getline(&line, &len, file_);

    if (string_len == -1) {
        throw std::runtime_error("Couldn't allocate memory");
    }

    int start = 0;
    for (int i = 0; i < string_len; i++) {
        if (line[i] != ' '){
            start = i;
            break;
        }
    }

    std::string l;
    for (int i = start; i < string_len - 1; i++) {
        l.push_back(line[i]);
    }

    delete line;
    return l;
}