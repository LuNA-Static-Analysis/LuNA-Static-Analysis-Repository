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
    std::cerr << "1\n";
    has_errors = analyze_shadow_import();
    std::cerr << "1\n";
    has_errors = analyze_df_redeclaration();
    std::cerr << "1\n";
    has_errors = analyze_existance_main_cf();
    std::cerr << "1\n";
    has_errors = analyze_cf_redeclaration();
    std::cerr << "1\n";
    has_errors = analyze_calling_undeclarated_func();
    std::cerr << "1\n";
    return has_errors;
}

std::vector<ast_analyzer::cf_info<expr *> *> ast_analyzer::get_all_calling(block* block) {
    std::vector<cf_info<expr *> *> cfs;
    for (auto i : *(block->statement_seq_->statements_)) {
        if (i == nullptr) continue;

        cf_statement* cur_cf = dynamic_cast<cf_statement*> (i);
        if (cur_cf != nullptr) {
            cf_info<expr *>* c;
            if (cur_cf->opt_exprs_->exprs_seq_ == nullptr) {
                c = new cf_info<expr *>(cur_cf->code_id_, new std::vector<expr *>());
            }
            else {
                c = new cf_info<expr *>(cur_cf->code_id_, cur_cf->opt_exprs_->exprs_seq_->expr_);
            }
            cfs.push_back(c);
            continue;
        }

        if_statement* cur_if = dynamic_cast<if_statement*> (i);
        if (cur_if != nullptr) {
            std::vector<ast_analyzer::cf_info<expr *> *> inner_cfs = get_all_calling(cur_if->block_);
            cfs.insert(cfs.end(), inner_cfs.begin(), inner_cfs.end());
            continue;
        }

        while_statement* cur_while = dynamic_cast<while_statement*> (i);
        if (cur_while != nullptr) {
            std::vector<ast_analyzer::cf_info<expr *> *> inner_cfs = get_all_calling(cur_while->block_);
            cfs.insert(cfs.end(), inner_cfs.begin(), inner_cfs.end());
            continue;
        }

        for_statement* cur_for = dynamic_cast<for_statement*> (i);
        if (cur_for != nullptr) {
            std::vector<ast_analyzer::cf_info<expr *> *> inner_cfs = get_all_calling(cur_for->block_);
            cfs.insert(cfs.end(), inner_cfs.begin(), inner_cfs.end());
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


bool ast_analyzer::analyze_calling_undeclarated_func() {
    bool has_errors = false;

    std::vector<sub_def *> sub_defs = *(ast_->get_program()->sub_defs);
    std::vector<cf_info<luna_string *> *> func_aliases;
    std::vector<cf_info<expr *>*> cfs;
    std::vector<cf_info<luna_string *>*> calls;

    for (auto i : sub_defs) {
        if (i == nullptr) continue;

        import* import_decl = dynamic_cast<import *> (i); 
        if (import_decl != nullptr) { 
            cf_info<luna_string *>* c = new cf_info<luna_string *>(import_decl->luna_code_id_, new std::vector<luna_string *>());

            if (import_decl->params_->seq_ != nullptr) {
                for (auto param : *(import_decl->params_->seq_->params_)) {
                    c->params_->push_back(param->type_);
                }
            }
            func_aliases.push_back(c);
        }

        luna_sub_def* luna_sub_def_decl = dynamic_cast<luna_sub_def *> (i); 
        if (luna_sub_def_decl != nullptr) {
            cf_info<luna_string *>* c = new cf_info<luna_string *>(luna_sub_def_decl->code_id_, new std::vector<luna_string *>());
            if (luna_sub_def_decl->params_->param_seq_ != nullptr) {
                for (auto param : *(luna_sub_def_decl->params_->param_seq_->params_)) {
                    c->params_->push_back(param->type_);
                }
            }
            func_aliases.push_back(c);
            std::vector<cf_info<expr *>*> cur_cfs = get_all_calling(luna_sub_def_decl->block_);

            for (auto cur_cf : cur_cfs) {
                std::vector<luna_string*>* types = new std::vector<luna_string *>();
                for (auto param : *(cur_cf->params_)) {

                    to_int* to_int_ = dynamic_cast<to_int*>(param);
                    if (to_int_ != nullptr) {
                        types->push_back(new luna_string(new std::string("int")));
                        continue;
                    }

                    to_real* to_real_ = dynamic_cast<to_real*>(param);
                    if (to_real_ != nullptr) {
                        types->push_back(new luna_string(new std::string("real")));
                        continue;
                    }

                    to_str* to_str_ = dynamic_cast<to_str*>(param);
                    if (to_str_ != nullptr) {
                        types->push_back(new luna_string(new std::string("string")));
                        continue;
                    }

                    eq* eq_ = dynamic_cast<eq*>(param);
                    if (eq_ != nullptr) {
                        types->push_back(new luna_string(new std::string("int")));
                        continue;
                    }
                    neq* neq_ = dynamic_cast<neq*>(param);
                    if (neq_ != nullptr) {
                        types->push_back(new luna_string(new std::string("int")));
                        continue;
                    }
                    dbleq* dbleq_ = dynamic_cast<dbleq*>(param);
                    if (dbleq_ != nullptr) {
                        types->push_back(new luna_string(new std::string("int")));
                        continue;
                    }
                    
                    lt* lt_ = dynamic_cast<lt*>(param);
                    if (lt_ != nullptr) {
                        types->push_back(new luna_string(new std::string("int")));
                        continue;
                    }

                    gt* gt_ = dynamic_cast<gt*>(param);
                    if (gt_ != nullptr) {
                        types->push_back(new luna_string(new std::string("int")));
                        continue;
                    }

                    mod* mod_ = dynamic_cast<mod*>(param);
                    if (mod_ != nullptr) {
                        types->push_back(new luna_string(new std::string("int")));
                        continue;
                    }

                    geq* geq_ = dynamic_cast<geq*>(param);
                    if (geq_ != nullptr) {
                        types->push_back(new luna_string(new std::string("int")));
                        continue;
                    }

                    leq* leq_ = dynamic_cast<leq*>(param);
                    if (leq_ != nullptr) {
                        types->push_back(new luna_string(new std::string("int")));
                        continue;
                    }

                    if (is_int(param->to_string())) {
                        types->push_back(new luna_string(new std::string("int")));
                        continue;
                    }
                    else if (is_real(param->to_string())) {
                        types->push_back(new luna_string(new std::string("real")));
                        continue;
                    }

                    else if (is_string(param->to_string())) {
                        types->push_back(new luna_string(new std::string("string")));
                        continue;
                    }

                    types->push_back(nullptr);
                }
                calls.push_back(new cf_info<luna_string *>(cur_cf->alias_, types));
            }
        }
    }

    for (auto i : calls) {
        luna_string*  alias = i->alias_;
        bool has_such_cf = false;
        for (auto j : func_aliases) {
            if (*(j->alias_->get_value()) != *(alias->get_value())) {
                continue;
            } 
            has_such_cf = true;

            if (j->params_->size() != i->params_->size()) {
                reporter.report(ERROR_LEVEL::ERROR,
                    "The number of parameters in call and declaration doesn't match",
                    "\tLine " + std::to_string(j->alias_->line_) + ": " + get_line_from_file(j->alias_->line_) + " // declaration\n" \
                    + "\tLine " + std::to_string(i->alias_->line_)+ ": " + get_line_from_file(i->alias_->line_) + " // calling \n",
                    0
                );

                break;
            }

            for (int k = 0; k < j->params_->size(); k++) {
                if (i->params_->at(k) == nullptr) {
                    continue; // runtime definition type
                }

                if (*(j->params_->at(k)->get_value()) != *(i->params_->at(k)->get_value())) {
                    reporter.report(ERROR_LEVEL::ERROR,
                        "Invalid " + std::to_string(k + 1) + "'th parameter type: \"" + (*(i->params_->at(k)->get_value())) + "\"",
                        get_line_from_file(i->alias_->line_),
                        i->alias_->line_,
                        *(j->params_->at(k)->get_value())
                    );
                    break;
                }
            }
        }

        if (!has_such_cf) {
            reporter.report(ERROR_LEVEL::ERROR,
                "Undefined reference: \"" + *(i->alias_->get_value()) + "\"",
                get_line_from_file(i->alias_->line_),
                i->alias_->line_
            );
        }
    }

    // for (auto i : func_aliases) {
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