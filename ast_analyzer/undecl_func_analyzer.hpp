#include "base_analyzer.hpp"
#include "utils.cpp"

class undecl_func_analyzer : public base_analyzer {
public:
    undecl_func_analyzer(ast* ast_, FILE* yyin, error_reporter* reporter, std::string luna_source)  {
        this->ast_ = ast_;
        this->file_ = yyin;
        this->reporter_ = reporter;
        this->real_luna_source_ = luna_source;
    }

    bool analyze() override {
        return analyze_calling_undeclarated_func();
    }

    std::string get_name() override {
        return "undecl_func_analyzer";
    }

    bool analyze_calling_undeclarated_func() {
        bool has_errors = false;

        std::vector<sub_def *> sub_defs = *(ast_->get_program()->sub_defs);

        std::map<luna_string , std::vector<luna_type> *> cf_decls;
        std::multimap<luna_string , std::vector<luna_type> *>* calls = nullptr;

        for (auto i : sub_defs) {
            if (i == nullptr) continue;

            // собираем все import декларации и иx параметры
            import* import_decl = dynamic_cast<import *> (i); 
            if (import_decl != nullptr) { 
                std::vector<luna_type>* params = new std::vector<luna_type>();
                
                if (import_decl->params_->seq_ != nullptr) {
                    for (auto param : *(import_decl->params_->seq_->params_)) {
                        params->push_back(param->get_type());
                    }
                }

                cf_decls.erase(*(import_decl->luna_code_id_));
                cf_decls.insert(std::make_pair(*(import_decl->luna_code_id_), params));
            }

            luna_sub_def* luna_sub_def_decl = dynamic_cast<luna_sub_def *> (i); 
            if (luna_sub_def_decl != nullptr) {
                std::vector<luna_type>* params = new std::vector<luna_type>();

                if (luna_sub_def_decl->params_->param_seq_ != nullptr) {
                    for (auto param : *(luna_sub_def_decl->params_->param_seq_->params_)) {
                        params->push_back(param->get_type());
                    }
                }

                cf_decls.erase(*(luna_sub_def_decl->code_id_));
                cf_decls.insert(std::pair<luna_string , std::vector<luna_type> *>(*(luna_sub_def_decl->code_id_), params));

                std::multimap<luna_string, std::vector<expr *> *> cur_cfs = get_all_calling(luna_sub_def_decl->block_);
                calls = get_types_from_calling(&cur_cfs);

                // собирает все вызовы которые не внутри if 
                // это проверка безусловной рекурсии
                std::vector<luna_string> cfs = get_cfs(luna_sub_def_decl->block_);
                for (auto i : cfs) {
                    if (i.to_string() == luna_sub_def_decl->code_id_->to_string()) {
                        details detail = details("34");
                        detail.add_call_stack_entry(call_stack_entry(get_file(), i.line_, current_cf));
                        reporter_->report_json(detail);
                    }
                }
            }
        }

        // std::cerr << "------- cf_decls --------\n";
        // for (auto i : cf_decls) {
        //     std::cerr << i.first.to_string() << " : " ;
        //     for (auto j : *(i.second)) {
        //         std::cerr << j << ", ";
        //     }
        //     std::cerr << std::endl;
        // }

        // std::cerr << "------- calls --------\n";
        // for (auto i : *calls) {
        //     std::cerr << i.first.to_string() << " : " ;
        //     for (auto j : *(i.second)) {
        //         std::cerr << j << ", ";
        //     }
        //     std::cerr << std::endl;
        // }

        if (calls == nullptr) {
            for (auto i : cf_decls) {
                delete i.second;
            }
            return false;
        }

        for (auto call : *calls) {
            luna_string alias = call.first;

            bool has_such_cf = false;

            for (auto func_decl : cf_decls) {

                // не та функция
                if (func_decl.first.to_string() != alias.to_string()) {
                    continue;
                } 

                has_such_cf = true;

                if (func_decl.second->size() != call.second->size()) {
                    details detail = details("06");
                    detail.add_call_stack_entry(call_stack_entry(get_file(), call.first.line_, current_cf));
                    detail.add_cf(cf(func_decl.first.to_string(), "extern", get_file(), func_decl.first.line_));
                    reporter_->report_json(detail);

                    break;
                }

                for (int k = 0; k < func_decl.second->size(); k++) {

                    luna_type cur_call_param_type = call.second->at(k);
                    luna_type cur_func_delc_call_param_type = func_decl.second->at(k);

                    ERROR_LEVEL level = is_valid_convert(cur_call_param_type, cur_func_delc_call_param_type);

                    if (level == ERROR_LEVEL::NO_ERROR) continue;

                    details detail = details("04");
                    detail.add_call_stack_entry(call_stack_entry(get_file(), call.first.line_, current_cf));
                    detail.add_cf(cf(func_decl.first.to_string(), "extern", get_file(), func_decl.first.line_));
                    reporter_->report_json(detail);
                }
            }

            if (!has_such_cf) {
                details detail = details("02");
                detail.add_call_stack_entry(call_stack_entry(get_file(), call.first.line_, current_cf));
                reporter_->report_json(detail);
            }
        }

        for (auto i : cf_decls) {
            delete i.second;
        }
        
        for (auto i : *calls) {
            if (i.second->size() == 0) delete i.second;
        }

        delete calls;

        return true;
    }


    std::vector<luna_string> get_cfs(block* block) {
        std::vector<luna_string> cfs;

        for (auto i : *(block->statement_seq_->statements_)) {
            if (i == nullptr) continue;

            cf_statement* cur_cf = dynamic_cast<cf_statement*> (i);
            if (cur_cf != nullptr) {
                cfs.push_back(*cur_cf->code_id_);
                continue;
            }

            // while_statement* cur_while = dynamic_cast<while_statement*> (i);
            // if (cur_while != nullptr) {
            //     auto inner_cfs = get_cfs(cur_while->block_);
            //     cfs.insert(cfs.end(), inner_cfs.begin(), inner_cfs.end());
            //     continue;
            // }

            for_statement* cur_for = dynamic_cast<for_statement*> (i);
            if (cur_for != nullptr) {
                auto inner_cfs = get_cfs(cur_for->block_);
                cfs.insert(cfs.end(), inner_cfs.begin(), inner_cfs.end());
                continue;
            }
        }
        return cfs;
    }


    std::multimap<luna_string, std::vector<expr*>*> get_all_calling(block* block) {
        std::multimap<luna_string, std::vector<expr*>*> cfs;

        for (auto i : *(block->statement_seq_->statements_)) {
            if (i == nullptr) continue;

            cf_statement* cur_cf = dynamic_cast<cf_statement*> (i);
            if (cur_cf != nullptr) {
                std::vector<expr*>* c = (cur_cf->opt_exprs_->exprs_seq_ == nullptr 
                                            ? new std::vector<expr*>() 
                                            : cur_cf->opt_exprs_->exprs_seq_->expr_);

                cfs.insert(std::pair<luna_string , std::vector<expr *> *>(*(cur_cf->code_id_), c));
                continue;
            }

            if_statement* cur_if = dynamic_cast<if_statement*> (i);
            if (cur_if != nullptr) {
                std::multimap<luna_string, std::vector<expr*>*> inner_cfs = get_all_calling(cur_if->block_);
                cfs.insert(inner_cfs.begin(), inner_cfs.end());
                continue;
            }

            while_statement* cur_while = dynamic_cast<while_statement*> (i);
            if (cur_while != nullptr) {
                std::multimap<luna_string, std::vector<expr*>*> inner_cfs = get_all_calling(cur_while->block_);
                cfs.insert(inner_cfs.begin(), inner_cfs.end());
                continue;
            }

            for_statement* cur_for = dynamic_cast<for_statement*> (i);
            if (cur_for != nullptr) {
                std::multimap<luna_string, std::vector<expr*>*> inner_cfs = get_all_calling(cur_for->block_);
                cfs.insert(inner_cfs.begin(), inner_cfs.end());
                continue;
            }
        }
        return cfs;
    }


    luna_type get_type(expr* expr) {
        if (is_int(expr->to_string())) return LUNA_INT;

        if (is_real(expr->to_string())) return LUNA_REAL;

        if (is_string(expr->to_string())) return LUNA_STRING;

        to_int* to_int_ = dynamic_cast<to_int*>(expr);
        if (to_int_ != nullptr) {
            luna_type t = get_type(to_int_->expr_);
            switch (t) {
                case LUNA_INT: return LUNA_INT;
                case LUNA_REAL: return LUNA_INT;
                case LUNA_STRING : return LUNA_ERROR_TYPE;
                case LUNA_ERROR_TYPE: return LUNA_ERROR_TYPE;
                case LUNA_UNDEFINED: return LUNA_UNDEFINED;
            }
            throw new std::runtime_error("unexpected type");
        }

        to_real* to_real_ = dynamic_cast<to_real*>(expr);
        if (to_real_ != nullptr) {
            luna_type t = get_type(to_real_->expr_);
            switch (t) {
                case LUNA_INT: return LUNA_REAL;
                case LUNA_REAL: return LUNA_REAL;
                case LUNA_STRING: return LUNA_ERROR_TYPE;
                case LUNA_ERROR_TYPE: return LUNA_ERROR_TYPE;
                case LUNA_UNDEFINED: return LUNA_UNDEFINED;
            }
            throw new std::runtime_error("unexpected type");
        }

        to_str* to_str_ = dynamic_cast<to_str*>(expr);
        if (to_str_ != nullptr) {
            luna_type t = get_type(to_str_->expr_);
            switch (t) {
                case LUNA_INT: return LUNA_STRING;
                case LUNA_REAL: return LUNA_STRING;
                case LUNA_STRING: return LUNA_STRING;
                case LUNA_ERROR_TYPE: return LUNA_ERROR_TYPE;
                case LUNA_UNDEFINED: return LUNA_UNDEFINED;
            }
            throw new std::runtime_error("unexpected type");
        }

        bin_op* bin_op_ = dynamic_cast<bin_op*>(expr);
        if (bin_op_ != nullptr) {
            luna_type left_type = get_type(bin_op_->left_);
            luna_type right_type = get_type(bin_op_->right_);

            if (left_type == LUNA_ERROR_TYPE || right_type == LUNA_ERROR_TYPE) return LUNA_ERROR_TYPE;
            if (left_type == LUNA_UNDEFINED || right_type == LUNA_UNDEFINED) {
                return LUNA_INT;
            }

            if (right_type == LUNA_STRING || left_type == LUNA_STRING) return LUNA_STRING; 
            if (right_type == LUNA_REAL || left_type == LUNA_REAL) return LUNA_REAL;
            if (right_type == LUNA_INT || left_type == LUNA_INT) return LUNA_INT;

            throw new std::runtime_error("unexpected type");
        }
        return LUNA_UNDEFINED;
    }

    ERROR_LEVEL is_valid_convert(luna_type from, luna_type to) {

        // bool is_valid = true;

        if (from == LUNA_INT && to == LUNA_REAL) return ERROR_LEVEL::WARNING; 
        if (from == LUNA_REAL && to == LUNA_INT) return ERROR_LEVEL::WARNING; 
        if (from == LUNA_INT && to == LUNA_STRING) return ERROR_LEVEL::WARNING; 

        if (from == LUNA_STRING && to == LUNA_INT) return ERROR_LEVEL::ERROR;  
        if (from == LUNA_STRING && to == LUNA_REAL) return ERROR_LEVEL::ERROR; 

        if (from == LUNA_INT && to == LUNA_NAME) return ERROR_LEVEL::ERROR; 
        if (from == LUNA_REAL && to == LUNA_NAME) return ERROR_LEVEL::ERROR; 
        if (from == LUNA_STRING && to == LUNA_NAME) return ERROR_LEVEL::ERROR; 

        if (from == LUNA_UNDEFINED) return ERROR_LEVEL::NO_ERROR;
        if (from == LUNA_ERROR_TYPE) return ERROR_LEVEL::ERROR;

        return ERROR_LEVEL::NO_ERROR;
        // throw new std::runtime_error("unexpected type");
    }

    std::vector<luna_type>* params_to_types(std::vector<expr *>* params) {
        std::vector<luna_type>* types = new std::vector<luna_type>();
        for (auto param : *(params)) {
            types->push_back(get_type(param));
        }
        return types;
    }

    std::multimap<luna_string , std::vector<luna_type> *>* get_types_from_calling(std::multimap<luna_string , std::vector<expr*> *>* cur_cfs ) {
        std::multimap<luna_string , std::vector<luna_type> *>* calls = new std::multimap<luna_string , std::vector<luna_type> *>();

        for (auto cur_cf : *cur_cfs) {
            std::vector<luna_type>* types = params_to_types(cur_cf.second);
            calls->insert(std::pair<luna_string , std::vector<luna_type> *>(cur_cf.first, types));
        }

        return calls;
    }
};