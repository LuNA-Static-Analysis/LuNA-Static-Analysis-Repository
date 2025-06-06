#pragma once
#include "undecl_names_analyzer.hpp"
#include "ast_json_reporter.cpp"

class unused_names_analyzer : public undeclarated_names_analyzer {

public:
    unused_names_analyzer(ast* ast_, FILE* yyin, AstErrorReporter::ErrorReporter* reporter, std::string luna_source) 
                : undeclarated_names_analyzer(ast_, yyin, reporter, luna_source) {}

    bool analyze() override {
        return analyze_unused_variables();
    }

    std::string get_name() override {
        return "unused_names_analyzer";
    }

    bool analyze_unused_variables() {
        std::vector<std::vector<luna_string*>*>* scope = new std::vector<std::vector<luna_string*>*>();

        std::vector<luna_sub_def*> luna_funcs = get_luna_sub_defs(ast_);
        bool has_errors = false;

        // for (auto i : luna_funcs) {
        //     std::cerr << i->to_string() << std::endl;
        // }

        for (auto i : luna_funcs) {
            if (i == nullptr) continue;

            std::vector<luna_string*> luna_sub_def_params; 

            if (i->params_->param_seq_ != nullptr) {
                for (auto param : *(i->params_->param_seq_->params_)) {
                    luna_sub_def_params.push_back(param->name_);
                }
                scope->push_back(&luna_sub_def_params);
            }
            has_errors = check_(scope, i->block_);

            current_cf = i->code_id_->to_string();
            // for (auto s : *scope) {
            //     for (auto j : *s) {
            //         std::cerr << j->to_string() << std::endl;
            //     }
            // }

            for (auto s : *scope) {
                for (auto var : *s) {
                    AstErrorReporter::Identifier id{var->to_string(), {get_file(), var->line_, current_cf}};
                    reporter_->addSEM4(id);
                }
            }

            scope->clear();
        }

        delete scope;
        return has_errors;
    }

    void delete_var_from_scope(std::vector<std::vector<luna_string*>*>* scope, luna_string* var) {
        if (!var) return; 
        std::string var_str = var->to_string();
    
        for (auto& inner_vec : *scope) {
            auto it = inner_vec->begin();
            while (it != inner_vec->end()) {
                if (*it && (*it)->to_string() == var_str) {
                    it = inner_vec->erase(it);
                } else {
                    it++;
                }
            }
        }
    }

    bool check_(std::vector<std::vector<luna_string*>*>* scope, block* block_) {
        std::vector<luna_string *>* cur_variables = new std::vector<luna_string*>();


        if (block_->opt_dfdecls_->dfdecls_ != nullptr) {
            // std::cerr << block_->opt_dfdecls_->dfdecls_->name_seq_->to_string();
            cur_variables->insert(cur_variables->end(),
                                block_->opt_dfdecls_->dfdecls_->name_seq_->names_->begin(),
                                block_->opt_dfdecls_->dfdecls_->name_seq_->names_->end());
        } 

        scope->push_back(cur_variables);

        // for (auto s : *scope) {
        //     for (auto j : *s) {
        //         std::cerr << j->to_string() << std::endl;
        //     }
        // }


        for (auto stat : *(block_->statement_seq_->statements_)) {
            if (stat == nullptr) continue;

            cf_statement* cur_cf = dynamic_cast<cf_statement*> (stat);

            if (cur_cf != nullptr ) {
                if (cur_cf->opt_exprs_->exprs_seq_ != nullptr) {
                    std::vector<luna_string *>* vars = get_vars(cur_cf->opt_exprs_->exprs_seq_->expr_);

                    // for (auto j : *vars) {
                    //     std::cerr << j->to_string() << std::endl;
                    // }

                    for (auto var : *vars) {
                        delete_var_from_scope(scope, var);
                    }
                }

                // // todo: проверка, что simple id
                // if (cur_cf->opt_label_->id_ != nullptr) {
                //     cur_variables->push_back(cur_cf->opt_label_->id_);
                // }
                continue;
            }


            if_statement* cur_if = dynamic_cast<if_statement*> (stat);
            if (cur_if != nullptr) {
                std::vector<expr *> v;
                v.push_back(cur_if->expr_);

                std::vector<luna_string*>* inner_if_vars = get_vars(&v);

                for (auto i : *inner_if_vars) {
                    delete_var_from_scope(scope, i);
                }

                check_(scope, cur_if->block_);
                
                for (auto var : *(scope->back())) {
                    AstErrorReporter::Identifier id{var->to_string(), {get_file(), var->line_, current_cf}};
                    reporter_->addSEM4(id);
                }

                scope->pop_back();
                continue;
            }

            // todo
            while_statement* cur_while = dynamic_cast<while_statement*> (stat);
            if (cur_while != nullptr) {
                std::vector<expr *> v;
                v.push_back(cur_while->left_);
                // v.push_back(cur_while->right_);

                std::vector<luna_string*>* while_vars = get_vars(&v);

                for (auto i : *while_vars) {
                    delete_var_from_scope(scope, i);
                }

                check_(scope, cur_while->block_);

                for (auto var : *(scope->back())) {
                    AstErrorReporter::Identifier id{var->to_string(), {get_file(), var->line_, current_cf}};
                    reporter_->addSEM4(id);
                }

                scope->pop_back();
                continue;
            }

            for_statement* cur_for = dynamic_cast<for_statement*> (stat);
            if (cur_for != nullptr) {
                std::vector<expr *> v;
                v.push_back(cur_for->expr_1_);
                v.push_back(cur_for->expr_2_);

                std::vector<luna_string*>* for_vars = get_vars(&v);
                for (auto i : *for_vars) {
                    delete_var_from_scope(scope, i);
                }

                check_(scope, cur_for->block_);

                for (auto var : *(scope->back())) {
                    AstErrorReporter::Identifier id{var->to_string(), {get_file(), var->line_, current_cf}};
                    reporter_->addSEM4(id);
                }

                scope->pop_back();
                continue;
            }
        }
        return true;
    }
};