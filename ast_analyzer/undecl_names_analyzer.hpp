#pragma once
#include "../parser/ast.hpp"
#include "error_reporter.hpp"
#include "base_analyzer.hpp"
#include "utils.cpp"

#include <vector>
#include <assert.h>

class undeclarated_names_analyzer : public base_analyzer {
public:
    undeclarated_names_analyzer(ast* ast_, FILE* yyin, error_reporter* reporter) {
        this->ast_ = ast_;
        this->file_ = yyin;
        this->reporter_ = reporter;
    }

    std::string get_name() override {
        return "undeclarated_names_analyzer";
    }

    bool analyze() override {
        return analyze_undecl_variables();
    }

    bool is_define_in_scope(luna_string* var, std::vector<std::vector<luna_string*>*>* scope) {
        for (auto i : *scope) {
            for (auto j : *i) {
                if (j->to_string() == var->to_string()) {
                    return true;
                }
            }
        }
        return false;
    }

    std::vector<luna_string *> get_vars_from_expr(expr* expr) {
        std::vector<luna_string *> cur_vars;

        if (is_int(expr->to_string())) return cur_vars; 
        if (is_real(expr->to_string())) return cur_vars;
        if (is_string(expr->to_string())) return cur_vars;

        luna_string* df = dynamic_cast<luna_string*>(expr);

        if (df != nullptr) {
            cur_vars.push_back(df);
            return cur_vars;
        }

        to_int* to_int_ = dynamic_cast<to_int*>(expr);
        if (to_int_ != nullptr) {
            std::vector<luna_string *> inner_vars = get_vars_from_expr(to_int_->expr_);
            cur_vars.insert(cur_vars.end(), inner_vars.begin(), inner_vars.end());
            return cur_vars;
        }

        to_real* to_real_ = dynamic_cast<to_real*>(expr);
        if (to_real_ != nullptr) {
            std::vector<luna_string *> inner_vars = get_vars_from_expr(to_real_->expr_);
            cur_vars.insert(cur_vars.end(), inner_vars.begin(), inner_vars.end());
            return cur_vars;
        }

        to_str* to_str_ = dynamic_cast<to_str*>(expr);
        if (to_str_ != nullptr) {
            std::vector<luna_string *> inner_vars = get_vars_from_expr(to_str_->expr_);
            cur_vars.insert(cur_vars.end(), inner_vars.begin(), inner_vars.end());
            return cur_vars;
        }

        simple_id* simple_id_ = dynamic_cast<simple_id*>(expr);

        if (simple_id_ != nullptr) {
            cur_vars.push_back(simple_id_->value_);
            return cur_vars;
        }

        complex_id* complex_id_ = dynamic_cast<complex_id*>(expr);
        if (complex_id_ != nullptr) {
            auto inner_vars1 = get_vars_from_expr(complex_id_->id_);
            cur_vars.insert(cur_vars.end(), inner_vars1.begin(), inner_vars1.end());

            auto inner_vars = get_vars_from_expr(complex_id_->expr_);
            cur_vars.insert(cur_vars.end(), inner_vars.begin(), inner_vars.end());
            return cur_vars;
        }

        bin_op* bin_op_ = dynamic_cast<bin_op*>(expr);
        if (bin_op_ != nullptr) {
            std::vector<luna_string *> inner_left_vars = get_vars_from_expr(bin_op_->left_);
            cur_vars.insert(cur_vars.end(), inner_left_vars.begin(), inner_left_vars.end());

            std::vector<luna_string *> inner_right_vars = get_vars_from_expr(bin_op_->right_);
            cur_vars.insert(cur_vars.end(), inner_right_vars.begin(), inner_right_vars.end());
            return cur_vars;
        }

        throw new std::runtime_error("no such type");
    }

    std::vector<luna_string*>* get_vars(std::vector<expr*>* exprs) {
        std::vector<luna_string *>* vars = new std::vector<luna_string *>();

        for (auto expr : *exprs) {
            std::vector<luna_string *> expr_vars = get_vars_from_expr(expr);
            vars->insert(vars->begin(), expr_vars.begin(), expr_vars.end());
        }
        return vars;
    }

    bool check_unused(std::vector<std::vector<luna_string*>*>* scope, block* block_) {
        std::vector<luna_string *>* cur_variables = new std::vector<luna_string*>();

        if (block_->opt_dfdecls_->dfdecls_ != nullptr) {
            cur_variables->insert(cur_variables->end(),
                                block_->opt_dfdecls_->dfdecls_->name_seq_->names_->begin(),
                                block_->opt_dfdecls_->dfdecls_->name_seq_->names_->end());
        } 
        scope->push_back(cur_variables);

        for (auto stat : *(block_->statement_seq_->statements_)) {
            if (stat == nullptr) continue;

            cf_statement* cur_cf = dynamic_cast<cf_statement*> (stat);

            if (cur_cf != nullptr) {

                if (cur_cf->opt_exprs_->exprs_seq_ != nullptr) {
                    std::vector<luna_string *>* vars = get_vars(cur_cf->opt_exprs_->exprs_seq_->expr_);

                    for (auto var : *vars) {
                        if (!is_define_in_scope(var, scope)) {
                            reporter_->report(ERROR_LEVEL::ERROR,
                                "Name \"" + var->to_string() + "\" is used, but not defined",
                                get_line_from_file(var->line_),
                                var->line_
                            );
                        }
                    }
                }

                if (cur_cf->code_id_ != nullptr) {
                    cur_variables->push_back(cur_cf->code_id_);
                }

                continue;
            }


            if_statement* cur_if = dynamic_cast<if_statement*> (stat);
            if (cur_if != nullptr) {
                std::vector<expr *> v;
                v.push_back(cur_if->expr_);

                std::vector<luna_string*>* inner_if_vars = get_vars(&v);

                for (auto i : *inner_if_vars) {
                    if (!is_define_in_scope(i, scope)) {
                        reporter_->report(ERROR_LEVEL::ERROR,
                            "Name \"" + i->to_string() + "\" is used, but not defined",
                            get_line_from_file(i->line_),
                            i->line_
                        );
                    }
                }

                check_unused(scope, cur_if->block_);
                scope->pop_back();
                continue;
            }

            while_statement* cur_while = dynamic_cast<while_statement*> (stat);
            if (cur_while != nullptr) {
                std::vector<luna_string*> inner_scope;
                inner_scope.push_back(cur_while->left_);
                // std::cerr << cur_while->left_->to_string() << std::endl;
                scope->push_back(&inner_scope);

                std::vector<expr *> v;
                v.push_back(cur_while->expr_);
                v.push_back(cur_while->right_);
                v.push_back(cur_while->id_);

                std::vector<luna_string*>* while_vars = get_vars(&v);

                for (auto i : *while_vars) {
                    if (!is_define_in_scope(i, scope)) {
                        reporter_->report(ERROR_LEVEL::ERROR,
                            "Name \"" + i->to_string() + "\" is used, but not defined",
                            get_line_from_file(i->line_),
                            i->line_
                        );
                    }
                }

                check_unused(scope, cur_while->block_);
                scope->pop_back();
                scope->pop_back();
                continue;
            }

            for_statement* cur_for = dynamic_cast<for_statement*> (stat);
            if (cur_for != nullptr) {
                std::vector<expr *> v;

                v.push_back(cur_for->expr_1_);
                v.push_back(cur_for->expr_2_);

                check_for_statement_types(cur_for);

                std::vector<luna_string*>* for_vars = get_vars(&v);
                for (auto i : *for_vars) {

                    if (!is_define_in_scope(i, scope)) {
                        reporter_->report(ERROR_LEVEL::ERROR,
                            "Name \"" + i->to_string() + "\" is used, but not defined",
                            get_line_from_file(i->line_),
                            i->line_
                        );
                    }
                }

                check_unused(scope, cur_for->block_);
                scope->pop_back();
                continue;
            }
        }
        return true;
    }

    std::vector<luna_sub_def*> get_luna_sub_defs(ast* ast_) {
        std::vector<luna_sub_def*> luna_sub_defs;
        std::vector<sub_def *> sub_defs = *(ast_->get_program()->sub_defs);
        for (auto i : sub_defs) {
            if (i == nullptr) continue;
            luna_sub_def* luna_sub_def_decl = dynamic_cast<luna_sub_def *> (i); 
            if (luna_sub_def_decl != nullptr) {
                luna_sub_defs.push_back(luna_sub_def_decl);
            }
        }
        return luna_sub_defs;
    }


    bool analyze_undecl_variables() {
        std::vector<std::vector<luna_string*>*>* scope = new std::vector<std::vector<luna_string*>*>();

        std::vector<luna_sub_def*> luna_funcs = get_luna_sub_defs(ast_);
        bool has_errors = false;

        for (auto i : luna_funcs) {
            std::vector<luna_string*> luna_sub_def_params; 

            if (i->params_->param_seq_ != nullptr) {
                for (auto param : *(i->params_->param_seq_->params_)) {
                    luna_sub_def_params.push_back(param->name_);
                }
                scope->push_back(&luna_sub_def_params);
            }

            has_errors = check_unused(scope, i->block_);
        }

        delete scope;
        return has_errors;
    }

    void check_for_statement_types(for_statement* for_stat) {
        expr* e1 = for_stat->expr_1_;
        expr* e2 = for_stat->expr_2_;

        if (is_real(e2->to_string()) || is_string(e2->to_string()) || is_real(e1->to_string()) || is_string(e1->to_string())) {
            reporter_->report(ERROR_LEVEL::ERROR,
                "Invalid type of expression",
                get_line_from_file(for_stat->line_),
                for_stat->line_,
                "integer or value"
            );
        }

        if (is_int(e1->to_string()) && is_int(e2->to_string())) {
            int l = std::stoi(e1->to_string());
            int r = std::stoi(e2->to_string());

            if (r < l) {
                reporter_->report(ERROR_LEVEL::ERROR,
                    "Bad range ",
                    get_line_from_file(for_stat->line_),
                    for_stat->line_
                );
            }
        }
    }
};