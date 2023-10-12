// #include "undecl_names_analyzer.hpp"
    
// bool undeclarated_names_analyzer::analyze() {
//     return analyze_unused_variables();
// }

// bool undeclarated_names_analyzer::is_define_in_scope(luna_string* var, std::vector<std::vector<luna_string*>*>* scope) {
//     for (auto i : *scope) {
//         for (auto j : *i) {
//             if (j->to_string() == var->to_string()) {
//                 return true;
//             }
//         }
//     }
//     return false;
// }

// std::vector<luna_string *> undeclarated_names_analyzer::get_vars_from_expr(expr* expr) {
//     std::vector<luna_string *> cur_vars;

//     if (is_int(expr->to_string())) return cur_vars; 
//     if (is_real(expr->to_string())) return cur_vars;
//     if (is_string(expr->to_string())) return cur_vars;

//     luna_string* df = dynamic_cast<luna_string*>(expr);

//     if (df != nullptr) {
//         cur_vars.push_back(df);
//         return cur_vars;
//     }

//     to_int* to_int_ = dynamic_cast<to_int*>(expr);
//     if (to_int_ != nullptr) {
//         std::vector<luna_string *> inner_vars = get_vars_from_expr(to_int_->expr_);
//         cur_vars.insert(cur_vars.end(), inner_vars.begin(), inner_vars.end());
//         return cur_vars;
//     }

//     to_real* to_real_ = dynamic_cast<to_real*>(expr);
//     if (to_real_ != nullptr) {
//         std::vector<luna_string *> inner_vars = get_vars_from_expr(to_real_->expr_);
//         cur_vars.insert(cur_vars.end(), inner_vars.begin(), inner_vars.end());
//         return cur_vars;
//     }

//     to_str* to_str_ = dynamic_cast<to_str*>(expr);
//     if (to_str_ != nullptr) {
//         std::vector<luna_string *> inner_vars = get_vars_from_expr(to_str_->expr_);
//         cur_vars.insert(cur_vars.end(), inner_vars.begin(), inner_vars.end());
//         return cur_vars;
//     }

//     bin_op* bin_op_ = dynamic_cast<bin_op*>(expr);
//     if (bin_op_ != nullptr) {
//         std::vector<luna_string *> inner_left_vars = get_vars_from_expr(bin_op_->left_);
//         cur_vars.insert(cur_vars.end(), inner_left_vars.begin(), inner_left_vars.end());

//         std::vector<luna_string *> inner_right_vars = get_vars_from_expr(bin_op_->right_);
//         cur_vars.insert(cur_vars.end(), inner_right_vars.begin(), inner_right_vars.end());
//         return cur_vars;
//     }

//     throw new std::runtime_error("no such type");
// }

// std::vector<luna_string*>* undeclarated_names_analyzer::get_vars(std::vector<expr*>* exprs) {
//     std::vector<luna_string *>* vars = new std::vector<luna_string *>();

//     for (auto expr : *exprs) {
//         std::vector<luna_string *> expr_vars = get_vars_from_expr(expr);
//         vars->insert(vars->begin(), expr_vars.begin(), expr_vars.end());
//     }
//     return vars;
// }

// bool undeclarated_names_analyzer::check_unused(std::vector<std::vector<luna_string*>*>* scope, block* block_) {
//     std::vector<luna_string *>* cur_variables = new std::vector<luna_string*>();

//     if (block_->opt_dfdecls_->dfdecls_ != nullptr) {
//         cur_variables->insert(cur_variables->end(),
//                             block_->opt_dfdecls_->dfdecls_->name_seq_->names_->begin(),
//                             block_->opt_dfdecls_->dfdecls_->name_seq_->names_->end());
//     } 
//     scope->push_back(cur_variables);

//     for (auto stat : *(block_->statement_seq_->statements_)) {
//         if (stat == nullptr) continue;

//         cf_statement* cur_cf = dynamic_cast<cf_statement*> (stat);

//         if (cur_cf != nullptr ) {
//             if (cur_cf->opt_exprs_->exprs_seq_ != nullptr) {
//                 std::vector<luna_string *>* vars = get_vars(cur_cf->opt_exprs_->exprs_seq_->expr_);

//                 for (auto var : *vars) {
//                     if (!is_define_in_scope(var, scope)) {
//                         reporter_->report(ERROR_LEVEL::ERROR,
//                             "Name " + var->to_string() + " is not defined",
//                             get_line_from_file(var->line_),
//                             var->line_
//                         );
//                     }
//                 }
//             }

//             if (cur_cf->code_id_ != nullptr) {
//                 cur_variables->push_back(cur_cf->code_id_);
//             }

//             continue;
//         }


//         if_statement* cur_if = dynamic_cast<if_statement*> (stat);
//         if (cur_if != nullptr) {
//             std::vector<expr *> v;
//             v.push_back(cur_if->expr_);

//             std::vector<luna_string*>* inner_if_vars = get_vars(&v);

//             for (auto i : *inner_if_vars) {
//                 if (!is_define_in_scope(i, scope)) {
//                     reporter_->report(ERROR_LEVEL::ERROR,
//                         "Name " + i->to_string() + " is not defined",
//                         get_line_from_file(i->line_),
//                         i->line_
//                     );
//                 }
//             }

//             scope->push_back(cur_variables);
//             check_unused(scope, cur_if->block_);
//             scope->pop_back();
//             continue;
//         }

//         while_statement* cur_while = dynamic_cast<while_statement*> (stat);
//         if (cur_while != nullptr) {
//             std::vector<expr *> v;
//             v.push_back(cur_while->left_);
//             // v.push_back(cur_while->right_);

//             std::vector<luna_string*>* while_vars = get_vars(&v);

//             for (auto i : *while_vars) {
//                 if (!is_define_in_scope(i, scope)) {
//                     std::cerr << i->to_string();
//                     reporter_->report(ERROR_LEVEL::ERROR,
//                         "Name " + i->to_string() + " is not defined",
//                         get_line_from_file(i->line_),
//                         i->line_
//                     );
//                 }
//             }

//             scope->push_back(cur_variables);
//             check_unused(scope, cur_while->block_);
//             scope->pop_back();
//             continue;
//         }

//         for_statement* cur_for = dynamic_cast<for_statement*> (stat);
//         if (cur_for != nullptr) {
//             std::vector<expr *> v;
//             v.push_back(cur_for->expr_1_);
//             v.push_back(cur_for->expr_2_);

//             std::vector<luna_string*>* for_vars = get_vars(&v);
//             for (auto i : *for_vars) {
//                 if (!is_define_in_scope(i, scope)) {
//                     reporter_->report(ERROR_LEVEL::ERROR,
//                         "Name " + i->to_string() + " is not defined",
//                         get_line_from_file(i->line_),
//                         i->line_
//                     );
//                 }
//             }

//             scope->push_back(cur_variables);
//             check_unused(scope, cur_for->block_);
//             scope->pop_back();
//             continue;
//         }
//     }
//     return true;
// }

// std::vector<luna_sub_def*> undeclarated_names_analyzer::get_luna_sub_defs(ast* ast_) {
//     std::vector<luna_sub_def*> luna_sub_defs;
//     std::vector<sub_def *> sub_defs = *(ast_->get_program()->sub_defs);
//     for (auto i : sub_defs) {
//         if (i == nullptr) continue;
//         luna_sub_def* luna_sub_def_decl = dynamic_cast<luna_sub_def *> (i); 
//         if (luna_sub_def_decl != nullptr) {
//             luna_sub_defs.push_back(luna_sub_def_decl);
//         }
//     }
//     return luna_sub_defs;
// }

// bool undeclarated_names_analyzer::analyze_unused_variables() {
//     std::vector<std::vector<luna_string*>*>* scope = new std::vector<std::vector<luna_string*>*>();

//     std::vector<luna_sub_def*> luna_funcs = get_luna_sub_defs(ast_);
//     bool has_errors = false;

//     // for (auto i : luna_funcs) {
//     //     std::cerr << i->to_string();
//     // }

//     for (auto i : luna_funcs) {
//         std::vector<luna_string*> luna_sub_def_params; 

//         if (i->params_->param_seq_ != nullptr) {
//             for (auto param : *(i->params_->param_seq_->params_)) {
//                 luna_sub_def_params.push_back(param->name_);
//             }
//             scope->push_back(&luna_sub_def_params);
//         }

//         has_errors = check_unused(scope, i->block_);
//     }

//     delete scope;
//     return has_errors;
// }


// std::string undeclarated_names_analyzer::get_line_from_file(uint num) {
//     // std::cerr << num;
//     int fseek_res = fseek(file_, 0, SEEK_SET);
//     if (fseek_res == FSEEK_ERROR) {
//         perror("fseek");
//         throw std::runtime_error("fseek");
//     }

//     int i = 1;
//     while (i != num) {
//         char c = fgetc(file_);
//         assert(c != EOF);

//         if (c == '\n') {
//             i++;
//         }
//     }

//     size_t len = 0;
//     char* line = NULL;
//     ssize_t string_len = getline(&line, &len, file_);

//     if (string_len == -1) {
//         throw std::runtime_error("Couldn't allocate memory");
//     }

//     int start = 0;
//     for (int i = 0; i < string_len; i++) {
//         if (line[i] != ' '){
//             start = i;
//             break;
//         }
//     }

//     std::string l;
//     for (int i = start; i < string_len - 1; i++) {
//         l.push_back(line[i]);
//     }

//     delete line;
//     return l;
// }