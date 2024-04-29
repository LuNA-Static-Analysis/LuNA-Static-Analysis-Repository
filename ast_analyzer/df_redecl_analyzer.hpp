#include "base_analyzer.hpp"

class df_redecl_analyzer : public base_analyzer {
public:
    df_redecl_analyzer(ast* ast_, FILE* yyin, error_reporter* reporter, std::string luna_source)  {
        this->ast_ = ast_;
        this->file_ = yyin;
        this->reporter_ = reporter;
        this->real_luna_source_ = luna_source;
    }

    bool analyze() override {
        return analyze_df_redeclaration();
    }

    std::string get_name() override {
        return "df_redecl_analyzer";
    }

    bool analyze_df_redeclaration() {
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

    bool has_df_redeclaration(std::vector<luna_string* > prev_values, block* block_) {
        bool has_errors = false;

        std::vector<luna_string* > all_values = prev_values;
        std::vector<luna_string* > block_values = get_block_values(block_);

        all_values.insert(all_values.end(), block_values.begin(), block_values.end());

        std::map<std::string, std::set<uint>> map = find_redecls(all_values);

        for (auto i : map) {
            if (i.second.size() <= 1) continue;

            details detail = details("13");
            declared declared_ = declared();

            for (auto line : i.second) {
                declared_.add_decl(call_stack(call_stack_entry(get_file(), line, current_cf)));
            }

            detail.add_df(df(i.first, declared_, initialized(), used()));

            reporter_->report_json(detail);
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
    std::vector<luna_string *> get_block_values(block* block_) {

        std::vector<luna_string* > all_values = std::vector<luna_string* >();

        if (block_->opt_dfdecls_->dfdecls_ != nullptr) {
            auto names = block_->opt_dfdecls_->dfdecls_->name_seq_->names_;
            all_values.insert(all_values.end(), names->begin(), names->end());
        } 

        for (auto i : *(block_->statement_seq_->statements_)) {
            cf_statement* cur_cf = dynamic_cast<cf_statement*> (i);
            if (cur_cf == nullptr) continue;
            if (cur_cf->opt_label_->id_ == nullptr) continue;

            simple_id* id = dynamic_cast<simple_id *> (cur_cf->opt_label_->id_);
            if (id != nullptr) {
                all_values.push_back(id->value_);           
            }
        }

        return all_values;
    }

    std::map<std::string, std::set<uint>> find_redecls(std::vector<luna_string* > values) {
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
};
