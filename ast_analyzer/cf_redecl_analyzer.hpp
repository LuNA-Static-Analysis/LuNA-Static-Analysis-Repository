#include "base_analyzer.hpp"

class cf_redecl_analyzer : public base_analyzer {
public:
    cf_redecl_analyzer(ast* ast_, FILE* yyin, error_reporter* reporter)  {
        this->ast_ = ast_;
        this->file_ = yyin;
        this->reporter_ = reporter;
    }

    bool analyze() override {
        return analyze_cf_redeclaration();
    }

    std::string get_name() override {
        return "cf_redecl_analyzer";
    }

    bool analyze_cf_redeclaration() {
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
            details detail = details();
            detail.add_cf(cf(i->code_id_->to_string(), "struct", get_file(), i->line_));
            reporter_->report_json("16", detail);
        }

        return duplicated.size() > 0;
    }

    template <typename T>
        std::vector<T> find_pairs(std::vector<T>* v) {
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
};
