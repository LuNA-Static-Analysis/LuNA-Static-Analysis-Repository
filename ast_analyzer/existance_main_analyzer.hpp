#include "base_analyzer.hpp"

#include "../parser/ast.hpp"
#include "error_reporter.hpp"
#include "../error_message/error_entry.hpp"

class existance_main_analyzer : public base_analyzer {
public:
    existance_main_analyzer(ast* ast_, FILE* yyin, error_reporter* reporter, std::string luna_source)  {
        this->ast_ = ast_;
        this->file_ = yyin;
        this->reporter_ = reporter;
        this->real_luna_source_ = luna_source;
    }

    bool analyze() override {
        return analyze_existance_main_cf();
    }

    std::string get_name() override {
        return "existance_main_analyzer";
    }

    bool analyze_existance_main_cf() {
        std::vector<sub_def *> sub_defs = *(ast_->get_program()->sub_defs);

        bool has_main = false;

        for (auto i : sub_defs) {
            if (i == nullptr) continue;

            luna_sub_def* luna_func = dynamic_cast<luna_sub_def *> (i); 
            if (luna_func == nullptr) continue;

            if (*(luna_func->code_id_->value_) == std::string("main")) {
                has_main = true;
                break;
            }
        }

        if (has_main) return false;

        reporter_->report_json(details("SYN7"));

        return true;
    }
};
