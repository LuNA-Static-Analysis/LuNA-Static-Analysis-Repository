#include <vector>
#include <map>
#include <string>
#include <set>

#include "../parser/ast.hpp"
#include "error_reporter.hpp"
#include "base_analyzer.hpp"

class shadow_import_analyzer : public base_analyzer {
public:
    shadow_import_analyzer(ast* ast_, FILE* yyin, error_reporter* reporter)  {
        this->ast_ = ast_;
        this->file_ = yyin;
        this->reporter_ = reporter;
    }

    bool analyze() override {
        return analyze_shadow_import();
    }

    bool analyze_shadow_import() {
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

            reporter_->report(ERROR_LEVEL::WARNING,
                "LuNA alias \"" + i.first + "\" multiple declarations",
                prev_decls,
                0 
            );
            has_errors = true;
        }
        return has_errors;
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