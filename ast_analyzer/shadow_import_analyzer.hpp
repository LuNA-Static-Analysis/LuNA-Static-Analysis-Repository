#include "../parser/ast.hpp"
#include "base_analyzer.hpp"
#include "ast_json_reporter.cpp"

class shadow_import_analyzer : public base_analyzer {
public:
    shadow_import_analyzer(ast* ast_, FILE* yyin, AstErrorReporter::ErrorReporter* reporter, std::string luna_source)  {
        this->ast_ = ast_;
        this->file_ = yyin;
        this->reporter_ = reporter;
        this->real_luna_source_ = luna_source;
    }

    bool analyze() override {
        return analyze_shadow_import();
    }

    std::string get_name() override {
        return "shadow_import_analyzer";
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

            std::vector<AstErrorReporter::CF> cfs;
            for (auto line : i.second) {
                AstErrorReporter::CF cf{i.first, "extern", get_file(), line};
                cfs.push_back(cf);
            }
            reporter_->addSYN6_1(cfs);
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