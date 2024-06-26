#ifndef LUNA_AST
#define LUNA_AST

#include <iostream>
#include <vector>
#include <cstring>
#include <sstream>      

enum luna_type {
    LUNA_INT,
    LUNA_REAL,
    LUNA_STRING,
    LUNA_NAME,
    LUNA_VALUE,
    LUNA_UNDEFINED,
    LUNA_ERROR_TYPE,
};

class block;

class virtual_token {
public:
    unsigned int line_;

    void set_position(uint line) {
        line_ = line;
    }

    virtual std::string to_string() const { return std::string("this class has no to_string method");}
    virtual std::string to_json(const uint shift) const { return std::string("this class has no to_json method");}
    virtual ~virtual_token() {}
};

class expr : public virtual_token {};

class luna_string : public expr {
public:
    std::string* value_;
   
    luna_string(std::string* value, uint line = 0) : value_(value) {
        line_ = line;
    } 

    luna_string() : value_(new std::string()) {}

    ~luna_string() {
        // std::cerr << "luna string dtor\n";
        // no delete because tokens_ will be free explicitly. See ~ast()
    }

    std::string* get_value() {
        return value_;
    }

    std::string to_string() const override {
        return *value_;
    }

    std::string to_json(const uint shift) const override {
        return "\"value\" : \"" + (*value_) + "\"";
    }


    bool operator==(const luna_string& other) const {
        return *(this->value_) == *(other.value_);
    }

    bool operator<(const luna_string& other) const {
        return *(this->value_) < *(other.value_);
    }

    bool operator>(const luna_string& other) const {
        return *(this->value_) > *(other.value_);
    }

};
    
class id : public expr {};

class param : public virtual_token {
    public:    
        luna_string *type_;
        luna_string* name_;

        param(luna_string* type, luna_string* name) : type_(type), name_(name) {}

        ~param() {
            delete type_;
            delete name_;

            type_ = nullptr;
            name_ = nullptr;
        }

        luna_type get_type() {
            if (type_ == nullptr) return luna_type::LUNA_ERROR_TYPE;
            if (type_->to_string() == std::string("int")) return luna_type::LUNA_INT;
            if (type_->to_string() == std::string("real")) return luna_type::LUNA_REAL;
            if (type_->to_string() == std::string("string")) return luna_type::LUNA_STRING;
            if (type_->to_string() == std::string("value")) return luna_type::LUNA_VALUE;
            if (type_->to_string() == std::string("name")) return luna_type::LUNA_NAME;
            return luna_type::LUNA_ERROR_TYPE;
        }

        std::string to_string() const override {
            return type_ == nullptr ? "nullptr" : type_->to_string() + " " + (name_ == nullptr ? "nullptr" : name_->to_string());
        }

        std::string to_json(const uint shift) const override {
            std::stringstream s;
            s << "\"param\" :{" << "\"type\" : {" << (type_ == nullptr ? "" : type_->to_json(shift + 1)) << "}, "
            << "\"name\": {" <<  (name_ == nullptr ? "" : name_->to_json(shift + 1))  << "}}";
            return s.str();
        }
};

class param_seq : public virtual_token {
    public:    
        std::vector<param *>* params_;
        param_seq(std::vector<param *>* params) : params_(params) {}

        param_seq() : params_(new std::vector<param *>()) {}

        ~param_seq() {
            for (auto i : *params_) {
                delete i;
            }
            delete params_;
        }
        
        std::string to_string() const override {
            std::string s;
            for (auto i : *params_) {
                s += (i == nullptr ? "nullptr" : i->to_string()) + ", ";
            }
            // s.pop_back();
            // s.pop_back();
            return s;
        }

        std::string to_json(const uint shift) const override {
            std::stringstream s;

            size_t len = params_->size();

            s << "\"params_seq\" : [";

            for (int j = 0; j < len; j++){
                auto i = params_->at(j);
                s << "{" << i->to_json(shift + 1) << (j == len - 1 ? "}" : "},");
            }
            
            s << "]";

            return s.str();
        }
};

class opt_params : public virtual_token {
    public:
        param_seq *param_seq_;
        opt_params(param_seq *param_seq) : param_seq_(param_seq) {}
        opt_params() : param_seq_(new param_seq()) {}

        ~opt_params() {
            delete param_seq_;
        }

        std::string to_string() const override {
            if (param_seq_ == nullptr) return "";
            return param_seq_->to_string();
        }

        std::string to_json(const uint shift) const override {
            std::stringstream s;
            std::string rep = std::string(shift, ' ');

            s << rep << "\"opt_params\": {\n" << (param_seq_ == nullptr ? "" : param_seq_->to_json(shift + 1));

            s << "\n}";

            return s.str();
        }
};

class name_seq : public virtual_token {
    public:
        std::vector<luna_string *> * names_;
        name_seq(std::vector<luna_string *> * names) : names_(names) {}
        name_seq() : names_(new std::vector<luna_string *>()) {}

        ~name_seq() {
            for (auto i : *names_) {
                delete i;
            }
            delete names_;
        }

        std::string to_string() const override {
            std::string s;
            for (auto i : *names_) {
                s += (i == nullptr ? "nullptr" : i->to_string()) + ", ";
            }
            // s.pop_back();
            // s.pop_back();
            return s;
        }

        std::string to_json(const uint shift) const override {
            std::stringstream s;
            std::string rep = std::string(shift, ' ');

            size_t len = names_->size();

            s << "\"name_seq\" : [";

            for (int j = 0; j < len; j++){
                auto i = names_->at(j);
                s << "{\"name\": {" << i->to_json(shift + 1) << "}" << (j == len - 1 ? "}" : "},");
            }
            
            s << "]";

            return s.str();
        }
};

class dfdecls : public virtual_token {
    public:
        name_seq *name_seq_;
        dfdecls(name_seq *name_seq) : name_seq_(name_seq) {}

        dfdecls() : name_seq_(new name_seq()) {}

        ~dfdecls() {
            // // std::cerr << "df decls dtor\n";
            delete name_seq_;
        }

        std::string to_string() const override {
            return "df " + (name_seq_ == nullptr ? "nullptr" : name_seq_->to_string()) + ";" ;
        }

        std::string to_json(const uint shift) const override {
            std::stringstream s;
            std::string rep = std::string(shift, ' ');

            s << rep << "\"name_seq\" : {\n" << ((name_seq_ == nullptr) ? "" : name_seq_->to_json(shift + 1)) << "\n" << rep << "}\n";
            return s.str();
        }
};

class opt_dfdecls : public virtual_token {
    public:
        dfdecls *dfdecls_;
        opt_dfdecls(dfdecls *dfdecls) : dfdecls_(dfdecls) {}
        opt_dfdecls() : dfdecls_(new dfdecls()) {}
        ~opt_dfdecls() {
            delete dfdecls_;
        }

        std::string to_string() const override {
            if (dfdecls_ == nullptr) return "";
            return dfdecls_->to_string();
        }

        std::string to_json(const uint shift) const override {
            std::stringstream s;
            std::string rep = std::string(shift, ' ');

            s << rep << "\"dfdecls\" : {\n" << ((dfdecls_ == nullptr) ? "" : dfdecls_->to_json(shift + 1)) << "\n" << rep << "}\n";
            return s.str();
        }
};

class statement : public virtual_token {
    public:
        block* block_;
};

class statement_seq : public virtual_token {
    public:
        std::vector<statement *>* statements_;
        statement_seq(std::vector<statement *>* statements) : statements_(statements) {}
        statement_seq() : statements_(new std::vector<statement *>()) {}

        ~statement_seq() {
            for (auto i : *statements_) {
                delete i;
            }
            delete statements_;
        }

        std::string to_string() const override {
            std::string s;
            for (auto i : *statements_) {
                s += (i == nullptr ? "nullptr" : i->to_string()) + ";\n";
            }
            return s;
        }

        std::string to_json(const uint shift) const override {
            std::stringstream s;
            std::string rep = std::string(shift, ' ');

            s << rep << "\"statement_seq\" : [\n";

            size_t len = statements_->size();

            for (int j = 0; j < len; j++) {
                auto i = statements_->at(j);
                s << rep << "{\n" << (i == nullptr ? "" : i->to_json(shift + 1)) << rep << "}" << (j == len - 1 ? "" : ",") << "\n";
            }

            s << rep << "]\n";

            return s.str();
        }
};

class behv_pragma : public virtual_token {};

class behv_pragmas_seq : public virtual_token {
    public:
        std::vector<behv_pragma*>* behv_pragma_;
        behv_pragmas_seq(std::vector<behv_pragma*>* behv_pragma) : behv_pragma_(behv_pragma) {}
        behv_pragmas_seq() : behv_pragma_(new std::vector<behv_pragma*>()) {}

        ~behv_pragmas_seq() {
            for (auto i : *behv_pragma_) {
                delete i;
            }
            delete behv_pragma_;
        }

        std::string to_string() const override {
            std::string s;
            for (auto i : *behv_pragma_) {
                s += (i == nullptr ? "nullptr" : i->to_string()) + ", ";
            }
            s.pop_back();
            s.pop_back();
            return s;
        }

    std::string to_json(const uint shift) const override {
        std::stringstream s;
        std::string rep = std::string(shift, ' ');


        s << rep << "\"behv_pragmas_seq\" : [\n";

        size_t len = behv_pragma_->size();

        for (int j = 0; j < len; j++) {
            auto i = behv_pragma_->at(j);
            s << rep << "{\n" << (i == nullptr ? "" : i->to_json(shift + 1)) << rep << "}" << (j == len - 1 ? "" : ",") << "\n";
        }

        s << rep << "]\n";

        return s.str();
    }
};

class opt_behavior : public virtual_token {
    public:
        behv_pragmas_seq *seq_;
        opt_behavior(behv_pragmas_seq *seq) : seq_(seq) {}
        opt_behavior() : seq_(new behv_pragmas_seq()) {}

        ~opt_behavior() {
            delete seq_;
        }
        
        std::string to_string() const override {
            if (seq_ == nullptr) return "";
            return seq_->to_string();
        }

    std::string to_json(const uint shift) const override {
        std::stringstream s;
        std::string rep = std::string(shift, ' ');

        s << rep << "\"opt_behavior\" : {\n" << (seq_ == nullptr ? "" : seq_->to_json(shift + 1)) << rep << "}\n";

        return s.str();
    }
};

class block : public virtual_token {
    public:
        opt_dfdecls* opt_dfdecls_;
        statement_seq* statement_seq_;
        opt_behavior* opt_behavior_;

        block(opt_dfdecls* opt_dfdecls,
                statement_seq* statement_seq,
                opt_behavior* opt_behavior)

            : opt_dfdecls_(opt_dfdecls), statement_seq_(statement_seq), opt_behavior_(opt_behavior) {}
        
        block() : opt_dfdecls_(new opt_dfdecls()),
                 statement_seq_(new statement_seq()),
                 opt_behavior_(new opt_behavior()) {}

        ~block() {
            delete opt_dfdecls_;
            delete statement_seq_;
            delete opt_behavior_;
        }

        std::string to_string() const override {
            return 
                (opt_dfdecls_ == nullptr ? "nullptr" : opt_dfdecls_->to_string()) + "\n" +
                (statement_seq_ == nullptr ? "nullptr" : statement_seq_->to_string()) + 
                (opt_behavior_ == nullptr ? "nullptr" : opt_behavior_->to_string()) +"\n";
        }

        std::string to_json(const uint shift) const override {
            std::stringstream s;
            std::string rep = std::string(shift, ' ');

            s << rep << "\"block\" : {\n";

            s << rep << (opt_dfdecls_ == nullptr ? "" : opt_dfdecls_->to_json(shift + 1)) <<  ",\n"
            << rep << (statement_seq_ == nullptr ? "" : statement_seq_->to_json(shift + 1)) << ",\n"
            << rep << (opt_behavior_ == nullptr ? "" : opt_behavior_->to_json(shift + 1)) << "\n";

            s << rep << "}\n";

            return s.str();
        }
};

class complex_id : public id {
    public:
        id *id_;
        expr *expr_;

        complex_id(id *id, expr *expr) : id_(id), expr_(expr) {} 

        complex_id() : id_(new id()), expr_(new expr()) {}

        ~complex_id() {
            delete id_;
            delete expr_;
        }

        std::string to_string() const override {
            return (id_ == nullptr ? "nullptr" : id_->to_string()) 
                + "[" + (expr_ == nullptr ? "nullptr" : expr_->to_string()) + "]";
        }

    std::string to_json(const uint shift) const override {
        std::stringstream s;
        std::string rep = std::string(shift, ' ');

        s << "\"complex_id\" : {\n" << "\"id\" : {" << id_->to_json(shift + 1) << "}" <<  ",\n" << "\"expr\" : {" << expr_->to_json(shift + 1) << "}" << "\n}\n";

        return s.str();
    }
};

class integer : public expr {
public:
    int* value_;
    integer(int* value) : value_(value) {}
    ~integer() {
        delete value_;
        // // // std::cerr << "integer dtor\n";
    }

    std::string to_string() const override {
        if (value_ == nullptr) return "nullptr";
        return std::to_string(*value_);
    }

    std::string to_json(const uint shift) const override {
        std::stringstream s;
        std::string rep = std::string(shift, ' ');

        s << "\"integer\" : "  << std::to_string(*value_);

        return s.str();
    }

};

class real : public expr {
    public:
        double* value_;
        real(double* value) : value_(value) {} 
        ~real() {
            delete value_;
        }

        std::string to_string() const override {
            if (value_ == nullptr) return "nullptr";
            return std::to_string(*value_);
        }

    std::string to_json(const uint shift) const override {
        std::stringstream s;
        std::string rep = std::string(shift, ' ');

        s << "\"real\" : "  << std::to_string(*value_);

        return s.str();
    }


};

class luna_cast : public expr {
public:
    expr *expr_;
    luna_cast(expr *expr) : expr_(expr) {}
    ~luna_cast() {
        delete expr_;
    }

    std::string to_string() const override {
        return expr_ == nullptr ? "nullptr" : expr_->to_string();
    }

    std::string to_json(const uint shift) const override {
        std::stringstream s;
        std::string rep = std::string(shift, ' ');

        return s.str();
    }
};

class to_int : public luna_cast {
public:
    to_int(expr* expr) : luna_cast(expr) {} 

    std::string to_json(const uint shift) const override {
        std::stringstream s;
        std::string rep = std::string(shift, ' ');

        s << "\"to_int\" : {\n"  << expr_->to_json(shift + 1) << "\n}\n";

        return s.str();
    }
};

class to_real : public luna_cast {
public:
    to_real(expr* expr) : luna_cast(expr) {} 

    std::string to_json(const uint shift) const override {
        std::stringstream s;
        std::string rep = std::string(shift, ' ');

        s << "\"to_real\" : {\n"  << expr_->to_json(shift + 1) << "\n}\n";

        return s.str();
    }
};

class to_str: public luna_cast {
public:
    to_str(expr* expr) : luna_cast(expr) {} 

    std::string to_json(const uint shift) const override {
        std::stringstream s;
        std::string rep = std::string(shift, ' ');

        s << "\"to_str\" : {\n"  << expr_->to_json(shift + 1) << "\n}\n";

        return s.str();
    }
};

class code_df_param : public virtual_token {
    public:
        luna_string *type_;
        luna_string *code_df_;

        code_df_param(luna_string* type, luna_string* df) : type_(type), code_df_(df) {}

        ~code_df_param() {
            delete type_;
            delete code_df_;
        }

        luna_type get_type() {
            if (type_ == nullptr) return luna_type::LUNA_ERROR_TYPE;
            if (type_->to_string() == std::string("int")) return luna_type::LUNA_INT;
            if (type_->to_string() == std::string("real")) return luna_type::LUNA_REAL;
            if (type_->to_string() == std::string("string")) return luna_type::LUNA_STRING;
            if (type_->to_string() == std::string("value")) return luna_type::LUNA_VALUE;
            if (type_->to_string() == std::string("name")) return luna_type::LUNA_NAME;
            return luna_type::LUNA_ERROR_TYPE;
        }

        std::string to_string() const override {
            return (type_ == nullptr ? "nullptr" : type_->to_string()) + " " + ((code_df_ == nullptr) ? "" : code_df_->to_string());
        }

        std::string to_json(const uint shift) const override {
            std::stringstream s;
            std::string rep = std::string(shift, ' ');
            s << 
                rep << "\"type\" : {" << (type_ == nullptr ? "" : type_->to_json(shift + 1)) << "}, " << "\n" <<
                rep << "\"code_df\" :  {" << ((code_df_ == nullptr) ? "" : code_df_->to_json(shift + 1)) << "}";
            return s.str();
        }
};

class ext_params_seq : public virtual_token {
public:
    std::vector<code_df_param *>* params_;
    ext_params_seq(std::vector<code_df_param *>* params) : params_(params) {}
    ext_params_seq() : params_(new std::vector<code_df_param *>()) {}

    ~ext_params_seq() {
        for (auto param : *params_) {
            delete param;
        }
        delete params_;
    }

    std::string to_string() const override {
        std::string s;

        for (auto i : *params_) {
            s += (i == nullptr ? "nullptr" :i->to_string()) + ", ";
        }
        s.pop_back();
        s.pop_back();
        return s;
    }

    std::string to_json(const uint shift) const override {
        std::stringstream s;
        std::string rep = std::string(shift, ' ');

        s << rep << "\"ext_params_seq\" : [\n";

        size_t len = params_->size();

        for (int j = 0; j < len; j++) {
            auto i = params_->at(j);
            s << rep << "{\n" << (i == nullptr ? "" : i->to_json(shift + 1)) << rep << "}" << (j == len - 1 ? "" : ",") << "\n";
        }

        s << rep << "\n]\n";

        return s.str();
    }
};

class opt_ext_params : public virtual_token {
    public:
        ext_params_seq *seq_;
        opt_ext_params(ext_params_seq *seq) : seq_(seq) {}
        opt_ext_params() : seq_(new ext_params_seq()) {}

        ~opt_ext_params() {
            delete seq_;
        }

        std::string to_string() const override {
            return seq_ == nullptr ? "nullptr" :seq_->to_string();
        }

        std::string to_json(const uint shift) const override {
            std::stringstream s;
            std::string rep = std::string(shift, ' ');

            s << rep << "\"opt_ext_params\" : {\n" << (seq_ == nullptr ? "" : seq_->to_json(shift + 1)) << rep << "}\n";
            return s.str();
        }
};

class bin_op : public expr {
    public:
        expr *left_;
        expr *right_;
        bin_op(expr *left, expr *right) : left_(left), right_(right) {}

        ~bin_op() {
            delete left_;
            delete right_;
        }
};

class eq : public bin_op {
    public:
        eq(expr *left, expr *right) : bin_op(left, right) {}

        std::string to_string() const override {
            return (left_ == nullptr ? "nullptr" : left_->to_string()) + " = " + (right_ == nullptr ? "nullptr" : right_->to_string());
        }

    std::string to_json(const uint shift) const override {
        std::stringstream s;
        std::string rep = std::string(shift, ' ');

        s << rep << "\"eq\" : {\n" <<  "\"left\" : " << left_->to_json(shift + 1) << rep << ",\n" << "\"right\" :{" << right_->to_json(shift + 1) << "\n}\n";

        return s.str();
    }
};

// class eqg : public bin_op {
//     public:
//         eqg(expr *left, expr *right) : bin_op(left, right) {}

//         std::string to_string() const override {
//             return left_->to_string() + " >= " + right_->to_string();
//         }
// };

class sum : public bin_op {
    public:
        sum(expr *left, expr *right) : bin_op(left, right) {}

        std::string to_string() const override {
            return (left_ == nullptr ? "nullptr" : left_->to_string()) + " + " + (right_ == nullptr ? "nullptr" : right_->to_string());
        }

    std::string to_json(const uint shift) const override {
        std::stringstream s;
        std::string rep = std::string(shift, ' ');

        s << rep << "\"sum\" : {\n" <<  "\"left\" : {" << left_->to_json(shift + 1) << rep << "},\n" <<  "\"right\" : {"<< right_->to_json(shift + 1) << "}\n}\n";

        return s.str();
    }
};

class sub : public bin_op {
    public:
        sub(expr *left, expr *right) : bin_op(left, right) {}

        std::string to_string() const override {
            return (left_ == nullptr ? "nullptr" : left_->to_string()) + " - " + (right_ == nullptr ? "nullptr" : right_->to_string());
        }

    std::string to_json(const uint shift) const override {
        std::stringstream s;
        std::string rep = std::string(shift, ' ');

        s << rep << "\"sub\" : {\n" <<  "\"left\" : {" << left_->to_json(shift + 1) << rep << "},\n" <<  "\"right\" : {"<< right_->to_json(shift + 1) << "}\n}\n";
        return s.str();
    }
};

class div1 : public bin_op {
    public:
        div1(expr *left, expr *right) : bin_op(left, right) {}

        std::string to_string() const override {
            return (left_ == nullptr ? "nullptr" : left_->to_string()) + " / " + (right_ == nullptr ? "nullptr" : right_->to_string());
        }

    std::string to_json(const uint shift) const override {
        std::stringstream s;
        std::string rep = std::string(shift, ' ');

        s << rep << "\"div\" : {\n" <<  "\"left\" : {" << left_->to_json(shift + 1) << rep << "},\n" <<  "\"right\" : {"<< right_->to_json(shift + 1) << "}\n}\n";
        return s.str();
    }
};

class mul : public bin_op {
    public:
        mul(expr *left, expr *right) : bin_op(left, right) {}

        std::string to_string() const override {
            return (left_ == nullptr ? "nullptr" : left_->to_string()) + " * " + (right_ == nullptr ? "nullptr" : right_->to_string());
        }

    std::string to_json(const uint shift) const override {
        std::stringstream s;
        std::string rep = std::string(shift, ' ');

        s << rep << "\"mul\" : {\n" <<  "\"left\" : {" << left_->to_json(shift + 1) << rep << "},\n" <<  "\"right\" : {"<< right_->to_json(shift + 1) << "}\n}\n";
        return s.str();
    }
};

class mod : public bin_op {
    public:
        mod(expr *left, expr *right) : bin_op(left, right) {}

        std::string to_string() const override {
            return (left_ == nullptr ? "nullptr" : left_->to_string()) + " % " + (right_ == nullptr ? "nullptr" : right_->to_string());
        }

    std::string to_json(const uint shift) const override {
        std::stringstream s;
        std::string rep = std::string(shift, ' ');

        s << rep << "\"mod\" : {\n" <<  "\"left\" : {" << left_->to_json(shift + 1) << rep << "},\n" <<  "\"right\" : {"<< right_->to_json(shift + 1) << "}\n}\n";

        return s.str();
    }
};

class lt : public bin_op {
    public:
        lt(expr *left, expr *right) : bin_op(left, right) {}

        std::string to_string() const override {
            return (left_ == nullptr ? "nullptr" : left_->to_string()) + " < " + (right_ == nullptr ? "nullptr" : right_->to_string());
        }

    std::string to_json(const uint shift) const override {
        std::stringstream s;
        std::string rep = std::string(shift, ' ');

        s << rep << "\"lt\" : {\n" <<  "\"left\" : {" << left_->to_json(shift + 1) << rep << "},\n" <<  "\"right\" : {"<< right_->to_json(shift + 1) << "}\n}\n";
        return s.str();
    }
};

class gt : public bin_op {
    public:

        gt(expr *left, expr *right) : bin_op(left, right) {}
        std::string to_string() const override {
            return (left_ == nullptr ? "nullptr" : left_->to_string()) + " > " + (right_ == nullptr ? "nullptr" : right_->to_string());
        }

    std::string to_json(const uint shift) const override {
        std::stringstream s;
        std::string rep = std::string(shift, ' ');

        s << rep << "\"gt\" : {\n" <<  "\"left\" : {" << left_->to_json(shift + 1) << rep << "},\n" <<  "\"right\" : {"<< right_->to_json(shift + 1) << "}\n}\n";
        return s.str();
    }
};

class leq : public bin_op {
    public:
        leq(expr *left, expr *right) : bin_op(left, right) {}

        std::string to_string() const override {
            return (left_ == nullptr ? "nullptr" : left_->to_string()) + " <= " + (right_ == nullptr ? "nullptr" : right_->to_string());
        }

    std::string to_json(const uint shift) const override {
        std::stringstream s;
        std::string rep = std::string(shift, ' ');

        s << rep << "\"leq\" : {\n" <<  "\"left\" : {" << left_->to_json(shift + 1) << rep << "},\n" <<  "\"right\" : {"<< right_->to_json(shift + 1) << "}\n}\n";
        return s.str();
    }
};

class geq : public bin_op {
    public:
        geq(expr *left, expr *right) : bin_op(left, right) {}

        std::string to_string() const override {
            return (left_ == nullptr ? "nullptr" : left_->to_string()) + " >= " + (right_ == nullptr ? "nullptr" : right_->to_string());
        }

    std::string to_json(const uint shift) const override {
        std::stringstream s;
        std::string rep = std::string(shift, ' ');

        s << rep << "\"geq\" : {\n" <<  "\"left\" : {" << left_->to_json(shift + 1) << rep << "},\n" <<  "\"right\" : {"<< right_->to_json(shift + 1) << "}\n}\n";
        return s.str();
    }
};

class dbleq : public bin_op {
    public:
        dbleq(expr *left, expr *right) : bin_op(left, right) {}

        std::string to_string() const override {
            return (left_ == nullptr ? "nullptr" : left_->to_string()) + " == " + (right_ == nullptr ? "nullptr" : right_->to_string());
        }

    std::string to_json(const uint shift) const override {
        std::stringstream s;
        std::string rep = std::string(shift, ' ');

        s << rep << "\"dbleq\" : {\n" <<  "\"left\" : {" << left_->to_json(shift + 1) << rep << "},\n" <<  "\"right\" : {" + right_->to_json(shift + 1) << "}\n}\n";
        return s.str();
    }
};

class neq : public bin_op {
    public:
        neq(expr *left, expr *right) : bin_op(left, right) {}
        std::string to_string() const override {
            return (left_ == nullptr ? "nullptr" : left_->to_string()) + " != " + (right_ == nullptr ? "nullptr" : right_->to_string());
        }

    std::string to_json(const uint shift) const override {
        std::stringstream s;
        std::string rep = std::string(shift, ' ');

        s << rep << "\"neq\" : {\n" << "\"left\" : {" << left_->to_json(shift + 1) << rep << "},\n" <<  "\"right\" : {"<< right_->to_json(shift + 1) << "}\n}\n";
        return s.str();
    }
};

class dblamp : public bin_op {
    public:
        dblamp(expr *left, expr *right) : bin_op(left, right) {}

        std::string to_string() const override {
            return (left_ == nullptr ? "nullptr" : left_->to_string()) + " && " + (right_ == nullptr ? "nullptr" : right_->to_string());
        }

    std::string to_json(const uint shift) const override {
        std::stringstream s;
        std::string rep = std::string(shift, ' ');

        s << rep << "\"dblamp\" : {\n" <<  "\"left\" : {" << left_->to_json(shift + 1) << rep << "},\n" <<  "\"right\" : {"<< right_->to_json(shift + 1) << "}\n}\n";
        return s.str();
    }
};

class dblpipe : public bin_op {
    public:
        dblpipe(expr *left, expr *right) : bin_op(left, right) {}

        std::string to_string() const override {
            return (left_ == nullptr ? "nullptr" : left_->to_string()) + " || " + (right_ == nullptr ? "nullptr" : right_->to_string());
        }

    std::string to_json(const uint shift) const override {
        std::stringstream s;
        std::string rep = std::string(shift, ' ');

        s << rep << "\"dblpipe\" : {\n" <<  "\"left\" : {" << left_->to_json(shift + 1) << rep << "},\n" <<  "\"right\" : {"<< right_->to_json(shift + 1) << "}\n}\n";
        return s.str();
    }
};

class sub_def : public virtual_token {
    public:
        ~sub_def() {}
};

class program : public virtual_token {
    public:
        std::vector<sub_def *>* sub_defs;

        program(std::vector<sub_def *>* other) : sub_defs(other) {}

        program() : sub_defs(new std::vector<sub_def *>(0)) {}
        
        ~program() {
            for (auto i : *sub_defs) {
                delete i;
            }
            delete sub_defs;
        }

        std::string to_string() const override {
            std::string res;
            for (auto i : *sub_defs) {
                // if (i == nullptr) continue;
                res += (i == nullptr ? "nullptr" : i->to_string()) + '\n';
            }
            // res.pop_back();
            return res;
        }

        std::string to_json(const uint shift) const override {
            std::stringstream s;
            std::string rep = std::string(shift, ' ');
            s << "{\n" << "\"program\" : [\n";

            size_t len =  sub_defs->size();

            for (int j = 0; j < len; j++) {
                sub_def* i = sub_defs->at(j);
                s << rep << "{\n" << (i == nullptr ? "" : (i->to_json(shift + 1))) << "}" <<  (j == len - 1 ? "" : ",") << "\n";
            }

            std::string str = s.str();

            s << "\n]" << "\n}";

            return s.str();
        }
};

class control_pragma : public virtual_token {
    public:
        luna_string *where_type_;
        std::vector<expr *>* expr_;
        control_pragma(luna_string* other_type, std::vector<expr *>* other_expr) : where_type_(other_type), expr_(other_expr) {}
        control_pragma() : where_type_(new luna_string()), expr_(new std::vector<expr *>()) {}
        ~control_pragma() {
            // // // std::cerr << "control pragma dtor\n";
            delete where_type_;
            delete expr_;
        }

        std::string to_string() const override {
            std::string s;
            s += where_type_->to_string();

            for (auto i : *expr_) {
                // if (i == nullptr) continue;
                s += (i == nullptr ? "nullptr" :i->to_string()) + '\n';
            }
            s.pop_back();
            return s;
        }

    std::string to_json(const uint shift) const override {
        std::stringstream s;
        std::string rep = std::string(shift, ' ');

        s << rep << "\"control_pragma\" : {\n" << 
        rep << "where_type : " << where_type_->to_json(shift + 1);

        s << "{\n" << "\"exprs_\" : [\n";

        for (auto i : *expr_) {
            s << rep;
            s << (i == nullptr ? "\"\"" : ("{\n" + rep + i->to_json(shift + 1)  +  "\n" + rep + "}")) + '\n';
        }

        s << "\n]" << "\n}";

        return s.str();
    }
};

class luna_sub_def : public sub_def {
    public:
        control_pragma *control_pragma_;
        luna_string *code_id_;
        opt_params *params_;
        block *block_;

        luna_sub_def(control_pragma* cp,
                    luna_string* id,
                    opt_params* params,
                    block* block) 
            : control_pragma_(cp), code_id_(id), params_(params), block_(block) {}
        
        luna_sub_def() : control_pragma_(new control_pragma()), 
                        code_id_(new luna_string()),
                        params_(new opt_params()),
                        block_(new block()) {}

        ~luna_sub_def() {
            delete control_pragma_;
            delete code_id_;
            delete params_;
            delete block_;
        }

        std::string* get_value() {
            return code_id_->value_;
        }

        std::string to_string() const override {
            return "sub " + (code_id_ == nullptr ? "nullptr" : code_id_->to_string()) + 
                "(" + (params_ == nullptr ? "nullptr" :params_->to_string()) + ") {\n" +
                (block_ == nullptr ? "nullptr" : block_->to_string()) + "}\n" +
                (control_pragma_ == nullptr ? "" : control_pragma_->to_string()); 
        }

        std::string to_json(const uint shift) const override {
            std::stringstream s;
            std::string rep = std::string(shift, ' ');

            s << "\"sub\" : {\n" << rep 
            << "\"code_id\" : {" << (code_id_ == nullptr ? "" : code_id_->to_json(shift + 1)) << "}"
            << (params_ == nullptr ? "" : ",\n" + params_->to_json(shift + 1)) << rep 
            << (block_ == nullptr ? "" : ",\n" + block_->to_json(shift + 1)) << rep 
            << (control_pragma_ == nullptr ? "" : ",\n" + control_pragma_->to_json(shift + 1)) << rep 
            << "}\n";

            return s.str();
        }
};

class import : public sub_def {
    public:
        luna_string *cxx_code_id_;
        opt_ext_params *params_;
        luna_string *luna_code_id_;
        std::string options_;

        import(luna_string* cxx_id, opt_ext_params* params, luna_string* luna_id, const std::string options) 
            : cxx_code_id_(cxx_id), params_(params), luna_code_id_(luna_id), options_(options) 
            {}

        import() : cxx_code_id_(new luna_string()),
                        params_(new opt_ext_params()),
                        luna_code_id_(new luna_string()) {}

        ~import() {
            delete cxx_code_id_;
            delete params_;
            delete luna_code_id_;
        }

        std::string to_string() const override {
            return "import " + (cxx_code_id_ == nullptr ? "nullptr" : cxx_code_id_->to_string())
            + "(" + (params_ == nullptr ? "nullptr" : params_->to_string())
            + ") as " + (luna_code_id_ == nullptr ? "nullptr" : luna_code_id_->to_string()) + ": "
            + options_ + ";";
        }

        std::string to_json(const uint shift) const override {
            std::stringstream s;
            std::string rep = std::string(shift, ' ');

            s << "\"import\" : {\n" << 
                rep << "\"cxx_code_id\" : {"  <<  (cxx_code_id_ == nullptr ? "" : cxx_code_id_->to_json(shift + 1)) << "}, " << "\n" <<
                rep << (params_ == nullptr ? "" : params_->to_json(shift + 1)) << "," <<
                rep << "\"luna_code_id\" : { " << (luna_code_id_ == nullptr ? "" : luna_code_id_->to_json(shift + 1)) << "}, " << "\n" <<
                rep << "\"options\" : " << (options_ == "" ? "\"\"" : options_) << "\n}";
            return s.str();
        }

};

class cxx_block_with_params_def : public sub_def {
    public:
        luna_string *code_id_;
        opt_params *params_;

        cxx_block_with_params_def(luna_string* id, opt_params* params) 
            : code_id_(id), params_(params) {}
        
        ~cxx_block_with_params_def() {
            delete code_id_;
            delete params_;
        }

    std::string to_json(const uint shift) const override {
        std::stringstream s;
        std::string rep = std::string(shift, ' ');

        s << rep << "\"cxx_block_with_params_def\" : {\n" 
        << rep << "\"code_id\" : " << code_id_->to_json(shift + 1) << ",\n"
        << params_->to_json(shift + 1) << "\n}\n";

        return s.str();
    }
};

class cxx_block_def : public sub_def {
    public:
        luna_string *name_;
        cxx_block_def(luna_string* id) : name_(id) {}

        ~cxx_block_def() {
            delete name_;
        }

    std::string to_json(const uint shift) const override {
        std::stringstream s;
        std::string rep = std::string(shift, ' ');
        s << rep << "\"cxx_block_def\" : {\n" 
        << rep << "\"name\" : " << name_->to_json(shift + 1) << "\n}\n";

        return s.str();
    }
};

class if_statement : public statement {
    public:
        expr *expr_;
        if_statement(expr *expr, block *block) :  expr_(expr) {
            block_ = block;
        }

        ~if_statement() {
            delete expr_;
            delete block_;
        }

        std::string to_string() const override {
            return "if " + (expr_ == nullptr ? "nullptr" :expr_->to_string()) + "{\n" +
                (block_ == nullptr ? "nullptr" : block_->to_string()) + "}";
        }

        std::string to_json(const uint shift) const override {
            std::stringstream s;
            std::string rep = std::string(shift, ' ');

            s << rep << "\"if_statement\" : {\n" 
            << rep << (expr_ == nullptr ? "" : expr_->to_json(shift + 1))
            << rep << (block_ == nullptr ? "" : ",\n" + block_->to_json(shift + 1)) << "\n" << rep << "}";

            return s.str();
        }
};

class for_statement : public statement {
    public:
        control_pragma *control_pragma_;
        luna_string *name_;
        expr *expr_1_;
        expr *expr_2_;
        // block *block_;
        for_statement(control_pragma *control_pragma,
                luna_string *name,
                expr *expr_1,
                expr *expr_2,
                block *block)
            : control_pragma_(control_pragma), name_(name), expr_1_(expr_1), expr_2_(expr_2) {
                block_ = block;
            }
        
        ~for_statement() {
            delete control_pragma_;
            delete name_;
            delete expr_1_;
            delete expr_2_;
            delete block_;
        }

        std::string to_string() const override {
            return 
                "for " +
                (name_ == nullptr ? "nullptr" :name_->to_string()) + 
                "=" +
                (expr_1_ == nullptr ? "nullptr" :expr_1_->to_string()) + ".." + (expr_2_ == nullptr ? "nullptr" :expr_2_->to_string()) + "{\n" +
                (block_ == nullptr ? "nullptr" :block_->to_string()) + "}\n" +
                (control_pragma_ == nullptr ? "" : control_pragma_->to_string());
        }

    std::string to_json(const uint shift) const override {
        std::stringstream s;
        std::string rep = std::string(shift, ' ');

        s << "\"for_statement\" : {\n"
        << rep << "\"name\" : {"  << (name_ == nullptr ? "" :name_->to_json(shift + 1)) << "}\n"
        << (expr_1_ == nullptr ? "" : ", \"left\" : {" + expr_1_->to_json(shift + 1)) << "}" 
        << (expr_2_ == nullptr ? "" : ", \"right\" :{" + expr_2_->to_json(shift + 1)) << "}"
        << (block_ == nullptr ? "" : ",\n" + block_->to_json(shift + 1)) 
        << (control_pragma_ == nullptr ? "" : ",\n" + control_pragma_->to_json(shift + 1))
        << "}"
        ;

        return s.str();
    }
};

class assign : public virtual_token {
    public:
        luna_string *name_;
        expr *expr_;
        assign(luna_string *name, expr *expr) : name_(name), expr_(expr) {}

        ~assign() {
            delete name_;
            delete expr_;
        }

        std::string to_string() const override {
            return (name_ == nullptr ? "" :name_->to_string()) + "=" + (expr_ == nullptr ? "" :expr_->to_string());
        }

    std::string to_json(const uint shift) const override {
        std::stringstream s;
        std::string rep = std::string(shift, ' ');

        s << rep << "\"assign\" : {\n" << "\"name\" : {" << name_->to_json(shift + 1) << "}," << "\"expr\" : {" << expr_->to_json(shift + 1) << "}" << "\n}\n";

        return s.str();
    }
};

class assign_seq : public virtual_token {
    public:
        std::vector<assign* >* assign_seq_;
        assign_seq(std::vector<assign* >* assign_seq) : assign_seq_(assign_seq) {}
        assign_seq() : assign_seq_(new std::vector<assign* >()) {}

        ~assign_seq() {
            for (auto i : *assign_seq_) {
                delete i;
            }
            delete assign_seq_;
        }

        std::string to_string() const override {
            std::string s;
            for (auto i : *assign_seq_) {
                s += (i == nullptr ? "nullptr" :i->to_string()) + ',';
            }
            s.pop_back();
            return s;
        }

    std::string to_json(const uint shift) const override {
        std::stringstream s;
        std::string rep = std::string(shift, ' ');

        s << "\"assign_seq\" : [\n";

        size_t len = assign_seq_->size();

        for (int j = 0; j < len; j++) {
            auto i = assign_seq_->at(j);
            s << rep << "{\n" << (i == nullptr ? "" : (i->to_json(shift + 1))) << "}" << (j == len - 1 ? "" : ",");
        }

        s << "\n]";

        return s.str();
    }
};

class let_statement : public statement {
    public:
        assign_seq *assign_seq_;
        // block *block_;
        let_statement(assign_seq *assign_seq_, block *block) : assign_seq_(assign_seq_) {
            block_ = block;
        }

        ~let_statement() {
            delete assign_seq_;
            delete block_;
        }
        
        std::string to_string() const override {
            return "let" + (assign_seq_ == nullptr ? "nullptr" : assign_seq_->to_string()) + "{\n"+
            (block_ == nullptr ? "nullptr" : block_->to_string()) + "}";
        }

    std::string to_json(const uint shift) const override {
        std::stringstream s;
        std::string rep = std::string(shift, ' ');

        s << rep << "\"let_statement\" : {\n" << assign_seq_->to_json(shift + 1) << "\n}\n";

        return s.str();
    }
};

class behv_pragma_seq : public behv_pragma {
    public:
        name_seq* name_seq_;
        behv_pragma_seq(name_seq* name_seq) : name_seq_(name_seq) {}
        ~behv_pragma_seq() {
            delete name_seq_;
        }

    std::string to_json(const uint shift) const override {
        std::stringstream s;
        std::string rep = std::string(shift, ' ');

        s << rep << "\"behv_pragma_seq\" : {\n" << (name_seq_ == nullptr ? "" : name_seq_->to_json(shift + 1)) << "\n}\n";

        return s.str();
    }
};

class while_statement : public statement {
    public:
        control_pragma *control_pragma_;
        luna_string *left_;
        expr *expr_;
        expr *right_;
        // block *block_;
        id *id_;

        while_statement(control_pragma *control_pragma,
                expr *expr_,
                luna_string *left,
                expr *right,
                id* id,
                block *block) 
            :  expr_(expr_), left_(left), right_(right), id_(id), control_pragma_(control_pragma) {
                block_ = block;
            }

        ~while_statement() {
            delete control_pragma_;
            delete expr_;
            delete left_;
            delete right_;
            delete id_;
            delete block_;
        }

        std::string to_string() const override {
            return 
                "while (" +
                (control_pragma_ == nullptr ? "" : control_pragma_->to_string()) +
                (left_ == nullptr ? "nullptr" :left_->to_string()) +
                (expr_ == nullptr ? "nullptr" :expr_->to_string()) +
                (right_ == nullptr ? "nullptr" :right_->to_string()) +
                ")" +
                (block_ == nullptr ? "nullptr" :block_->to_string()) +
                (id_ == nullptr ? "nullptr" :id_->to_string());
        }

        std::string to_json(const uint shift) const override {
            std::stringstream s;
            std::string rep = std::string(shift, ' ');

            s << "\"while_statement\" : {\n"
            << rep << "\"id\" : {"  << (id_ == nullptr ? "" : id_->to_json(shift + 1)) << "}\n"
            << (left_ == nullptr ? "" : ", \"left\" : {" + left_->to_json(shift + 1)) << "}" 
            << (right_ == nullptr ? "" : ", \"right\" : {" + right_->to_json(shift + 1)) << "}"
            << (expr_ == nullptr ? "" : ", \"expr\" : {" + expr_->to_json(shift + 1)) << "}"
            << (block_ == nullptr ? "" : ",\n" + block_->to_json(shift + 1)) 
            << (control_pragma_ == nullptr ? "" : ",\n" + control_pragma_->to_json(shift + 1))
            << "}"
            ;

            return s.str();
        }
};

class exprs_seq : public virtual_token {
    public:
        std::vector<expr* >* expr_;
        exprs_seq(std::vector<expr* >* expr) : expr_(expr) {}
        exprs_seq() : expr_(new std::vector<expr* >()) {}

        ~exprs_seq() {
            for (auto i : *expr_) {
                delete i;
            }
            delete expr_;
        }
        
        std::string to_string() const override {
            std::string s;
            for (auto i : *expr_) {
                // if (i == nullptr) continue;
                s += (i == nullptr ? "nullptr" : i->to_string()) + ',';
            }
            s.pop_back();
            return s;
        }

    std::string to_json(const uint shift) const override {
        std::stringstream s;
        std::string rep = std::string(shift, ' ');

        s << "\"exprs_seq\" : [\n";

        size_t len = expr_->size();

        for (int j = 0; j < len; j++) {
            auto i = expr_->at(j);
            s << rep << "{\n" << (i == nullptr ? "" : (i->to_json(shift + 1))) << "}" << (j == len - 1 ? "" : ",");
        }

        s << "\n]";

        return s.str();
    }
};

class opt_exprs : public virtual_token {
public:
    exprs_seq *exprs_seq_;
    opt_exprs(exprs_seq *exprs_seq) : exprs_seq_(exprs_seq) {}

    ~opt_exprs() { 
        delete exprs_seq_;
    }

    std::string to_string() const override {
        if (exprs_seq_ == nullptr) return "";
        return exprs_seq_->to_string();
    }

    std::string to_json(const uint shift) const override {
        std::stringstream s;
        std::string rep = std::string(shift, ' ');

        s << rep << "\"opt_exprs\" : {\n" << (exprs_seq_ == nullptr ? "" : exprs_seq_->to_json(shift + 1)) << rep << "}\n";

        return s.str();
    }
};

class opt_setdf_rules : public virtual_token {
public:
    opt_exprs *opt_exprs_;
    opt_setdf_rules(opt_exprs *opt_exprs) : opt_exprs_(opt_exprs) {}
    ~opt_setdf_rules() {
        delete opt_exprs_;
    }

    std::string to_string() const override {
        if (opt_exprs_ == nullptr) return "";
        return opt_exprs_->to_string();
    }

    std::string to_json(const uint shift) const override {
        std::stringstream s;
        std::string rep = std::string(shift, ' ');

        s << rep << "\"opt_setdf_rules\" : {\n" << (opt_exprs_ == nullptr ? "" : opt_exprs_->to_json(shift + 1)) << rep << "}\n";

        return s.str();
    }
};

class opt_label : public virtual_token {
public:
    id *id_;
    opt_label(id *id) : id_(id) {}

    ~opt_label() {
        delete id_;
    }
    std::string to_string() const override {
        if (id_ == nullptr) return "";
        return "cf " + id_->to_string() + ":";
    }

    std::string to_json(const uint shift) const override {
        std::stringstream s;
        std::string rep = std::string(shift, ' ');

        s << rep << "\"opt_label\" : {\n" << (id_ == nullptr ? "" : id_->to_json(shift + 1)) << rep << "}\n";

        return s.str();
    }
};

class opt_rules : public virtual_token {
public:
    opt_exprs *opt_exprs_;
    opt_rules(opt_exprs *opt_exprs) : opt_exprs_(opt_exprs) {}

    ~opt_rules() {
        delete opt_exprs_;
    }

    std::string to_string() const override {
        if (opt_exprs_ == nullptr) return "";
        return opt_exprs_->to_string();
    }

    std::string to_json(const uint shift) const override {
        std::stringstream s;
        std::string rep = std::string(shift, ' ');

        s << rep << "\"opt_rules\" : {\n" << (opt_exprs_ == nullptr ? "" : opt_exprs_->to_json(shift + 1)) << rep << "}\n";
        return s.str();
    }
};

class cf_statement : public statement {
public:
    opt_label *opt_label_;
    luna_string *code_id_;
    opt_exprs *opt_exprs_;
    opt_setdf_rules *opt_setdf_rules_;
    opt_rules *opt_rules_;
    opt_behavior *opt_behavior_;

    cf_statement(opt_label *opt_label,
            luna_string *code_id,
            opt_exprs *opt_exprs,
            opt_setdf_rules *opt_setdf_rules,
            opt_rules *opt_rules,
            opt_behavior *opt_behavior)
        : opt_label_(opt_label),
        code_id_(code_id), 
        opt_exprs_(opt_exprs),
        opt_setdf_rules_(opt_setdf_rules),
        opt_rules_(opt_rules),
        opt_behavior_(opt_behavior) {}
    
    ~cf_statement() {
        delete opt_label_;
        delete code_id_;
        delete opt_exprs_;
        delete opt_setdf_rules_;
        delete opt_rules_;
        delete opt_behavior_;
    }

    std::string to_string() const override {
        
        return 
            (opt_label_ == nullptr ? "nullptr" : opt_label_->to_string()) + " " +
            (code_id_ == nullptr ? "nullptr" : code_id_->to_string()) + "(" +
            (opt_exprs_ == nullptr ? "nullptr" :opt_exprs_->to_string()) + ")" +
            (opt_setdf_rules_ == nullptr ? "nullptr" :opt_setdf_rules_->to_string()) +
            (opt_rules_ == nullptr ? "nullptr" :opt_rules_->to_string()) + 
            (opt_behavior_ == nullptr ? "nullptr" :opt_behavior_->to_string());
    }

    std::string to_json(const uint shift) const override {
        std::stringstream s;
        std::string rep = std::string(shift, ' ');

        s << rep << "\"cf_statement\" : {\n" 
        << rep << " " << "\"code_id\": {" << (code_id_ == nullptr ? "" : code_id_->to_json(shift + 1)) << rep << "}" 
        << (opt_label_ == nullptr ? "" : ",\n" + opt_label_->to_json(shift + 1)) << rep 
        << (opt_exprs_ == nullptr ? "" : ",\n" + opt_exprs_->to_json(shift + 1))  << rep 
        << (opt_setdf_rules_ == nullptr ? "" : ",\n" + opt_setdf_rules_->to_json(shift + 1)) << rep 
        << (opt_rules_ == nullptr ? "" : ",\n" + opt_rules_->to_json(shift + 1)) << rep 
        << (opt_behavior_ == nullptr ? "" : ",\n" + opt_behavior_->to_json(shift + 1)) 
        << rep << "}\n";

        return s.str();
    }
};

class id_seq : public virtual_token {
public:
    std::vector<id*>* seq_;
    id_seq(std::vector<id*>* seq) : seq_(seq) {}
    id_seq() : seq_(new std::vector<id*>()) {}

    ~id_seq() {
        for (auto i : *seq_) {
            delete i;
        }
        delete seq_;
    }

    std::string to_json(const uint shift) const override {
        std::stringstream s;
        std::string rep = std::string(shift, ' ');

        s << "\"id_seq\" : [\n";

        size_t len = seq_->size();

        for (int j = 0; j < len; j++) {
            auto i = seq_->at(j);
            s << rep << "{\n" << (i == nullptr ? "" : (i->to_json(shift + 1))) << "}" << (j == len - 1 ? "" : ",");
        }

        s << "\n]";

        return s.str();
    }
};

class behv_pragma_eq : public behv_pragma {
    public:
        luna_string *name_;
        id* id_;
        expr* expr_;
        behv_pragma_eq(luna_string *name, id* id, expr* expr) : name_(name), id_(id), expr_(expr) {}

        ~behv_pragma_eq() {
            delete name_;
            delete id_;
            delete expr_;
        }

    std::string to_json(const uint shift) const override {
        std::stringstream s;
        std::string rep = std::string(shift, ' ');

        s << rep << "\"behv_pragma_eq\" : {\n" 
        << rep << "name: {" << name_->to_json(shift + 1) << "},\n"
        << id_->to_json(shift + 1) << ",\n" 
        << expr_->to_json(shift + 1) << "\n}\n";

        return s.str();
    }
};

class behv_pragma_eqg : public behv_pragma {
    public:
        luna_string *name_;
        id* id_;
        expr* expr_;
        behv_pragma_eqg(luna_string *name, id* id, expr* expr) : name_(name), id_(id), expr_(expr) {}

        ~behv_pragma_eqg() {
            delete name_;
            delete id_;
            delete expr_;
        }

    std::string to_json(const uint shift) const override {
        std::stringstream s;
        std::string rep = std::string(shift, ' ');

        s << rep << "\"behv_pragma_eqg\" : {\n" 
        << rep << "\"name\": {" << name_->to_json(shift + 1) << "},\n"
        << id_->to_json(shift + 1) << ",\n" 
        << expr_->to_json(shift + 1) << "\n}\n";


        return s.str();
    }
};

class behv_pragma_id_seq : public behv_pragma {
    public:
        luna_string *name_;
        id_seq *id_seq_;
        behv_pragma_id_seq(luna_string *name, id_seq *id_seq) : name_(name), id_seq_(id_seq) {}
        behv_pragma_id_seq() : name_(new luna_string()), id_seq_(new id_seq()) {}

        ~behv_pragma_id_seq() {
            delete name_;
            delete id_seq_;
        }

    std::string to_json(const uint shift) const override {
        std::stringstream s;
        std::string rep = std::string(shift, ' ');

        s << rep << "\"behv_pragma_id_seq\" : {\n" 
        << rep << "\"name\": {" << name_->to_json(shift + 1) 
        << (id_seq_ == nullptr ? "": "},\n" + id_seq_->to_json(shift + 1)) << "\n}\n";

        return s.str();
    }
};

class behv_pragma_expr : public behv_pragma {
    public:
        luna_string *name_;
        expr *expr_;
        behv_pragma_expr(luna_string *name, expr *expr) : expr_(expr), name_(name) {}

        ~behv_pragma_expr() {
            delete name_;
            delete expr_;
        }

    std::string to_json(const uint shift) const override {
        std::stringstream s;
        std::string rep = std::string(shift, ' ');

        s << rep << "\"behv_pragma_expr\" : {\n" 
        << rep << "\"name\": {" << name_->to_json(shift + 1) << "}" << ",\n"
        << "\"expr\" : {" << expr_->to_json(shift + 1) << "}" << "\n}\n";

        return s.str();
    }
};

class behv_pragma_name_seq : public behv_pragma {
    public:    
        luna_string *name_;
        name_seq *seq_;
        behv_pragma_name_seq(luna_string *name, name_seq *seq) : name_(name), seq_(seq) {}

        ~behv_pragma_name_seq() {
            delete name_;
            delete seq_;
        }

    std::string to_json(const uint shift) const override {
        std::stringstream s;
        std::string rep = std::string(shift, ' ');

        s << rep << "\"behv_pragma_name_seq\" : {\n" 
        << rep << "name: " << name_->to_json(shift + 1) << ",\n"
        << seq_->to_json(shift + 1) << "\n}\n";

        return s.str();
    }
};

class opt_expr : public virtual_token {
    public:
        exprs_seq *exprs_seq_;
        opt_expr(exprs_seq *exprs_seq) : exprs_seq_(exprs_seq) {}

        ~opt_expr() {
            delete exprs_seq_;
        }

    std::string to_json(const uint shift) const override {
        std::stringstream s;
        std::string rep = std::string(shift, ' ');

        s << rep << "\"opt_expr\" : {\n" << exprs_seq_->to_json(shift + 1) << "\n}\n";

        return s.str();
    }
};

class simple_id : public id {
    public:
        luna_string* value_;
        simple_id(luna_string* name) {
            // delete value_;
            value_ = name;
        }

        std::string to_string() const override {
            return value_->to_string();
        }

    std::string to_json(const uint shift) const override {
        std::stringstream s;
        std::string rep = std::string(shift, ' ');

        s << rep << "\"simple_id\" : {" << value_->to_json(shift + 1) << "}\n";

        return s.str();
    }
};

class ast {
public:
    ast(program * program) : program_(program) {}
    ast() : tokens_(new std::vector<std::string*>()) {}

    ~ast() {
        for (auto i : *tokens_) {
            // std::cerr << "dtor: " << i << " " << *i << std::endl;
            delete i; 
        }

        delete tokens_;

        delete program_;
    }

    void set_program(program* program) {
        program_ = program;
    }

    program* get_program() {
        return program_;
    }

    void push_token(std::string* token) {
        tokens_->push_back(token);
    }

    std::string to_string() const {
        // // std::cerr << "ast to_string\n";
        return program_->to_string();
    }

    int get_tokens_count() {
        return tokens_->size();
    }

    std::string to_json() const {
        if (program_ == nullptr) {
            return "{}";
        }
        return program_->to_json(1);
    }

private:
    program* program_;
    std::vector<std::string*>* tokens_;

    friend class DDG;
};

#endif
