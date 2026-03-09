#pragma once

#include <nlohmann/json.hpp>

#include <map>
#include <string>
#include <vector>

class base_object {
  public:
    virtual ~base_object() = default;

    virtual nlohmann::json to_json() const = 0;
};

class call_stack_entry : public base_object {
    std::string file;
    int line;
    std::string name;

  public:
    call_stack_entry(const std::string &file, int line, const std::string &name)
        : file(file), line(line), name(name) {}

    nlohmann::json to_json() const override {
        nlohmann::json j;
        j["file"] = file;
        j["line"] = line;
        j["name"] = name;
        return j;
    }
};

class call_stack : public base_object {
    std::vector<call_stack_entry> entries;

  public:
    call_stack(const std::vector<call_stack_entry> &entries) : entries(entries) {}

    nlohmann::json to_json() const override {
        nlohmann::json j = nlohmann::json::array();
        for (const auto &entry : entries) {
            j.push_back(entry.to_json());
        }
        return j;
    }
};

class df_struct : public base_object {
    std::string name;
    std::vector<call_stack> declared;
    std::vector<call_stack> initialized;
    std::vector<call_stack> used;

  public:
    df_struct(const std::string &name, const std::vector<call_stack> &declared,
              const std::vector<call_stack> &initialized = {},
              const std::vector<call_stack> &used = {})
        : name(name), declared(declared), initialized(initialized), used(used) {}

    nlohmann::json to_json() const override {
        nlohmann::json j;
        j["name"] = name;

        j["declared"] = nlohmann::json::array();
        for (const auto &cs : declared) {
            j["declared"].push_back(cs.to_json());
        }

        if (!initialized.empty()) {
            j["initialized"] = nlohmann::json::array();
            for (const auto &cs : initialized) {
                j["initialized"].push_back(cs.to_json());
            }
        }

        if (!used.empty()) {
            j["used"] = nlohmann::json::array();
            for (const auto &cs : used) {
                j["used"].push_back(cs.to_json());
            }
        }

        return j;
    }
};

class cf_struct : public base_object {
    std::string name;
    std::string type;  // "struct" or "extern"
    std::string file;
    int line;

  public:
    cf_struct(const std::string &name, const std::string &type, const std::string &file, int line)
        : name(name), type(type), file(file), line(line) {}

    cf_struct(const std::string &name, const std::string &type,
              const std::pair<std::string, int> &file_line)
        : name(name), type(type), file(file_line.first), line(file_line.second) {}

    nlohmann::json to_json() const override {
        nlohmann::json j;
        j["name"] = name;
        j["type"] = type;
        j["file"] = file;
        j["line"] = line;
        return j;
    }
};

class cf_struct_array : public base_object {
    std::vector<cf_struct> cf_list;

  public:
    cf_struct_array(const std::vector<cf_struct> &cf_list) : cf_list(cf_list) {}

    nlohmann::json to_json() const override {
        nlohmann::json j = nlohmann::json::array();
        for (const auto &cf : cf_list) {
            j.push_back(cf.to_json());
        }
        return j;
    }
};

class for_struct : public base_object {
    std::string var;
    std::string first;
    std::string last;
    call_stack where;

  public:
    for_struct(const std::string &var, const std::string &first, const std::string &last,
               const call_stack &where)
        : var(var), first(first), last(last), where(where) {}
    nlohmann::json to_json() const override {
        nlohmann::json j;
        j["var"] = var;
        j["first"] = first;
        j["last"] = last;
        j["where"] = where.to_json();
        return j;
    }
};

class global_const : public base_object {
    std::string name;
    std::string file;
    int line;

  public:
    global_const(const std::string &name, const std::string &file, int line)
        : name(name), file(file), line(line) {}

    global_const(const std::string &name, const std::pair<std::string, int> &file_line)
        : name(name), file(file_line.first), line(file_line.second) {}

    nlohmann::json to_json() const override {
        nlohmann::json j;
        j["name"] = name;
        j["file"] = file;
        j["line"] = line;
        return j;
    }
};

class global_var : public base_object {
    std::string name;
    std::string file;
    int line;

  public:
    global_var(const std::string &name, const std::string &file, int line)
        : name(name), file(file), line(line) {}

    global_var(const std::string &name, const std::pair<std::string, int> &file_line)
        : name(name), file(file_line.first), line(file_line.second) {}

    nlohmann::json to_json() const override {
        nlohmann::json j;
        j["name"] = name;
        j["file"] = file;
        j["line"] = line;
        return j;
    }
};

enum class ErrorType {
    BASE_ERROR,
    SYN1,
    SYN2,
    SYN3,
    SYN4,
    SYN54,
    SYN55,
    SYN61,
    SYN62,
    SYN7,
    SYN8,
    SYN9,
    SYN10,
    SYN11,
    SYN12,
    SYN13,
    SYN14,
    SEM13,
    MY1,
    MY2,
};

class ErrorBase {
  private:
    ErrorType type = ErrorType::BASE_ERROR;
    std::map<std::string, std::string> strDetails;
    std::map<std::string, int> intDetails;
    std::map<std::string, std::unique_ptr<base_object>> objDetails;

  public:
    ErrorBase() = default;
    ErrorBase(ErrorType type) : type(type) {}

    ErrorBase(const ErrorBase &) = delete;
    ErrorBase &operator=(const ErrorBase &) = delete;

    ErrorBase(ErrorBase &&) = default;
    ErrorBase &operator=(ErrorBase &&) = default;

    ~ErrorBase() = default;

    void setType(ErrorType newType) { type = newType; }
    ErrorType getType() const { return type; }

    void setStringDetail(const std::string &key, const std::string &value) {
        strDetails[key] = value;
    }

    std::string getStringDetail(const std::string &key) const {
        auto it = strDetails.find(key);
        return it != strDetails.end() ? it->second : "";
    }

    void setIntDetail(const std::string &key, int value) { intDetails[key] = value; }

    int getIntDetail(const std::string &key) const {
        auto it = intDetails.find(key);
        return it != intDetails.end() ? it->second : 0;
    }

    void setObjectDetail(const std::string &key, std::unique_ptr<base_object> value) {
        objDetails[key] = std::move(value);
    }

    const base_object *getObjectDetail(const std::string &key) const {
        auto it = objDetails.find(key);
        return it != objDetails.end() ? it->second.get() : nullptr;
    }

    nlohmann::json to_json() const {
        nlohmann::json j;

        switch (type) {
        case ErrorType::SYN1: {
            nlohmann::json details;
            details["call_stack_entry"] = objDetails.at("call_stack_entry")->to_json();
            details["cf"] = objDetails.at("cf")->to_json();
            j["details"] = details;
            j["error_code"] = "SYN1";
            break;
        }
        case ErrorType::SYN2: {
            nlohmann::json details;
            details["call_stack_entry"] = objDetails.at("call_stack_entry")->to_json();
            j["details"] = details;
            j["error_code"] = "SYN2";
            break;
        }
        case ErrorType::SYN3: {
            nlohmann::json details;
            details["call_stack_entry"] = objDetails.at("call_stack_entry")->to_json();
            details["cf"] = objDetails.at("cf")->to_json();
            j["details"] = details;
            j["error_code"] = "SYN3";
            break;
        }
        case ErrorType::SYN4: {
            nlohmann::json details;
            j["details"] = details;
            j["error_code"] = "SYN4";
            break;
        }
        case ErrorType::SYN54: {
            nlohmann::json details;
            details["cf"] = objDetails.at("cf")->to_json();
            j["details"] = details;
            j["error_code"] = "SYN5.4";
            break;
        }
        case ErrorType::SYN55: {
            nlohmann::json details;
            details["cf"] = objDetails.at("cf")->to_json();
            j["details"] = details;
            j["error_code"] = "SYN5.5";
            break;
        }
        case ErrorType::SYN61: {
            nlohmann::json details;
            details["cfs"] = objDetails.at("cfs")->to_json();
            j["details"] = details;
            j["error_code"] = "SYN6.1";
            break;
        }
        case ErrorType::SYN62: {
            nlohmann::json details;
            details["cfs"] = objDetails.at("cfs")->to_json();
            j["details"] = details;
            j["error_code"] = "SYN6.2";
            break;
        }
        case ErrorType::SYN7: {
            j["details"] = nlohmann::json::object();
            j["error_code"] = "SYN7";
            break;
        }
        // case ErrorType::SYN8: {
        //     nlohmann::json details;
        //     details["df"] = objDetails.at("df")->to_json();
        //     j["details"] = details;
        //     j["error_code"] = "SYN8";
        //     break;
        // }
        // case ErrorType::SYN9: {
        //     nlohmann::json details;
        //     details["expression"] = getStringDetail("expression");
        //     j["details"] = details;
        //     j["error_code"] = "SYN9";
        //     break;
        // }
        case ErrorType::SYN10: {
            nlohmann::json details;
            details["cf"] = objDetails.at("cf")->to_json();
            j["details"] = details;
            j["error_code"] = "SYN10";
            break;
        }
        // case ErrorType::SYN11: {
        //     nlohmann::json details;
        //     details["expression"] = getStringDetail("expression");
        //     j["details"] = details;
        //     j["error_code"] = "SYN11";
        //     break;
        // }
        case ErrorType::SYN12: {
            nlohmann::json details;
            details["cf"] = objDetails.at("cf")->to_json();
            j["details"] = details;
            j["error_code"] = "SYN12";
            break;
        }
        case ErrorType::SYN13: {
            nlohmann::json details;
            details["first"] = objDetails.at("first")->to_json();
            details["second"] = objDetails.at("second")->to_json();
            j["details"] = details;
            j["error_code"] = "SYN13";
            break;
        }
        case ErrorType::SYN14: {
            nlohmann::json details;
            details["first"] = objDetails.at("first")->to_json();
            details["second"] = objDetails.at("second")->to_json();
            j["details"] = details;
            j["error_code"] = "SYN14";
            break;
        }
        case ErrorType::SEM13: {
            nlohmann::json details;
            details["global_var"] = objDetails.at("global_var")->to_json();
            j["details"] = details;
            j["error_code"] = "SEM13";
            break;
        }
        default:
            break;
        }

        return j;
    }
};
