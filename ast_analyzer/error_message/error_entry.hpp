#pragma once

#include <cstring>
#include <vector>

class serializiable {
public:
  virtual std::string to_json() const = 0;
};

class call_stack_entry : public serializiable {
public:
  std::string file;
  int line;
  std::string name;

  call_stack_entry(std::string file, int line, std::string name) : file(file), line(line), name(name) {}

  std::string to_json() const override {
    std::stringstream s;

    s << "\"call_stack_entry\" : {" << "\"file\" : " << "\"" << file << "\"" << ","
    << "\"line\" : " << std::to_string(line) << ","
    << "\"name\" : " << "\"" << name << "\""
    << "}";

    return s.str();
  }
};

class init : public serializiable{
public:
  std::vector<int> lines;

  std::string to_json() const override{
    std::stringstream s;

    size_t len = lines.size();
    s << "\"init : \" : [";

    for (int j = 0; j < len; j++) {
        auto i = lines.at(j);
        s << "{" << "\"line\" : " << std::to_string(i) << "}" << (j == len - 1 ? "" : ",");
    }

    s << "]";

    return s.str();
  }
};

class declared : public serializiable{
public:
  std::vector<call_stack_entry> decls_;

  void add_decl(call_stack_entry d) {
    decls_.push_back(d);
  }

  declared() = default;

  declared(call_stack_entry d) : declared() {
    add_decl(d);
  }

  std::string to_json() const override{
    std::stringstream s;

    size_t len = decls_.size();
    s << "\"declared\" : [";

    for (int j = 0; j < len; j++) {
        auto i = decls_.at(j);
        s << "{" << i.to_json() << "}" << (j == len - 1 ? "" : ",");
    }

    s << "]";

    return s.str();
  }
};

class initialized : public serializiable{
public:
  std::vector<init> inits_;

  std::string to_json() const override{
    std::stringstream s;

    size_t len = inits_.size();
    s << "\"initialized : \" : [";

    for (int j = 0; j < len; j++) {
        auto i = inits_.at(j);
        s << "{" << i.to_json() << "}" << (j == len - 1 ? "" : ",");
    }

    s << "]";

    return s.str();
  }
};

class df : public serializiable{
public:
  std::string name_;
  declared declared_;
  initialized initialized_;

  df(std::string name, declared d, initialized i) : name_(name), declared_(d), initialized_(i) {}

  std::string to_json() const override{
    std::stringstream s;

    s << "\"df\" : {"<< "\"name\" : " << "\"" << name_ << "\"" << ","
    << declared_.to_json() << ","
    << initialized_.to_json()
    << "}";

    return s.str();
  }
};

class cf : public serializiable {
public:
  std::string name;
  std::string type;
  std::string file;
  int line;

  cf(std::string name, std::string type, std::string file, int line) : file(file), type(type), line(line), name(name) {}

  std::string to_json() const override {
    std::stringstream s;

    s << "\"cf\" : {" 
    << "\"file\" : " << "\"" << file << "\"" << ","
    << "\"type\" : " << "\"" << type << "\"" << ","
    << "\"name\" : " << "\"" << name << "\"" << ","
    << "\"line\" : " << std::to_string(line)
    << "}";

    return s.str();
  }
};

class details : public serializiable{
public:
  std::vector<df> dfs;
  std::vector<cf> cfs;
  std::vector<call_stack_entry> cse;

  void add_df(df d) {
    dfs.push_back(d);
  }

  void add_cf(cf c) {
    cfs.push_back(c);
  }

  void add_call_stack_entry(call_stack_entry c) {
    cse.push_back(c);
  }

  details() = default;

  std::string to_json() const override {
    std::stringstream s;

    size_t len = dfs.size();
    s << "\"details \" : [";

    for (int j = 0; j < len; j++) {
        auto i = dfs.at(j);
        s << "{" << i.to_json() << "}" << (j == len - 1 ? "" : ",");
    }

    s << (len == 0 ? " " : ",");

    len = cfs.size();
    for (int j = 0; j < len; j++) {
        auto i = cfs.at(j);
        s << "{" << i.to_json() << "}" << (j == len - 1 ? "" : ",");
    }

    s << (len == 0 ? " " : ",");

    len = cse.size();
    for (int j = 0; j < len; j++) {
        auto i = cse.at(j);
        s << "{" << i.to_json() << "}" << (j == len - 1 ? "" : ",");
    }

    s << "]";

    return s.str();
  }
};


// class expression : serializiable {};

// class bin_expression : public expression {
// public:
//   std::string type;
//   std::vector<expression&> args;
// };

// class const_expression : public expression {
// public:
//   std::string value;
// };

// class ref_expression : public expression {
// public:
//   std::string name;
//   std::vector<expression&> indecies;
// };