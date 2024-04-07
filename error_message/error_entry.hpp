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
    s << "\"init\" : [";

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
    s << "\"initialized\" : [";

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
  std::vector<std::string> exprs;
  std::string error_code;

  void add_df(df d) {
    dfs.push_back(d);
  }

  void add_cf(cf c) {
    cfs.push_back(c);
  }

  void add_call_stack_entry(call_stack_entry c) {
    cse.push_back(c);
  }

  void add_expression(std::string e) {
    exprs.push_back(e);
  }

  details() = default;
  details(std::string error_code) : error_code(error_code) {}

  std::string to_json() const override {

    std::stringstream s;

    size_t len = dfs.size();
    s << "\"details\" : {";

    s << dfs_to_json();
    s << (len != 0 && cfs.size() != 0 ? "," : " ");
    len = cfs.size();

    s << cfs_to_json();
    s << (len != 0 && cse.size() != 0 ? "," : " ");
    len = cse.size();

    s << (len != 0 ? cse.at(0).to_json() : "");

    s << (len != 0 && exprs.size() != 0 ? "," : " ");

    len = exprs.size();
    s << (len != 0 ? "\"expr\" : " + exprs.at(0) : "");

    s << "}";

    return s.str();
  }


  std::string dfs_to_json() const {
    if (dfs.size() == 0) return "";

    if (error_code == "03" || error_code == "05" || error_code == "07" || error_code == "10" || error_code == "14") {
      return dfs.at(0).to_json();
    }
    else {
      std::stringstream s;
      s << "\"dfs \" : [";
      for (int j = 0; j < dfs.size(); j++) {
          auto i = dfs.at(j);
          s << "{" << i.to_json() << "}" << (j == dfs.size() - 1 ? "" : ",");
      }
      s << "]";
      return s.str();
    }
    return "";
  }

  std::string cfs_to_json() const {
    if (cfs.size() == 0) return "";

    if (error_code == "02" || error_code == "04" || error_code == "17") {
      return cfs.at(0).to_json();
    }
    else {
      std::stringstream s;
      s << "\"cfs \" : [";
      for (int j = 0; j < cfs.size(); j++) {
          auto i = cfs.at(j);
          s << "{" << i.to_json() << "}" << (j == cfs.size() - 1 ? "" : ",");
      }
      s << "]";
      return s.str();
    }
    return "";
  }
};