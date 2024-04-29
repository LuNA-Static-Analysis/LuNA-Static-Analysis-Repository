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

    s << "{" << "\"file\" : " << "\"" << file << "\"" << ","
    << "\"line\" : " << std::to_string(line) << ","
    << "\"name\" : " << "\"" << name << "\""
    << "}";

    return s.str();
  }
};

class init : public serializiable {
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

class call_stack : public serializiable {
public:
  std::vector<call_stack_entry> call_stack_entries_;

  void add_call_stack_entry(call_stack_entry d) {
    call_stack_entries_.push_back(d);
  }

  call_stack(call_stack_entry cse) {
    add_call_stack_entry(cse);
  }

  call_stack() = default;


  std::string to_json() const override{
    std::stringstream s;

    size_t len = call_stack_entries_.size();

    for (int j = 0; j < len; j++) {
        auto i = call_stack_entries_.at(j);
        s <<  i.to_json() << (j == len - 1 ? "" : ",");
    }

    return s.str();
  }
};

class declared : public serializiable {
public:
  std::vector<call_stack> call_stacks_;

  void add_decl(call_stack d) {
    call_stacks_.push_back(d);
  }

  declared() = default;

  declared(call_stack d) : declared() {
    add_decl(d);
  }

  std::string to_json() const override{
    std::stringstream s;

    size_t len = call_stacks_.size();
    s << "\"declared\" : [";

    for (int j = 0; j < len; j++) {
        auto i = call_stacks_.at(j);
        s << "[" << i.to_json() << "]" << (j == len - 1 ? "" : ",");
    }

    s << "]";

    return s.str();
  }
};

class initialized : public serializiable{
public:
  std::vector<call_stack> call_stacks_;

  std::string to_json() const override{
    std::stringstream s;

    size_t len = call_stacks_.size();
    s << "\"initialized\" : [";

    for (int j = 0; j < len; j++) {
        auto i = call_stacks_.at(j);
        s << "[" << i.to_json() << "]" << (j == len - 1 ? "" : ",");
    }

    s << "]";

    return s.str();
  }
};

class used : public serializiable{
public:
  std::vector<call_stack> inits_;

  std::string to_json() const override{
    std::stringstream s;

    size_t len = inits_.size();
    s << "\"used\" : [";

    for (int j = 0; j < len; j++) {
        auto i = inits_.at(j);
        s << "[" << i.to_json() << "]" << (j == len - 1 ? "" : ",");
    }

    s << "]";

    return s.str();
  }
};


class df : public serializiable {
public:
  std::string name_;
  declared declared_;
  initialized initialized_;
  used used_;

  df(std::string name, declared d, initialized i, used u) : name_(name), declared_(d), initialized_(i), used_(u) {}

  std::string to_json() const override{
    std::stringstream s;

    s << "{"
    << "\"name\" : " << "\"" << name_ << "\"" << ","
    << declared_.to_json() << ","
    << initialized_.to_json() << ","
    << used_.to_json()
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

    s << "{" 
    << "\"file\" : " << "\"" << file << "\"" << ","
    << "\"type\" : " << "\"" << type << "\"" << ","
    << "\"name\" : " << "\"" << name << "\"" << ","
    << "\"line\" : " << std::to_string(line)
    << "}";

    return s.str();
  }
};

class for_entry : public serializiable {
public:
  std::string var;
  std::string first;
  std::string last;
  call_stack where;

  for_entry(std::string var, std::string first, std::string last, call_stack cs) : 
          var(var), 
          first(first), 
          last(last), 
          where(cs) {}

  for_entry() = default; 

  std::string to_json() const override {
    std::stringstream s;

    s << "{" 
    << "\"var\" : " << "\"" << var << "\"" << ","
    << "\"first\" : " << "\"" << first << "\"" << ","
    << "\"last\" : " << "\"" << last << "\"" << ","
    << "\"where\" : [" << where.to_json() << "]"
    << "}";

    return s.str();
  }
};

class details : public serializiable {
public:
  std::vector<df> dfs;
  std::vector<cf> cfs;
  std::vector<call_stack_entry> cse;
  std::vector<std::string> exprs;
  for_entry f;
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

  void set_for(for_entry f) {
    this->f = f;
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

    s << (len != 0 ? "\"call_stack_entry\" : " + cse.at(0).to_json() : "");

    s << (len != 0 && exprs.size() != 0 ? "," : " ");

    len = exprs.size();
    s << (len != 0 ? "\"expr\" : " + exprs.at(0) : "");

    s << (f.var != "" && len != 0 ? "," : " ");
    s << (f.var != "" ? ("\"for\" :" + f.to_json()) : "");

    s << "}";

    return s.str();
  }

  std::string dfs_to_json() const {
    if (dfs.size() == 0) return "";

    if (error_code == "03" || error_code == "05" || error_code == "07" || error_code == "10" || error_code == "14") {
      return "\"df\" :" + dfs.at(0).to_json();
    }
    else {
      std::stringstream s;
      s << "\"dfs\" : [";
      for (int j = 0; j < dfs.size(); j++) {
          auto i = dfs.at(j);
          s << i.to_json() << (j == dfs.size() - 1 ? "" : ",");
      }
      s << "]";
      return s.str();
    }
    return "";
  }

  std::string cfs_to_json() const {
    if (cfs.size() == 0) return "";

    if (error_code == "02" || error_code == "04" || error_code == "06" || error_code == "17") {
      return "\"cf\" :" + cfs.at(0).to_json();
    }
    else {
      std::stringstream s;
      s << "\"cfs\" : [";
      for (int j = 0; j < cfs.size(); j++) {
          auto i = cfs.at(j);
          s << i.to_json() << (j == cfs.size() - 1 ? "" : ",");
      }
      s << "]";
      return s.str();
    }
    return "";
  }
};