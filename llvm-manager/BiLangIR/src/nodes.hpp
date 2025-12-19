#pragma once

#include "depends.hpp"

#include <string>
#include <vector>

using namespace llvm;

class BlockExprAST;
class CheckableExprAST;

class ExprAST {
  private:
    int position = -1;

  public:
    virtual ~ExprAST() = default;
    virtual Value *codegen() = 0;
    virtual std::string getText() = 0;
    virtual InitStatus containsInit() = 0;

    void setPosition(int pos) { position = pos; }
    int getPosition() const { return position; }
};

extern std::map<std::string, Value *> NameMap;
extern std::map<std::string, bool> initsNameMap;

class IntExprAST : public ExprAST {
    int Val;

  public:
    IntExprAST(int val, int position = -1) : Val(val) { setPosition(position); }
    Value *codegen() override;
    std::string getText() override;
    InitStatus containsInit() override;
};

class RealExprAST : public ExprAST {
    double Val;

  public:
    RealExprAST(double val, int position = -1) : Val(val) { setPosition(position); }
    Value *codegen() override;
    std::string getText() override;
    InitStatus containsInit() override;
};

class StringExprAST : public ExprAST {
    std::string Val;

  public:
    StringExprAST(const std::string &val, int position = -1) : Val(val) { setPosition(position); }
    Value *codegen() override;
    std::string getText() override;
    InitStatus containsInit() override;
};

class ValueExprAST : public ExprAST {
    std::string Name;

  public:
    ValueExprAST(const std::string &Name, int position = -1) : Name(Name) { setPosition(position); }
    Value *codegen() override;
    std::string getText() override;
    InitStatus containsInit() override;
};

class NameExprAST : public ExprAST {
    std::string Name;

  public:
    NameExprAST(const std::string &Name, int position = -1) : Name(Name) { setPosition(position); }
    Value *codegen() override;
    std::string getText() override;
    InitStatus containsInit() override;
};

class VariableExprAST : public ExprAST {
    std::string Name;

  public:
    VariableExprAST(const std::string &Name, int position = -1) : Name(Name) {
        setPosition(position);
    }
    Value *codegen() override;
    std::string getText() override;
    InitStatus containsInit() override;
};

class ComplexExprAST : public ExprAST {
    std::unique_ptr<ExprAST> id, expr;
    std::string name;

  public:
    ComplexExprAST(std::unique_ptr<ExprAST> id, std::unique_ptr<ExprAST> expr,
                   const std::string &name, int position = -1)
        : id(std::move(id)), expr(std::move(expr)), name(name) {
        setPosition(position);
    }
    Value *codegen() override;
    std::string getText() override;
    InitStatus containsInit() override;
    std::string getBaseName();
    int getDimension(int currentDim = 1);
};

class BinaryExprAST : public ExprAST {
    std::string Op;
    std::unique_ptr<ExprAST> LHS, RHS;

  public:
    BinaryExprAST(const std::string &Op, std::unique_ptr<ExprAST> LHS, std::unique_ptr<ExprAST> RHS,
                  int position = -1)
        : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {
        setPosition(position);
    }
    Value *codegen() override;
    std::string getText() override;
    InitStatus containsInit() override;
};

class BlockExprAST : public ExprAST {
    std::vector<std::unique_ptr<ExprAST>> DfDecls;
    std::vector<std::unique_ptr<CheckableExprAST>> Statements;
    std::vector<std::unique_ptr<CheckableExprAST>> InitNames;
    std::vector<std::unique_ptr<CheckableExprAST>> UndefStatements;

  public:
    BlockExprAST(std::vector<std::unique_ptr<ExprAST>> DfDecls,
                 std::vector<std::unique_ptr<CheckableExprAST>> Statements,
                 std::vector<std::unique_ptr<CheckableExprAST>> InitNames,
                 std::vector<std::unique_ptr<CheckableExprAST>> UndefStatements = {},
                 int position = -1)
        : DfDecls(std::move(DfDecls)), Statements(std::move(Statements)),
          InitNames(std::move(InitNames)), UndefStatements(std::move(UndefStatements)) {
        setPosition(position);
    }
    Value *codegen() override;
    std::string getText() override;
    InitStatus containsInit() override;
    bool containsUndef();
    InitStatus tryResolving();
};

class CheckableExprAST : public ExprAST {
  protected:
    std::unique_ptr<BlockExprAST> Block;

  public:
    CheckableExprAST(std::unique_ptr<BlockExprAST> Block, int position = -1)
        : Block(std::move(Block)) {
        setPosition(position);
    }
    virtual ~CheckableExprAST() = default;

    virtual InitStatus tryResolveInitStatus() = 0;
};

class IfExprAST : public CheckableExprAST {
    std::unique_ptr<ExprAST> Cond;

  public:
    IfExprAST(std::unique_ptr<ExprAST> Cond, std::unique_ptr<BlockExprAST> Then, int position = -1)
        : CheckableExprAST(std::move(Then), position), Cond(std::move(Cond)) {}
    Value *codegen() override;
    std::string getText() override;
    InitStatus containsInit() override;
    InitStatus tryResolveInitStatus() override;
};

class WhileExprAST : public CheckableExprAST {
    std::string VarName;
    std::unique_ptr<ExprAST> outDfId, Cond;
    std::unique_ptr<ExprAST> startVal;

  public:
    WhileExprAST(const std::string &VarName, std::unique_ptr<ExprAST> outDfId,
                 std::unique_ptr<ExprAST> Cond, std::unique_ptr<BlockExprAST> Block,
                 std::unique_ptr<ExprAST> startVal, int position = -1)
        : CheckableExprAST(std::move(Block), position), VarName(VarName),
          outDfId(std::move(outDfId)), Cond(std::move(Cond)), startVal(std::move(startVal)) {}
    Value *codegen() override;
    std::string getText() override;
    InitStatus containsInit() override;
    InitStatus tryResolveInitStatus() override;
};

class ForExprAST : public CheckableExprAST {
    std::string VarName;
    std::unique_ptr<ExprAST> Start, End;

  public:
    ForExprAST(const std::string &VarName, std::unique_ptr<ExprAST> Start,
               std::unique_ptr<ExprAST> End, std::unique_ptr<BlockExprAST> Block, int position = -1)
        : CheckableExprAST(std::move(Block), position), VarName(VarName), Start(std::move(Start)),
          End(std::move(End)) {}
    Value *codegen() override;
    std::string getText() override;
    InitStatus containsInit() override;
    InitStatus tryResolveInitStatus() override;
};

class LetExprAST : public CheckableExprAST {
    std::vector<std::string> Names;
    std::vector<std::unique_ptr<ExprAST>> Exprs;
    std::vector<Type *> Types;

  public:
    LetExprAST(std::vector<std::string> Names, std::vector<std::unique_ptr<ExprAST>> Exprs,
               std::unique_ptr<BlockExprAST> Block, std::vector<Type *> Types, int position = -1)
        : CheckableExprAST(std::move(Block), position), Names(std::move(Names)),
          Exprs(std::move(Exprs)), Types(std::move(Types)) {}
    Value *codegen() override;
    std::string getText() override;
    InitStatus containsInit() override;
    InitStatus tryResolveInitStatus() override;
};

class CallExprAST : public CheckableExprAST {
    std::string Callee;
    std::vector<std::unique_ptr<ExprAST>> Args;
    bool isInitFunc;

  public:
    CallExprAST(const std::string &Callee, std::vector<std::unique_ptr<ExprAST>> Args,
                bool isInitFunc = false, int position = -1)
        : CheckableExprAST(nullptr, position), Callee(Callee), Args(std::move(Args)),
          isInitFunc(isInitFunc) {}
    Value *codegen() override;
    std::string getText() override;
    InitStatus containsInit() override;
    InitStatus tryResolveInitStatus() override;
};

class SubExprAST : public CheckableExprAST {
    std::string Name;
    std::vector<Type *> Params;
    std::vector<std::string> Names;
    std::vector<int> NamePositions;
    std::string subName;

  public:
    SubExprAST(const std::string &Name, std::vector<Type *> Params,
               std::unique_ptr<BlockExprAST> Block, std::vector<std::string> Names,
               std::vector<int> NamePositions, int position = -1)
        : CheckableExprAST(std::move(Block), position), Name(Name), Params(std::move(Params)),
          Names(std::move(Names)), NamePositions(std::move(NamePositions)) {}
    Value *codegen() override;
    std::string getText() override;
    InitStatus containsInit() override;
    InitStatus tryResolveInitStatus() override;

    void setSubName(const std::string &subName) { this->subName = subName; }
    std::string getSubName() const { return subName; }
};

class ImportExprAST : public ExprAST {
    std::string LunaCodeID;
    std::string CFuncName;
    std::vector<Type *> Params;
    std::vector<int> NamePositions;

  public:
    ImportExprAST(const std::string &LunaCodeID, const std::string &CFuncName,
                  std::vector<Type *> Params, std::vector<int> NamePositions, int position = -1)
        : LunaCodeID(LunaCodeID), CFuncName(CFuncName), Params(std::move(Params)),
          NamePositions(std::move(NamePositions)) {
        setPosition(position);
    }
    Value *codegen() override;
    std::string getText() override;
    InitStatus containsInit() override;
};

class ProgramExprAST : public ExprAST {
    std::vector<std::unique_ptr<ExprAST>> Imports;
    std::vector<std::unique_ptr<SubExprAST>> Subs;
    std::string fa_name;

  public:
    ProgramExprAST(std::string fa_name, std::vector<std::unique_ptr<ExprAST>> imports,
                   std::vector<std::unique_ptr<SubExprAST>> subs, int position = -1)
        : Imports(std::move(imports)), Subs(std::move(subs)), fa_name(fa_name) {
        setPosition(position);
    }
    Value *codegen() override;
    std::string getText() override;
    InitStatus containsInit() override;
};

class IntCastExprAST : public ExprAST {
    std::unique_ptr<ExprAST> Expr;

  public:
    IntCastExprAST(std::unique_ptr<ExprAST> Expr, int position = -1) : Expr(std::move(Expr)) {
        setPosition(position);
    }
    Value *codegen() override;
    std::string getText() override;
    InitStatus containsInit() override;
};

class RealCastExprAST : public ExprAST {
    std::unique_ptr<ExprAST> Expr;

  public:
    RealCastExprAST(std::unique_ptr<ExprAST> Expr, int position = -1) : Expr(std::move(Expr)) {
        setPosition(position);
    }
    Value *codegen() override;
    std::string getText() override;
    InitStatus containsInit() override;
};

class StringCastExprAST : public ExprAST {
    std::unique_ptr<ExprAST> Expr;

  public:
    StringCastExprAST(std::unique_ptr<ExprAST> Expr, int position = -1) : Expr(std::move(Expr)) {
        setPosition(position);
    }
    Value *codegen() override;
    std::string getText() override;
    InitStatus containsInit() override;
};

class TernaryExprAST : public ExprAST {
    std::unique_ptr<ExprAST> Cond, True, False;

  public:
    TernaryExprAST(std::unique_ptr<ExprAST> Cond, std::unique_ptr<ExprAST> True,
                   std::unique_ptr<ExprAST> False, int position = -1)
        : Cond(std::move(Cond)), True(std::move(True)), False(std::move(False)) {
        setPosition(position);
    }
    Value *codegen() override;
    std::string getText() override;
    InitStatus containsInit() override;
};
