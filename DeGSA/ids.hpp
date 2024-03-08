#pragma once

#include "classes.hpp"

#include "enums.hpp"
#include "vertices.hpp"
#include "../parser/ast.hpp"
#include "expr.hpp"

//todo use smart pointers (shared ones)
// but there should not be much leaks already

class BaseDFName;

class Identifier {

protected:

    // todo ??? name == "" if type == subArgName; else it is valid
    std::string name;
    IdentifierType type;
    int line;
    std::set<Vertex*> useSet;//todo init
    std::set<Vertex*> defSet;//todo init

public:

    std::string getName();

    IdentifierType getType();

    std::set<Vertex*> getUseSet();

    std::set<Vertex*> getDefSet();

    virtual int getLine() = 0;

    // recursive method that returns a map of base names and amount of [] that were included in this identifier
    // ForIds, WhileIds are ignored -- we do not care if they were used or not
    // method is used to initialize use/defs of all BaseDFNames
    //todo improve readability

    // pure ( = 0) virtual method, i.e. it must be initialized in every derived class so they are not abstract
    //TODO this might be redundant, as we have markUse and markDef now
    //virtual std::set<std::pair<Identifier*, int>> getRoots() = 0;

    virtual std::vector<std::string> markAsUse(Vertex* currentVertex, int size) = 0;

    virtual std::vector<std::string> markAsDef(Vertex* currentVertex, int size) = 0;

    Identifier();

    virtual ~Identifier();

};

class SubArgName: public Identifier {

private:

    // reference is an expression that is used as a call arg mapped to current name inside a sub
    Expression* reference;

public:

    Expression* getReference();

    int getLine();

    std::vector<std::string> markAsUse(Vertex* currentVertex, int size);

    std::vector<std::string> markAsDef(Vertex* currentVertex, int size);

    SubArgName(std::string name, Expression* reference, int line);

    ~SubArgName();

};

// this class' objects are created at DF declaration line (not as args of a sub!)
// objects save information about every indexed and simple DF, i.e. their sizes and use/defs
class BaseDFName: public Identifier {

private:

    // this map has information about what indexed/simple DFs and with how many indices
    // are used and defined in what vertices
    //basically: map(int size, pair<vector use, vector def>);
    std::map<int, std::pair<std::vector<Vertex*>*, std::vector<Vertex*>*>> sizeToUseDefVectors;

public:

    std::map<int, std::pair<std::vector<Vertex*>*, std::vector<Vertex*>*>> getMap();

    int getLine();

    std::vector<std::string> markAsUse(Vertex* currentVertex, int size);

    std::vector<std::string> markAsDef(Vertex* currentVertex, int size);

    BaseDFName(std::string name, int line);

    ~BaseDFName();

};

// this class' objects are created at finding any DF in an expression
// it stores information about its base DF, but this DF could be either a BaseDFName or a LetName
class IndexedDFName: public Identifier {

private:

    // pointer to the base name
    Identifier* base;

    // this array shows, at what positions (inside "[]") are what expressions (in "ast.hpp" terms)
    // in this indexed DF (starting from 0)
    // if expressionsVector is empty, then it is a simple DF with no indices
    std::vector<Expression*> expressionsVector;

public:

    Identifier* getBase();

    std::vector<Expression*> getExpressionsVector();

    int getLine();

    std::vector<std::string> markAsUse(Vertex* currentVertex, int size);

    std::vector<std::string> markAsDef(Vertex* currentVertex, int size);

    IndexedDFName(std::string name, Identifier* base, std::vector<Expression*> expressionsVector, int line);

    ~IndexedDFName();

};

class ForIteratorName: public Identifier {

private:

    Vertex* forVertex;

public:

    int getLine();

    Expression* getLeftBorder();

    Expression* getRightBorder();

    void setVertex(Vertex* currentVertex);

    std::vector<std::string> markAsUse(Vertex* currentVertex, int size);

    std::vector<std::string> markAsDef(Vertex* currentVertex, int size);

    ForIteratorName(std::string name);

    ~ForIteratorName();

};

class WhileIteratorName: public Identifier {

private:

    Vertex* whileVertex;

public:

    Expression* getConditionExpr();

    Expression* getStartExpr();

    int getLine();

    void setVertex(Vertex* currentVertex);

    std::vector<std::string> markAsUse(Vertex* currentVertex, int size);

    std::vector<std::string> markAsDef(Vertex* currentVertex, int size);

    WhileIteratorName(std::string name);

    ~WhileIteratorName();

};

//TODO
class ValueId: public Identifier {//todo rename this

private:

public:

    int getLine();

    //std::set<std::pair<Identifier*, int>> getRoots();

};

//todo what first -- vertex or letname???
class LetName: public Identifier {

private:

    Vertex* letVertex;

    // reference shows, which expression this name was assigned
    Expression* reference;

public:

    Expression* getReference();

    int getLine();

    void setVertex(Vertex* currentVertex);

    std::vector<std::string> markAsUse(Vertex* currentVertex, int size);

    std::vector<std::string> markAsDef(Vertex* currentVertex, int size);

    LetName(std::string name, Expression* assignedExpression);

    ~LetName();

};

class MainArgName: public Identifier {

private:

    Vertex* mainVertex;

public:

    int getLine();

    void setVertex(Vertex* currentVertex);

    std::vector<std::string> markAsUse(Vertex* currentVertex, int size);

    std::vector<std::string> markAsDef(Vertex* currentVertex, int size);

    MainArgName(std::string name);

    ~MainArgName();

};

// it is used inside Expression constructor when engaging indexed name
// todo check if it works
IndexedDFName* parseIndexedDFExpression(expr* expression, std::map<std::string, Identifier*> nameTable, int line,
            std::vector<std::string>* errorReports);
