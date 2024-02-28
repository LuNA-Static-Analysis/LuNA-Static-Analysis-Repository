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

public:

    std::string getName();

    IdentifierType getType();

    virtual int getLine() = 0;

    // recursive method that returns a map of base names and amount of [] that were included in this identifier
    // ForIds, WhileIds are ignored -- we do not care if they were used or not
    // method is used to initialize use/defs of all BaseDFNames
    //todo improve readability

    // pure ( = 0) virtual method, i.e. it must be initialized in every derived class so they are not abstract
    virtual std::set<std::pair<Identifier*, int>> getRoots() = 0;

    Identifier();

    virtual ~Identifier();

};

class SubArgName: public Identifier {

private:

    std::set<Identifier*> nameReferenceSet;
    // has some names
    // if only one name, then might be be initialized or could be used
    // else only used
    // TODO redo to use Expression

public:

    std::set<Identifier*> getNameReferenceSet();

    std::set<std::pair<Identifier*, int>> getRoots();

    int getLine();

    SubArgName(std::string name, std::set<Identifier*> nameReferenceSet, int line);

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

    //todo check is this method allows duplicates
    void addUse(int size, Vertex* vertex);

    //todo check is this method allows duplicates
    void addDef(int size, Vertex* vertex);

    std::map<int, std::pair<std::vector<Vertex*>*, std::vector<Vertex*>*>> getMap();

    std::set<std::pair<Identifier*, int>> getRoots();

    int getLine();

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
    // in this indexed DF
    // if expressionsVector is empty, then it is a simple DF with no indices
    std::vector<Expression*> expressionsVector;

public:

    Identifier* getBase();

    std::vector<Expression*> getExpressionsVector();

    std::set<std::pair<Identifier*, int>> getRoots();

    int getLine();

    IndexedDFName(std::string name, Identifier* base, std::vector<Expression*> expressionsVector, int line);

    ~IndexedDFName();

};

class ForIteratorName: public Identifier {

private:

    Vertex* forVertex;
    expr* leftBorder;
    expr* rightBorder;

public:

    std::set<std::pair<Identifier*, int>> getRoots();

    int getLine();

    expr* getLeftBorder();

    expr* getRightBorder();

    ForIteratorName(std::string name, expr* leftBorder, expr* rightBorder);

    ~ForIteratorName();

};

class WhileIteratorName: public Identifier {

private:

    Vertex* whileVertex;
    expr* conditionExpr;
    expr* startExpr;

public:

    expr* getConditionExpr();

    expr* getStartExpr();

    std::set<std::pair<Identifier*, int>> getRoots();

    int getLine();

    WhileIteratorName(std::string name, expr* conditionExpr, expr* startExpr);

    ~WhileIteratorName();

};

//TODO
class ValueId: public Identifier {//todo rename this

private:

public:

    //std::set<std::pair<Identifier*, int>> getRoots();

};

class LetName: public Identifier {

private:

    Vertex* letVertex;
    expr* assignedExpr;

    std::set<Identifier*> nameReferenceSet;
    // has some names
    // if only one name, then might be be initialized or could be used
    // else only used
    // TODO redo to use Expression

public:

    int getLine();

    expr* getAssignedExpr();

    std::set<Identifier*> getNameReferenceSet();

    std::set<std::pair<Identifier*, int>> getRoots();

    LetName(std::string name, expr* assignedExpression, std::set<Identifier*> nameReferenceSet);

    ~LetName();

};

class MainArgName: public Identifier {

public:

    std::set<std::pair<Identifier*, int>> getRoots();

    int getLine();

    MainArgName(std::string name);

    ~MainArgName();

};

IndexedDFName* parseIndexedDFExpression(expr* expression, std::map<std::string, Identifier*> nameTable, int line,
            std::vector<std::string>* errorReports);