#pragma once

#include "classes.hpp"

#include "enums.hpp"
#include "vertices.hpp"
#include "../parser/ast.hpp"

//todo use smart pointers (shared ones)
// but there should not be much leaks already

class BaseDFName;

class Identifier {

protected:

    // name == "" if type == subArgName; else it is valid
    std::string name;
    IdentifierType type;

public:

    std::string getName();

    IdentifierType getType();

    // recursive method that returns a map of base names and amount of [] that were included in this identifier
    // ForIds, WhileIds are ignored -- we do not care if they were used or not
    // method is used to initialize use/defs of all BaseDFNames
    //todo improve readability
    virtual std::set<std::pair<Identifier*, int>> getRoots() = 0;//todo understand what the hell is it

    Identifier();

    virtual ~Identifier();

};

class SubArgName: public Identifier {

private:

    std::set<Identifier*> nameReferenceSet;
    // has some names
    // if only one name, then might be be initialized or could be used
    // else only used

public:

    std::set<Identifier*> getNameReferenceSet();

    std::set<std::pair<Identifier*, int>> getRoots();

    SubArgName(std::string name, std::set<Identifier*> nameReferenceSet);

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

    BaseDFName(std::string name);

    ~BaseDFName();

};

// this class' objects are created at finding any DF in an expression
// it stores information about its base DF, but this DF could be either a BaseDFName or a LetId
class IndexedDFName: public Identifier {

private:

    // pointer to the base name
    Identifier* base;

    // this array shows, at what positions (inside "[]") are what expressions (in "ast.hpp" terms)
    // in this indexed DF
    // if expressionsVector is empty, then it is a simple DF with no indices
    std::vector<expr*> expressionsVector;

public:

    Identifier* getBase();

    std::set<std::pair<Identifier*, int>> getRoots();

    std::vector<expr*> getExpressionsVector();

    IndexedDFName(std::string name, Identifier* base, std::vector<expr*> expressionsVector);

    ~IndexedDFName();

};

//TODO
class ForId: public Identifier {//todo rename this

private:

    Vertex* forVertex;
    expr* leftBorder;
    expr* rightBorder;

public:

    std::set<std::pair<Identifier*, int>> getRoots();

    ForId(std::string name, expr* leftBorder, expr* rightBorder);

    ~ForId();

};

//TODO
class WhileId: public Identifier {//todo rename this

private:

    Vertex* whileVertex;

public:

    std::set<std::pair<Identifier*, int>> getRoots();

};

//TODO
class ValueId: public Identifier {//todo rename this

private:

public:

    std::set<std::pair<Identifier*, int>> getRoots();

};

//TODO
class LetId: public Identifier {//todo rename this

private:

    Vertex* letVertex;

public:

    std::set<std::pair<Identifier*, int>> getRoots();

};
