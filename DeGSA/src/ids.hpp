#pragma once

#include "global.hpp"

#include "enums.hpp"
#include "vertices.hpp"
#include "../../parser/ast.hpp"
#include "expr.hpp"

class BaseDFName;

class Identifier {

protected:

    std::string m_name;
    Expression* m_reference; // expression that is assigned to this variable (if possible)
    Vertex* m_vertex; // vertex where name was declared
    IdentifierClass m_identifierClass;
    ValueType m_valueType;
    std::set<Vertex*> m_useSet = {};
    std::set<Vertex*> m_defSet = {};

    void calculateValueType();

public:

    Vertex* getVertex() { return m_vertex; };

    Expression* getReference() { return m_reference; };

    std::string getName() { return m_name; };

    ValueType getValueType() {
        if (m_valueType == notCalculated)
        calculateValueType();
        return m_valueType; 
    };

    IdentifierClass getClass() { return m_identifierClass; };

    //todo check how this works actuaaly; perhaps use const &; also returning and creating a lot if string reports is retarded
    std::set<Vertex*> getUseSet() { return m_useSet; };

    std::set<Vertex*> getDefSet() { return m_defSet; };

    //todo wth is this
    virtual int getLine();

    //void setVertex(Vertex* currentVertex) { m_vertex = currentVertex; };

    /* 
       recursively marks every referenced identifiers as used
       initializes global reports array
    */
    virtual void markAsUse(Vertex* currentVertex, int size) = 0;

    /* 
       recursively marks every referenced identifiers as defined, if it is allowed
       initializes global reports array
    */
    virtual void markAsDef(Vertex* currentVertex, int size) = 0;

    virtual bool isIndexable() = 0;

    Identifier(std::string name, Expression* reference, Vertex* vertex, IdentifierClass identifierClass, ValueType valueType)
      : m_name(name), m_reference(reference), m_vertex(vertex), m_identifierClass(identifierClass), m_valueType(valueType) {};

    virtual ~Identifier() {};
};

// this class' objects are created at DF declaration line (not as args of a sub!)
// objects save information about every indexed and simple DF, i.e. their sizes and use/defs
class BaseDFName: public Identifier {

private:

    // this map has information about what indexed/simple DFs there is and with how many indices
    // they are used and defined in what vertices
    // basically: map(int size, pair<vector use, vector def>);
    std::map<int, std::pair<std::vector<Vertex*>*, std::vector<Vertex*>*>> _sizeToUseDefVectors = {};
    int _line;

public:

    std::map<int, std::pair<std::vector<Vertex*>*, std::vector<Vertex*>*>> getMap() { return _sizeToUseDefVectors; };

    int getLine() { return _line; }

    void markAsUse(Vertex* currentVertex, int size);

    void markAsDef(Vertex* currentVertex, int size);

    // obviously BaseDFName is always indexable
    bool isIndexable() { return true; };

    BaseDFName(std::string name, Vertex* vertex, int line) : Identifier(name, nullptr, vertex, baseDFNameClass, noneType), _line(line) {};

    ~BaseDFName() {};
};

// this class' objects are created at finding any DF in an expression
// it stores information about its base DF, but this DF could be either a BaseDFName or a LetName
class IndexedDFName: public Identifier {

private:

    // pointer to the base name
    Identifier* _base;

    // this array shows, at what positions (inside "[]") are what expressions (in "ast.hpp" terms)
    // in this indexed DF (starting from 0)
    // if expressionsVector is empty, then it is a simple DF with no indices
    std::vector<Expression*> _expressionsVector;

public:

    Identifier* getBase() { return _base; };

    std::vector<Expression*> getExpressionsVector() { return _expressionsVector; };

    void markAsUse(Vertex* currentVertex, int size);

    void markAsDef(Vertex* currentVertex, int size);

    /* all indexed DFs are checked at construction, so already constucted
    ones are definetely indexable*/
    bool isIndexable();

    //todo this is wrong, see .cpp
    IndexedDFName(std::string name, Vertex* currentVertex, Identifier* base, std::vector<Expression*> expressionsVector);

    ~IndexedDFName() {};
};

class ForIteratorName: public Identifier {

public:

    Expression* getLeftBorder();

    Expression* getRightBorder();

    void markAsUse(Vertex* currentVertex, int size);

    void markAsDef(Vertex* currentVertex, int size);

    bool isIndexable() { return false; };

    ForIteratorName(std::string name, Vertex* currentVertex) : Identifier(name, nullptr /* todo what to do here?*/, currentVertex, forIteratorNameClass, intType) {};

    ~ForIteratorName() {};
};

class WhileIteratorName: public Identifier {

public:

    Expression* getConditionExpr();

    Expression* getStartExpr();

    void markAsUse(Vertex* currentVertex, int size);

    void markAsDef(Vertex* currentVertex, int size);

    bool isIndexable() { return false; };

    WhileIteratorName(std::string name, Vertex* currentVertex) : Identifier(name, nullptr /* todo what to do here?*/, currentVertex, whileIteratorNameClass, intType) {};

    ~WhileIteratorName() {};
};

//TODO
class ValueName: public Identifier {

private:

public:

    //std::set<std::pair<Identifier*, int>> getRoots();

};


class LetName: public Identifier {

public:

    void markAsUse(Vertex* currentVertex, int size);

    void markAsDef(Vertex* currentVertex, int size);

    bool isIndexable() { return m_reference->isIndexable(); };

    LetName(std::string name, Expression* reference, Vertex* currentVertex) : Identifier(name, reference, currentVertex, letNameClass, notCalculated) {};

    ~LetName() {};
};


/*
1. mutableargname (don't have type (so just "name"), mutable, indexed DF basically)
   it will have a pointer to an expression, which is used as an actual argument;
   this is necessary, as arguments could be unsuitable -- so, this is a type error:
   we can not assign expressions and immutable variables to "name" arguments
   is indexable in all situations whenever it is used properly

2. immutableargname (have type ("int", "string"), immutable) -- also will have a pointer to an expression
   it's never indexable!
*/

class MutableArgName: public Identifier {

public:

    void markAsUse(Vertex* currentVertex, int size);

    void markAsDef(Vertex* currentVertex, int size);

    bool isIndexable() { return true; }

    MutableArgName(std::string name, Expression* reference, Vertex* currentVertex, ValueType valueType) : Identifier(name, reference, currentVertex, mutableArgNameClass, valueType/*todo calculate later dynamically*/) {};

    ~MutableArgName() {};
};

class ImmutableArgName: public Identifier {

public:

    void markAsUse(Vertex* currentVertex, int size);

    void markAsDef(Vertex* currentVertex, int size);

    bool isIndexable() { return false; }

    // in case of a "main()" function argument Expression reference must be nullptr
    // this will tell us that we can not predict its value
    // todo: use this philosophy everywhere else
    ImmutableArgName(std::string name, Expression* reference, Vertex* currentVertex, ValueType valueType) : Identifier(name, reference, currentVertex, immutableArgNameClass, valueType) {}

    ~ImmutableArgName() {};
};

// it is used inside Expression constructor when encountering indexed name
IndexedDFName* parseIndexedDFExpression(expr* expression, std::map<std::string, Identifier*> nameTable, int line, Vertex* currentVertex);
