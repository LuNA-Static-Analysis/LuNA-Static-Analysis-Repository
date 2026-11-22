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

    void calculateValueType();

public:

    Vertex* getVertex() const { return m_vertex; };

    Expression* getReference() const { return m_reference; };

    std::string getName() const { return m_name; };

    ValueType getValueType() {//todo perhaps should be const?
        if (m_valueType == notCalculated)
        calculateValueType();
        return m_valueType; 
    };

    std::string getValueTypeAsString() const {
        switch(m_valueType){
            case intType:
                return "int";
            case realType:
                return "real";
            case stringType:
                return "string";
            case valueType:
                return "value";
            case nameType:
                return "name";
            default:
                return "unknown";
        }
    }

    std::string getIdentifierTypeAsString() const {
        switch (m_identifierClass){
            case IdentifierClass::baseDFNameClass:
                return "base name";
            case IdentifierClass::forIteratorNameClass:
                return "for iterator";
            case IdentifierClass::immutableArgNameClass:
                return "immutable argument";
            case IdentifierClass::mutableArgNameClass:
                return "mutable argument";
            case IdentifierClass::indexedDFNameClass:
                return "indexed DF";
            case IdentifierClass::letNameClass:
                return "let variable";
            case IdentifierClass::valueNameClass:
                return "value name";
            case IdentifierClass::whileIteratorNameClass:
                return "while iterator";
            default:
                return "unknown";
        }
    }

    IdentifierClass getClass() const { return m_identifierClass; };

    //todo wth is this
    virtual int getLine() const;

    // recursively gets to the base name and marks it as used with the calculated size (indices count) and on the specified position in vertex
    virtual void markAsUse(Vertex* currentVertex, int position, int size) = 0;

    // recursively gets to the base name and marks it as defined with the calculated size (indices count) and on the specified position in vertex
    virtual void markAsDef(Vertex* currentVertex, int position, int size) = 0;

    virtual bool isIndexable() = 0;

    virtual std::set<Identifier*> getAllIdentifiers() = 0;

    Identifier(std::string name, Expression* reference, Vertex* vertex, IdentifierClass identifierClass, ValueType valueType)
      : m_name(name), m_reference(reference), m_vertex(vertex), m_identifierClass(identifierClass), m_valueType(valueType) {};

    virtual ~Identifier() {};
};

class DataIdentifier {//TODO this should be used for main args too
public:
    std::vector<std::pair<Vertex*, int>> getUseVector() { return _aUseVector; }
    std::vector<std::pair<Vertex*, int>> getDefVector() { return _aDefVector; }
protected:
    std::vector<std::pair<Vertex*, int>> _aUseVector; // vertex and position
    std::vector<std::pair<Vertex*, int>> _aDefVector; // vertex and position
};

// this class' objects are created at DF declaration line (not as args of a sub!)
// objects save information about every indexed and simple DF, i.e. their sizes and use/defs
class BaseDFName: public Identifier, public DataIdentifier {

private:

    // this map has information about what indexed/simple DFs there is and with how many indices
    // they are used and defined in what vertices
    // basically: map(int size, pair<vector use, vector def>);
    //std::map<int, std::pair<std::vector<Vertex*>*, std::vector<Vertex*>*>> _sizeToUseDefVectors = {};//todo remove, use struct
    
    struct BaseDFNameData {
        private:
            // meaning: map, where by giving a size of resulting indexed DF (if int = 0 then DF is non-indexed) we get full data about uses and defs of this indexed DF
            // value is a pair, where first is a vector of Vertices which show where (on what index = second) DF was used, and
            // second is a vector of Vertices which show where (on what index = second) DF was defined, and
            std::map<int, std::pair<
                std::vector<std::pair<Vertex*, int>>*,
                std::vector<std::pair<Vertex*, int>>*
            >> _data;
        public:

        BaseDFNameData(){};

        bool empty() const { return _data.empty(); }

        std::vector<int> getAllSizes()
        {
            std::vector<int> result{};
            for (auto entry : _data) {
                result.push_back(entry.first);
            }
            return result;
        }

        std::vector<std::pair<Vertex*, int>>* getUseVectorByIndicesSize(int iIndicesSize)
        {
            if (_data.find(iIndicesSize) != _data.end())
                return _data[iIndicesSize].first;
            else
                return nullptr;
        }

        std::vector<std::pair<Vertex*, int>>* getDefVectorByIndicesSize(int iIndicesSize)
        {
            if (_data.find(iIndicesSize) != _data.end())
                return _data[iIndicesSize].second;
            else
                return nullptr;
        }

        // ipVertex = where IDF with iIndicesSize indices was used at iArgPosition
        void addUse(Vertex* ipVertex, int iIndicesSize, int iArgPosition)//todo rules for ifs, fors and whiles!
        {
            std::vector<std::pair<Vertex*, int>>* apUseVector = getUseVectorByIndicesSize(iIndicesSize);
            if (apUseVector) {
                for (auto t : *apUseVector) {
                    if (t.first == ipVertex && t.second == iArgPosition) {
                        std::cout << "INTERNAL ERROR: baseName->addUse tried to add duplicate entry!" << std::endl;
                        return;
                    }
                }
                apUseVector->push_back({ipVertex, iArgPosition});
            } else {
                if (getDefVectorByIndicesSize(iIndicesSize)) {
                    // pair is present, create new useVector
                    _data[iIndicesSize].first = new std::vector<std::pair<Vertex*, int>>{ {ipVertex, iArgPosition} };
                } else {
                    // pair is not present, create new pair with new useVector and null defVector
                    std::vector<std::pair<Vertex*, int>>* pT = new std::vector<std::pair<Vertex*, int>>{ {ipVertex, iArgPosition} };
                    _data[iIndicesSize] = std::make_pair(pT, nullptr);
                }
            }
        }

        // ipVertex = where IDF with iIndicesSize indices was defined at iArgPosition
        void addDef(Vertex* ipVertex, int iIndicesSize, int iArgPosition)
        {
            std::vector<std::pair<Vertex*, int>>* apDefVector = getDefVectorByIndicesSize(iIndicesSize);
            if (apDefVector) {
                for (auto t : *apDefVector) {
                    if (t.first == ipVertex && t.second == iArgPosition) {
                        std::cout << "INTERNAL ERROR: baseName->addDef tried to add duplicate entry!" << std::endl;
                        return;
                    }
                }
                apDefVector->push_back({ipVertex, iArgPosition});
            } else {
                if (getUseVectorByIndicesSize(iIndicesSize)) {
                    // pair is present, create new defVector
                    _data[iIndicesSize].second = new std::vector<std::pair<Vertex*, int>>{ {ipVertex, iArgPosition} };
                } else {
                    // pair is not present, create new pair with new defVector and null useVector
                    std::vector<std::pair<Vertex*, int>>* pT = new std::vector<std::pair<Vertex*, int>>{ {ipVertex, iArgPosition} };
                    _data[iIndicesSize] = std::make_pair(nullptr, pT);
                }
            }
        }
    };

    BaseDFNameData _data{};

    int _line;

public:

    int getLine() { return _line; }

    void markAsUse(Vertex* currentVertex, int position, int size);

    void markAsDef(Vertex* currentVertex, int position, int size);

    // obviously BaseDFName is always indexable
    bool isIndexable() { return true; };

    std::set<Identifier*> getAllIdentifiers() { return { this }; };

    BaseDFName::BaseDFNameData* getData() { return &_data; };

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

    void markAsUse(Vertex* currentVertex, int position, int size);

    void markAsDef(Vertex* currentVertex, int position, int size);

    /* all indexed DFs are checked at construction, so already constucted
    ones are definetely indexable*/
    bool isIndexable();

    std::set<Identifier*> getAllIdentifiers()
    {
        std::set<Identifier*> aIdentifiers{ this };
        for (Expression* pExpression : getExpressionsVector()) {
            auto aNewIdentifiers = pExpression->getAllIdentifiers();
            aIdentifiers.insert(aNewIdentifiers.begin(), aNewIdentifiers.end());
        }
        return aIdentifiers;
    };

    //todo this is wrong, see .cpp
    IndexedDFName(std::string name, Vertex* currentVertex, Identifier* base, std::vector<Expression*> expressionsVector);

    ~IndexedDFName() {};
};

class ForIteratorName: public Identifier, public DataIdentifier {

public:

    Expression* getLeftBorder();

    Expression* getRightBorder();

    void markAsUse(Vertex* currentVertex, int position, int size);

    void markAsDef(Vertex* currentVertex, int position, int size);

    bool isIndexable() { return false; };

    std::set<Identifier*> getAllIdentifiers() { return { this }; };

    ForIteratorName(std::string name, Vertex* currentVertex) : Identifier(name, nullptr /* todo what to do here?*/, currentVertex, forIteratorNameClass, intType) {
        /*
        for i=1..2 {};
        arg 0: iterator
        arg 1: left border
        arg 2: right border
        */
        _aDefVector.push_back(std::make_pair(currentVertex, 0));
    };

    ~ForIteratorName() {};
};

class WhileIteratorName: public Identifier, public DataIdentifier {

public:

    Expression* getConditionExpr();

    Expression* getStartExpr();

    void markAsUse(Vertex* currentVertex, int position, int size);

    void markAsDef(Vertex* currentVertex, int position, int size);

    bool isIndexable() { return false; };

    std::set<Identifier*> getAllIdentifiers() { return { this }; };

    WhileIteratorName(std::string name, Vertex* currentVertex) : Identifier(name, nullptr /* todo what to do here?*/, currentVertex, whileIteratorNameClass, intType) {
        /*
        while (0 < 0), 1 = 2..out 3
        arg 0: condition
        arg 1: iterator
        arg 2: left border
        arg 3: out name
        */
        _aDefVector.push_back(std::make_pair(currentVertex, 1));
    };

    ~WhileIteratorName() {};
};

//TODO
class ValueName: public Identifier {

private:

public:

    //std::set<std::pair<Identifier*, int>> getRoots();

};


class LetName: public Identifier {
    bool m_referenced = false;

public:
    void markReferenced() { m_referenced = true; }
    bool isReferenced() const { return m_referenced; }


    void markAsUse(Vertex* currentVertex, int position, int size);

    void markAsDef(Vertex* currentVertex, int position, int size);

    bool isIndexable() { return m_reference->isIndexable(); };

    std::set<Identifier*> getAllIdentifiers() { return (m_reference ? m_reference->getAllIdentifiers() : std::set<Identifier*>{}); };

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

    void markAsUse(Vertex* currentVertex, int position, int size);

    void markAsDef(Vertex* currentVertex, int position, int size);

    bool isIndexable() { return true; }

    std::set<Identifier*> getAllIdentifiers() { return (m_reference ? m_reference->getAllIdentifiers() : std::set<Identifier*>{}); };

    MutableArgName(std::string name, Expression* reference, Vertex* currentVertex, ValueType valueType) : Identifier(name, reference, currentVertex, mutableArgNameClass, valueType/*todo calculate later dynamically*/) {};

    ~MutableArgName() {};
};

class ImmutableArgName: public Identifier {

public:

    void markAsUse(Vertex* currentVertex, int position, int size);

    void markAsDef(Vertex* currentVertex, int position, int size);

    bool isIndexable() { return false; }

    std::set<Identifier*> getAllIdentifiers() { return { this }; };

    // in case of a "main()" function argument Expression reference must be nullptr
    // this will tell us that we can not predict its value
    // todo: use this philosophy everywhere else
    ImmutableArgName(std::string name, Expression* reference, Vertex* currentVertex, ValueType valueType) : Identifier(name, reference, currentVertex, immutableArgNameClass, valueType) {}

    ~ImmutableArgName() {};
};

// it is used inside Expression constructor when encountering indexed name
IndexedDFName* parseIndexedDFExpression(expr* expression, std::map<std::string, Identifier*> nameTable, int line, Vertex* currentVertex);
