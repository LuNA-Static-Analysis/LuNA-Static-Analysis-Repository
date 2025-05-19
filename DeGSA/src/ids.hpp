#pragma once

#include "global.hpp"

#include "enums.hpp"
#include "vertices.hpp"
#include "../../parser/ast.hpp"
#include "expr.hpp"

class BaseDFName;

class IndexedDFName;
class IndexedDFValueName;
class IndexedDFAliasName;

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

    IdentifierClass getClass() const { return m_identifierClass; };

    //todo check how this works actuaaly; perhaps use const &; also returning and creating a lot if string reports is retarded
    std::set<Vertex*> getUseSet() const { return m_useSet; };

    std::set<Vertex*> getDefSet() const { return m_defSet; };

    //todo wth is this
    virtual int getLine() const;

    //void setVertex(Vertex* currentVertex) { m_vertex = currentVertex; };

    /* 
       recursively marks every referenced identifiers as used
       initializes global reports array
    */
    virtual void markAsUse(Vertex* currentVertex, const std::vector<Expression*>& indexesExpression) = 0;

    /* 
       recursively marks every referenced identifiers as defined, if it is allowed
       initializes global reports array
    */
    virtual void markAsDef(Vertex* currentVertex, const std::vector<Expression*>& indexesExpression) = 0;

    virtual bool isIndexable() const = 0;

    virtual TrueIndexedDFData getTrueBaseNameIndexedDF(TrueIndexedDFData currentIndexedDF) = 0;

    virtual std::string getAsTrueString() const = 0;

    Identifier(std::string name, Expression* reference, Vertex* vertex, IdentifierClass identifierClass, ValueType valueType)
      : m_name(name), m_reference(reference), m_vertex(vertex), m_identifierClass(identifierClass), m_valueType(valueType) {};

    virtual ~Identifier() {};
};


/* TODO MAJOR ISSUE 05.05.2025, or am I schizo, i dunno
actually there should not be just everybody inheriting from Identifier
as there are basically two types of names:
- base names, i.e. BaseDFName or MutableArgs
- value names, i.e. iterators
also there is a let name which can be whatever, and IDFs as well are pluripotent
currently there are unused code in every identifier because they simply inherit Identifier
fix this later perhaps :)
*/

// this class' objects are created at DF declaration line (not as args of a sub!)
// objects save information about every indexed and simple DF, i.e. their sizes and use/defs
class BaseDFName final: public Identifier {

private:

    // list of indexed DFs with this base name
    // it is possiblle to just get use/def info from them directly, and indices expressions as well
    //TODO think about example of mutable arg, indexedf df and let
    // basically only leafs IDFs must be created as value, and only them should be marked as use or def
    
    // creating IDF means creating it with true name already
    // either this IDF will be used directly, or indirectly
    // we can not tell for sure, as, for example, we can do:
    /*
    
    sub main() {
        df c;
        foo(c[0]);
    }
    sub foo(name a) {
        let b = a {
            print(b);//c[0], no IDF will be created, only marked Alias as used
            print(b[1]);//c[0][1], ValueIDF will be created
        }
    }
    
    */
    std::vector<IndexedDFValueName*> _indexedDFValueNames = {};
    std::vector<IndexedDFAliasName*> _indexedDFAliasNames = {};

    int _line;

public:

    std::vector<IndexedDFValueName*> getAllIndexedDFValueNames() { return _indexedDFValueNames; };

    std::vector<IndexedDFAliasName*> getAllIndexedDFAliasNames() { return _indexedDFAliasNames; };

    int getLine() { return _line; }

    void markAsUse(Vertex* currentVertex, const std::vector<Expression*>& indexesExpression) override;

    void markAsDef(Vertex* currentVertex, const std::vector<Expression*>& indexesExpression) override;

    // obviously BaseDFName is always indexable
    bool isIndexable() const override { return true; };

    virtual TrueIndexedDFData getTrueBaseNameIndexedDF(TrueIndexedDFData currentIndexedDF) override;

    virtual std::string getAsTrueString() const override {
        return m_name;
    }
    
    void AddNewIndexedDFValueName(IndexedDFValueName* indexedDF) {
        for (auto existingIDF : _indexedDFValueNames) {
            if (indexedDF == existingIDF) {
                logInternalError("tried adding duplicate IDF value name to the base name table");
                return;
            }
        }
        _indexedDFValueNames.push_back(indexedDF);
    };

    void AddNewIndexedDFAliasName(IndexedDFAliasName* indexedDF) {
        for (auto existingIDF : _indexedDFAliasNames) {
            if (indexedDF == existingIDF) {
                logInternalError("tried adding duplicate IDF alias name to the base name table");
                return;
            }
        }
        _indexedDFAliasNames.push_back(indexedDF);
    };

    BaseDFName(std::string name, Vertex* vertex, int line) : Identifier(name, nullptr, vertex, baseDFNameClass, noneType), _line(line) {};

    ~BaseDFName() {};
};


// this class' objects are created at finding any DF in an expression
// abstract class!
class IndexedDFName: public Identifier {

protected:

    // pointer to the base name
    BaseDFName* _base;

    // this array shows, at what positions (inside "[]") are what expressions
    // in this indexed DF (starting from 0)
    // if expressionsVector is empty, then it is a simple DF with no indices
    std::vector<Expression*> _expressionsVector;

    // constructor is private, creation must happen using static method
    IndexedDFName(Vertex* currentVertex, BaseDFName* trueBaseName, std::vector<Expression*> expressionsVector);

public:

    BaseDFName* getBase() { return _base; };

    std::vector<Expression*> getExpressionsVector() { return _expressionsVector; };

    /* all indexed DFs are checked at construction, so already constucted
    ones are definetely indexable*/
    bool isIndexable() const override { return true; };

    virtual std::string getAsTrueString() const override {
        std::string trueName = m_name;
        for (auto exp : _expressionsVector)
            trueName.append("[" + exp->getAsTrueString() + "]");
        return trueName;
    }

    ~IndexedDFName() {};
};


//TODO currently both these classes are identical?
// this class' objects are created at finding any DF in an expression that are used or defined, i.e. have a value
class IndexedDFValueName final: public IndexedDFName {

private:

    // constructor is private, creation must happen using static method
    IndexedDFValueName(Vertex* currentVertex, BaseDFName* trueBaseName, std::vector<Expression*> expressionsVector);

public:

    void markAsUse(Vertex* currentVertex, const std::vector<Expression*>& indexesExpression) override;

    void markAsDef(Vertex* currentVertex, const std::vector<Expression*>& indexesExpression) override;

    virtual TrueIndexedDFData getTrueBaseNameIndexedDF(TrueIndexedDFData currentIndexedDF) override;

    // it is used inside Expression constructor when encountering indexed name
    static IndexedDFValueName* TryCreateIndexedDFValueName(expr* expression, std::map<std::string, Identifier*> nameTable, int line, Vertex* currentVertex);

    ~IndexedDFValueName() {};
};



// this class' objects are created at finding any DF in an expression that are used as a base for some other IDF
class IndexedDFAliasName final: public IndexedDFName {

private:

    // constructor is private, creation must happen using static method
    IndexedDFAliasName(Vertex* currentVertex, BaseDFName* trueBaseName, std::vector<Expression*> expressionsVector);

public:

    void markAsUse(Vertex* currentVertex, const std::vector<Expression*>& indexesExpression) override;

    void markAsDef(Vertex* currentVertex, const std::vector<Expression*>& indexesExpression) override;

    virtual TrueIndexedDFData getTrueBaseNameIndexedDF(TrueIndexedDFData currentIndexedDF) override;

    static // it is used inside Expression constructor when encountering indexed name
    IndexedDFAliasName* TryCreateIndexedDFAliasName(expr* expression, std::map<std::string, Identifier*> nameTable, int line, Vertex* currentVertex);

    ~IndexedDFAliasName() {};
};



class ForIteratorName final: public Identifier {

public:

    Expression* getLeftBorder();

    Expression* getRightBorder();

    void markAsUse(Vertex* currentVertex, const std::vector<Expression*>& indexesExpression) override;

    void markAsDef(Vertex* currentVertex, const std::vector<Expression*>& indexesExpression) override;

    bool isIndexable() const override { return false; };

    virtual TrueIndexedDFData getTrueBaseNameIndexedDF(TrueIndexedDFData currentIndexedDF) override;

    virtual std::string getAsTrueString() const override {
        return m_name;
    }

    ForIteratorName(std::string name, Vertex* currentVertex) : Identifier(name, nullptr /* todo what to do here?*/, currentVertex, forIteratorNameClass, intType) {};

    ~ForIteratorName() {};
};



class WhileIteratorName final: public Identifier {

public:

    Expression* getConditionExpr();

    Expression* getStartExpr();

    void markAsUse(Vertex* currentVertex, const std::vector<Expression*>& indexesExpression) override;

    void markAsDef(Vertex* currentVertex, const std::vector<Expression*>& indexesExpression) override;

    bool isIndexable() const override { return false; };

    virtual TrueIndexedDFData getTrueBaseNameIndexedDF(TrueIndexedDFData currentIndexedDF) override;

    virtual std::string getAsTrueString() const override {
        return m_name;
    }

    WhileIteratorName(std::string name, Vertex* currentVertex) : Identifier(name, nullptr /* todo what to do here?*/, currentVertex, whileIteratorNameClass, intType) {};

    ~WhileIteratorName() {};
};



//TODO
class ValueName final: public Identifier {

private:

public:

    //std::set<std::pair<Identifier*, int>> getRoots();

};



class LetName final: public Identifier {

public:

    void markAsUse(Vertex* currentVertex, const std::vector<Expression*>& indexesExpression) override;

    void markAsDef(Vertex* currentVertex, const std::vector<Expression*>& indexesExpression) override;

    bool isIndexable() const override { return m_reference->isIndexable(); };

    virtual TrueIndexedDFData getTrueBaseNameIndexedDF(TrueIndexedDFData currentIndexedDF) override;

    virtual std::string getAsTrueString() const override {
        return m_name;
    }

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

class MutableArgName final: public Identifier {

public:

    void markAsUse(Vertex* currentVertex, const std::vector<Expression*>& indexesExpression) override;

    void markAsDef(Vertex* currentVertex, const std::vector<Expression*>& indexesExpression) override;

    bool isIndexable() const override { return true; }

    virtual TrueIndexedDFData getTrueBaseNameIndexedDF(TrueIndexedDFData currentIndexedDF) override;

    virtual std::string getAsTrueString() const override {
        return m_name;
    }

    MutableArgName(std::string name, Expression* reference, Vertex* currentVertex, ValueType valueType) : Identifier(name, reference, currentVertex, mutableArgNameClass, valueType/*todo calculate later dynamically*/) {};

    ~MutableArgName() {};
};



class ImmutableArgName final: public Identifier {

public:

    void markAsUse(Vertex* currentVertex, const std::vector<Expression*>& indexesExpression) override;

    void markAsDef(Vertex* currentVertex, const std::vector<Expression*>& indexesExpression) override;

    bool isIndexable() const override { return false; }

    virtual TrueIndexedDFData getTrueBaseNameIndexedDF(TrueIndexedDFData currentIndexedDF) override;

    virtual std::string getAsTrueString() const override {
        return m_name;
    }

    // in case of a "main()" function argument Expression reference must be nullptr
    // this will tell us that we can not predict its value
    // todo: use this philosophy everywhere else
    ImmutableArgName(std::string name, Expression* reference, Vertex* currentVertex, ValueType valueType) : Identifier(name, reference, currentVertex, immutableArgNameClass, valueType) {}

    ~ImmutableArgName() {};
};
