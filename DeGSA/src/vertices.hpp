#pragma once

#include <iostream>
#include <map>
#include <set>
#include <vector>

#include "global.hpp"
#include "ids.hpp"
#include "facts.hpp"

// this class represents a vertex in a DDG
// vertex is a computational fragment (VF), so it could require some (or none) VFs to be ran before, and also allow other VFs to run
// also vertex can contain VFs inside, if its operator allows to have blocks (i.e subprogram, but not an import)
class Vertex {

    // this struct serves as a binding between vertices
    // it has a pointer to a vertice and a DF used or defined
    struct Binding {

    private:

        Identifier* _identifier;
        Vertex* _pointerTo;
        Vertex* _pointerFrom;

    public:

        Binding(Vertex* pointerTo, Vertex* pointerFrom, Identifier* identifier) : _pointerTo(pointerTo), _pointerFrom(pointerFrom), _identifier(identifier) {};

        Identifier* getId() { return _identifier; };

        Vertex* getPointerTo() { return _pointerTo; };

        Vertex* getPointerFrom() { return _pointerFrom; };

        bool operator<(const Binding b) const;//todo redo this
    };

    protected:

        std::set<Binding*> m_inSet = {}; // vertices that must be ran directly before current
        std::set<Binding*> m_outSet = {}; // vertices that require directly current vertex to be ran
        std::set<Vertex*> m_insideSet = {}; // vertices that are inside the body of a current vertex

        /* DFs that are visible inside (and declared outside) the block of this vertice, but not including ones that are declared in this block;
            has no duplicates*/
        std::map<std::string, Identifier*> m_declaredOutsideIdsMap = {};

        /* DFs that are declared in current block;
            has no duplicates */
        std::map<std::string, Identifier*> m_declaredInsideIdsMap = {};

        /* sum of previous two containers;
            has no duplicates */
        std::map<std::string, Identifier*> m_declaredBothIdsMap = {};

        std::set<Identifier*> m_useSet = {}; // list of DFs that are used in this vertex
        std::set<Identifier*> m_defSet = {}; // list of DFs that are defined in this vertex

        std::set<GLeNFact*> m_facts = {}; // set of facts dictated by for, if, while

        std::string m_name; // name of a vertex ("for", "while", ..., and CF names)
        Vertex* m_parent; // vertex that has current vertex inside its block
        VertexType m_vertexType; // type of a vertex (VF type)
        int m_depth; // amount of blocks that this vertex is in
        int m_line; // line in code that this operator is in
        std::string m_fileName;
        const block* const m_block;
        const statement* m_statement;

        void printGenericInfo(std::ostream* outputTarget);

    public:
        Vertex(std::string name, Vertex* parent, VertexType vertexType, int depth, int line, std::string fileName, block* block, statement* statement, std::map<std::string, Identifier*> declaredOutsideIdsMap, std::set<GLeNFact*> facts) :
            m_name(name), m_parent(parent), m_vertexType(vertexType), m_depth(depth), m_line(line), m_fileName(fileName), m_block(block), m_statement(statement), m_declaredOutsideIdsMap(declaredOutsideIdsMap), m_declaredBothIdsMap(declaredOutsideIdsMap), m_facts(facts) {};

        virtual ~Vertex() {};

        VertexType getVertexType() { return m_vertexType; };

        std::string getName() { return m_name; };

        std::string getFileName() { return m_fileName; };

        std::set<Identifier*> getUseSet() { return m_useSet; };

        std::set<Identifier*> getDefSet() { return m_defSet; };

        std::set<Vertex*> getInsideSet() { return m_insideSet; };

        std::set<Binding*> getInSet() { return m_inSet; };

        std::set<Binding*> getOutSet() { return m_outSet; };

        int getDepth() { return m_depth; };

        int getLine() { return m_line; };

        Vertex* getParent() { return m_parent; };

        const std::map<std::string, Identifier*>& getDeclaredInsideIdsMap() { return m_declaredInsideIdsMap; };

        const std::map<std::string, Identifier*>& getDeclaredOutsideIdsMap() { return m_declaredOutsideIdsMap; };

        const std::map<std::string, Identifier*>& getDeclaredBothIdsMap() { return m_declaredBothIdsMap; };

        void bindTo(Vertex* pointerTo, Identifier* identifier);

        //todo maybe just use bindings?
        //void addUse(Identifier* identifier) { m_useSet.insert(identifier); };

        //void addDef(Identifier* identifier) { m_defSet.insert(identifier); };

        virtual void printInfo(std::ostream* outputTarget) = 0;

        void printCallStack(std::ostream* outputTarget);

        // initializeVertex must be called on a vertex (initially main() vertex). It parses imports and saves information about subs (using enterBlock());
        // for other VFs simply calls enterBlock(). initializeVertex() creats a corresponding vertex for each VF and keeps track of what DFs are use and defined and where exactly, and storing
        // this information in Use and Def sets of a vertice (this is later used in bindVertices() to fully create a graph)
        // Identifier objects (except for SubArgNames) and Expressions are being created in enterBlock(), Vertex objects are being created in initializeVertex()
        //todo redo these docs
        virtual void initializeVertex() = 0;
        void enterBlock();

        void scanForDFDecls();

        void iterateThroughBlockStatements();
        void handleSub(cf_statement* cfStatement);
        void handleImport(cf_statement* cfStatement);
        void handleFor(for_statement* forStatement);
        void handleWhile(while_statement* whileStatement);
        void handleIf(if_statement* ifStatement);
        void handleLet(let_statement* letStatement);

        void addInside(Vertex* vertex) { m_insideSet.insert(vertex); }
};

class SubVertex: public Vertex {

    private:

        std::vector<Identifier*> _arguments; // vector of MutableArgNames and ImmutableArgNames
        std::vector<Expression*> _callArgs;
        std::vector<DeclaredArg> _declaredArgs;

    public:

        SubVertex(std::string name, Vertex* parent, VertexType vertexType, int depth, int line, std::string fileName, block* block, statement* statement, std::map<std::string, Identifier*> declaredOutsideIdsMap, std::set<GLeNFact*> facts, std::vector<Expression*> callArgs, std::vector<DeclaredArg> declaredArgs) :
            Vertex(name, parent, vertexType, depth, line, fileName, block, statement, declaredOutsideIdsMap, facts), _callArgs(callArgs), _declaredArgs(declaredArgs) {};

        void printInfo(std::ostream* outputTarget);
        virtual void initializeVertex();

        void checkUnusedArgs();
};

class ImportVertex: public Vertex {

    private:

        std::vector<Identifier*> _arguments; // vector of MutableArgNames and ImmutableArgNames
        std::vector<Expression*> _callArgs;
        std::vector<DeclaredArg> _declaredArgs;

    public:

        ImportVertex(std::string name, Vertex* parent, VertexType vertexType, int depth, int line, std::string fileName, block* block, statement* statement, std::map<std::string, Identifier*> declaredOutsideIdsMap, std::set<GLeNFact*> facts, std::vector<Expression*> callArgs, std::vector<DeclaredArg> declaredArgs) :
            Vertex(name, parent, vertexType, depth, line, fileName, block, statement, declaredOutsideIdsMap, facts), _callArgs(callArgs), _declaredArgs(declaredArgs) {};

        void printInfo(std::ostream* outputTarget);
        virtual void initializeVertex();
};

class ForVertex: public Vertex {

    private:

        ForIteratorName* _iterator;
        Expression* _leftBorder;
        Expression* _rightBorder;

    public:

        ForVertex(Vertex* parent, int depth, int line, std::string fileName, block* block, statement* statement, std::map<std::string, Identifier*> declaredOutsideIdsMap, std::set<GLeNFact*> facts) :
            Vertex("for", parent, forVF, depth, line, fileName, block, statement, declaredOutsideIdsMap, facts) {};

        ForIteratorName* getIterator() { return _iterator; };

        Expression* getLeftBorder() { return _leftBorder; };

        Expression* getRightBorder() { return _rightBorder; };

        void printInfo(std::ostream* outputTarget);
        virtual void initializeVertex();
};

class WhileVertex: public Vertex {

    private:

        WhileIteratorName* _iterator;
        Identifier* _outName;
        Expression* _conditionExpr;
        Expression* _startExpr;

    public:

        WhileVertex(Vertex* parent, int depth, int line, std::string fileName, block* block, statement* statement, std::map<std::string, Identifier*> declaredOutsideIdsMap, std::set<GLeNFact*> facts) :
            Vertex("while", parent, whileVF, depth, line, fileName, block, statement, declaredOutsideIdsMap, facts) {};

        WhileIteratorName* getIterator() { return _iterator; };

        Identifier* getOutName() { return _outName; };

        Expression* getConditionExpr() { return _conditionExpr; };

        Expression* getStartExpr() { return _startExpr; };

        void printInfo(std::ostream* outputTarget);
        virtual void initializeVertex();
};

class IfVertex: public Vertex {

    private:

        Expression* _conditionExpr;

    public:

        IfVertex(Vertex* parent, int depth, int line, std::string fileName, block* block, statement* statement, std::map<std::string, Identifier*> declaredOutsideIdsMap, std::set<GLeNFact*> facts) : 
            Vertex("if", parent, ifVF, depth, line, fileName, block, statement, declaredOutsideIdsMap, facts) {};

        Expression* getConditionExpr() { return _conditionExpr; };

        void printInfo(std::ostream* outputTarget);
        virtual void initializeVertex();
};

class LetVertex: public Vertex {

    private:

        std::vector<LetName*> _letNamesVector = {};

    public:

        LetVertex(Vertex* parent, int depth, int line, std::string fileName, block* block, statement* statement, std::map<std::string, Identifier*> declaredOutsideIdsMap, std::set<GLeNFact*> facts) :
            Vertex("let", parent, letVF, depth, line, fileName, block, statement, declaredOutsideIdsMap, facts) {};

        const std::vector<LetName*>& getLetNamesVector() { return _letNamesVector; };

        //todo add method to add names

        void printInfo(std::ostream* outputTarget);
        virtual void initializeVertex();
};
