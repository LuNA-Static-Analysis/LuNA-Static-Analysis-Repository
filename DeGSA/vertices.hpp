#pragma once

#include <iostream>
#include <map>
#include <set>
#include <vector>

#include "enums.hpp"
#include "../parser/ast.hpp"
#include "ids.hpp"

// this class represents a vertex in a DDG
// vertex is a computational fragment (VF), so it could require some (or none) VFs to be ran before, and also allow other VFs to run
// also vertex can contain VFs inside, if its operator allows to have blocks (i.e subprogram, but not an import)
class Vertex {

    // this struct serves as a binding between vertices of the same level
    // it has a pointer to a vertice and a DF used or defined
    struct Binding {

    private:

        Identifier* id;
        Vertex* pointerTo;

    public:

        Binding(Vertex* pointerTo, Identifier* id);

        Identifier* getId();

        Vertex* getPointerTo();

        bool operator<(const Binding b) const;
    };

    protected:

        std::set<Binding> in; // vertices that must be ran directly before current
        std::set<Binding> out; // vertices that require directly current vertex to be ran
        std::set<Vertex*> inside; // vertices that are inside the body of a current vertex
        Vertex* parent; // vertex that has current vertex inside its block

        /* DFs that are visible inside (and declared outside) the block of this vertice, but not including ones that are declared in this block;
            has no duplicates*/
        std::map<std::string, Identifier*> declaredOutsideIdsMap;

        /* DFs that are declared in current block;
            has no duplicates */
        std::map<std::string, Identifier*> declaredInsideIdsMap;

        /* sum of previous two containers;
            has no duplicates */
        std::map<std::string, Identifier*> declaredBothIdsMap;

        std::set<Identifier*> use; // list of DFs that are used in this vertex
        std::set<Identifier*> def; // list of DFs that are defined in this vertex

        VertexType vertexType; // type of a vertex (VF type)
        std::string name; // name of a vertex ("for", "while", ..., and CF names)
        int depth; // amount of blocks that this vertex is in
        int number; // unique number of a vertice
        int line; // line in code that this operator is in
        std::string fileName;

    public:
        Vertex();

        virtual ~Vertex();

        VertexType getVertexType();

        std::string getName();

        std::string getFileName();

        std::set<Identifier*> getUseSet();

        std::set<Identifier*> getDefSet();

        std::set<Vertex*> getInsideSet();

        std::set<Binding> getInSet();

        std::set<Binding> getOutSet();

        int getDepth();

        int getNumber();

        int getLine();

        Vertex* getParent();

        void addIn(Vertex* vertex, Identifier* id);

        void addOut(Vertex* vertex, Identifier* id);

        void addInside(Vertex* vertex);

        void addUse(Identifier* id);

        void addDef(Identifier* id);

        std::map<std::string, Identifier*> getDeclaredInsideIdsMap();

        std::map<std::string, Identifier*> getDeclaredOutsideIdsMap();

        std::map<std::string, Identifier*> getDeclaredBothIdsMap();

        void setDeclaredInsideIdsMap(std::map<std::string, Identifier*> declaredInsideIdsMap);

        void setDeclaredOutsideIdsMap(std::map<std::string, Identifier*> declaredOutsideIdsMap);

        void setDeclaredBothIdsMap(std::map<std::string, Identifier*> declaredBothIdsMap);

        virtual void printInfo(std::ostream* outputTarget) = 0;

        void printCallStack(std::ostream* outputTarget);

};

class CFVertex: public Vertex {

    private:

        std::vector<Identifier*> argNames; // vector of SubArgNames/MainArgNames

    public:

        CFVertex(int depth, int number, int line,
            std::string name, VertexType vertexType, Vertex* parent, std::vector<Identifier*> argNames,
            std::string fileName);

        void printInfo(std::ostream* outputTarget);

};

class ForVertex: public Vertex {

    private:

        ForIteratorName* iterator;
        Expression* leftBorder;
        Expression* rightBorder;

    public:

        ForVertex(int depth, int number, int line,
            ForIteratorName* iterator, Expression* leftBorder, Expression* rightBorder, Vertex* parent,
            std::string fileName);

        ForIteratorName* getIterator();

        Expression* getLeftBorder();

        Expression* getRightBorder();

        void printInfo(std::ostream* outputTarget);
};

class WhileVertex: public Vertex {

    private:

        WhileIteratorName* iterator;
        Identifier* outName;
        Expression* conditionExpr;
        Expression* startExpr;

    public:

        WhileVertex(int depth, int number, int line,
            WhileIteratorName* iterator, Identifier* outName, Expression* conditionExpr, Expression* startExpr,
            Vertex* parent, std::string fileName);

        WhileIteratorName* getIterator();

        Identifier* getOutName();

        Expression* getConditionExpr();

        Expression* getStartExpr();

        void printInfo(std::ostream* outputTarget);

};

class IfVertex: public Vertex {

    private:

        Expression* conditionExpr;

    public:

        IfVertex(int depth, int number, int line,
            Expression* conditionExpr,
            Vertex* parent, std::string fileName);

        Expression* getConditionExpr();

        void printInfo(std::ostream* outputTarget);

};

class LetVertex: public Vertex {

    private:

        std::vector<LetName*>* letNamesVector;

    public:

        LetVertex(int depth, int number, int line,
            std::vector<LetName*>* letNamesVector,
            Vertex* parent, std::string fileName);

        std::vector<LetName*>* getLetNamesVector();

        void printInfo(std::ostream* outputTarget);

};
