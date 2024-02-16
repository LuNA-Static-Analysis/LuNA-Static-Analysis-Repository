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

        BaseDFName* id;
        Vertex* pointerTo;

    public:

        Binding(Vertex* pointerTo, BaseDFName* id);

        BaseDFName* getId();

        Vertex* getPointerTo();

        bool operator<(const Binding b) const;
    };

    protected:

        std::set<Binding> in; // vertices that must be ran directly before current
        std::set<Binding> out; // vertices that require directly current vertex to be ran
        std::set<Vertex*> inside; // vertices that are inside the body of a current vertex

        /* DFs that are visible inside (and declared outside) the block of this vertice, but not including ones that are declared in this block;
            has no duplicates*/
        std::map<std::string, Identifier*> declaredOutsideIdsMap;

        /* DFs that are declared in current block;
            has no duplicates */
        std::map<std::string, Identifier*> declaredInsideIdsMap;

        /* sum of previous two containers;
            has no duplicates */
        std::map<std::string, Identifier*> declaredBothIdsMap;

        std::set<BaseDFName*> use; // list of DFs that are used in this vertex
        std::set<BaseDFName*> def; // list of DFs that are defined in this vertex

        VertexType vertexType; // type of a vertex (VF type)
        int depth; // amount of blocks that this vertex is in
        int number; // unique number of a vertice
        int line; // line in code that this operator is in

    public:
        Vertex();

        virtual ~Vertex();

        VertexType getVertexType();

        std::set<BaseDFName*> getUseSet();

        std::set<BaseDFName*> getDefSet();

        std::set<Vertex*> getInsideSet();

        std::set<Binding> getInSet();

        std::set<Binding> getOutSet();

        int getDepth();

        int getNumber();

        int getLine();

        void addIn(Vertex* vertex, BaseDFName* id);

        void addOut(Vertex* vertex, BaseDFName* id);

        void addInside(Vertex* vertex);

        void addUse(BaseDFName* id);

        void addDef(BaseDFName* id);

        std::map<std::string, Identifier*> getDeclaredInsideIdsMap();

        std::map<std::string, Identifier*> getDeclaredOutsideIdsMap();

        std::map<std::string, Identifier*> getDeclaredBothIdsMap();

        void setDeclaredInsideIdsMap(std::map<std::string, Identifier*> declaredInsideIdsMap);

        void setDeclaredOutsideIdsMap(std::map<std::string, Identifier*> declaredOutsideIdsMap);

        void setDeclaredBothIdsMap(std::map<std::string, Identifier*> declaredBothIdsMap);

        virtual void printInfo();

};

class CFVertex: public Vertex {

    private:

        std::string name; // name of an import/sub

    public:

        //todo copy constructor

        CFVertex(int depth, int number, int line,
            std::string name, VertexType vertexType);

        std::string getName();

        void printInfo();

};

class ForVertex: public Vertex {

    private:

        ForId* iterator;
        expr* leftBorder;
        expr* rightBorder;

    public:

        //todo copy constructor

        //todo check if this works
        ForVertex(int depth, int number, int line,
            ForId* iterator, expr* leftBorder, expr* rightBorder);

        ForId* getIterator();

        expr* getLeftBorder();

        expr* getRightBorder();

        void printInfo();
};