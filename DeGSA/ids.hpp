#pragma once

#include "enums.hpp"
#include "vertices.hpp"
#include "../parser/ast.hpp"

class Id {

protected:

    std::string name;
    IdType type;

public:

    std::string getName();

    IdType getType();

    Id();

};

// this class' objects are created at DF declaration line (not as args of a sub!)
// objects save information about every indexed and simple DF, i.e. their sizes and use/defs
class BaseDFId: public Id {

private:

    // this map has information about what indexed/simple DFs and with how many indices
    // are used and defined in what vertices
    std::map<std::pair<int, UseDef>, std::vector<Vertex*>> sizeAndUseDefToVertexVector;

public:

    BaseDFId(std::string name);

};

// this class' objects are created at finding any DF in an expression
// it stores information about its base DF, but this DF could be either a BaseDFId or a LetId
class IndexedDFId: public Id {

private:

    // pointer to the base name
    Id* base;

    // this array shows, at what positions (inside "[]") are what expressions (in "ast.hpp" terms)
    // in this indexed DF
    // if expressionsVector is empty, then it is a simple DF with no indices
    std::vector<expr*> expressionsVector;

public:

    IndexedDFId(std::string name, Id* base, std::vector<expr*> expressionsVector);

};

//TODO
class ForId: public Id {

private:

    Vertex* forVertex;

};

//TODO
class WhileId: public Id {

private:

    Vertex* whileVertex;

};

//TODO
class ValueId: public Id {

private:

};

//TODO
class LetId: public Id {

private:

    Vertex* letVertex;

};