#pragma once

#include "enums.hpp"

class Expression;

struct GLeNFact {

public:

    ExpressionType _type;
    Expression* _left;
    Expression* _right;

    GLeNFact(ExpressionType type, Expression* left, Expression* right) :
          _type(type), _left(left), _right(right) {};

    //todo the idea:
    //in the expression class z3 will be integrated
    //there will be context + glenfacts (restrictions) set in the expression as static members
    //basically glenfacts will provide z3 restrictions for context
    //then also a method will be added for expression -- evaluate its equality to another expression using z3 and all current available context
    //available context boils down to:
    // 1. current callstack (active glenfacts) -- whiles, ifs, fors
    // 2. all known information from the Identifier-Expression tree, so that is basically using information dependencies
};
