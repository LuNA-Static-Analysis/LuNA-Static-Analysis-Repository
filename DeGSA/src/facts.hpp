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

};
