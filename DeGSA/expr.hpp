#pragma once

#include "enums.hpp"
#include "../parser/ast.hpp"


// abstract class that represents nodes in an AST
// required to store information about expressions in LuNA code
// TODO currently it is simply a shell for expr class
class Expression {

    private:

        ExpressionType type; // type of a node (i.e. add, subtract, assign, ... )
        expr* ASTexpr;

    public:

        ExpressionType getType();

        expr* getExpr();

        Expression(expr* ASTexpr);

};
