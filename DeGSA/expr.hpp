#pragma once

#include "enums.hpp"
#include "ids.hpp"
#include "../parser/ast.hpp"

#include <regex>

// abstract class that represents nodes in an AST
// required to store information about expressions in LuNA code
// terminals are Identifiers
class Expression {

    private:

        ExpressionType type; // type of a node [operation] (i.e. add, subtract, assign, identifier, ... )
        Expression* leftExpr; // left operand
        Expression* rightExpr; // right operand
        Expression* ternaryOperatorCondition; // extra expression serving as ternary operator condition
        Identifier* identifier; // nullptr if not an identifier (type will also be not "identifierNode")
        std::string constant; // int, string, real LuNA constant
        expr* ASTexpr;

    public:

        ExpressionType getType();

        expr* getExpr();

        std::string getConstant();

        Expression binOp();

        Expression terOp();

        Identifier* getAsIdentifier();

        Expression getAsConstant();

        std::vector<std::string> markAsUse(Vertex* currentVertex, int size);

        std::vector<std::string> markAsDef(Vertex* currentVertex, int size);

        Expression(std::string constant, ExpressionType type);

        Expression(expr* ASTexpr);

        Expression(expr* ASTexpr, std::map<std::string, Identifier*> nameTable, std::vector<std::string>* errorReports);

};
