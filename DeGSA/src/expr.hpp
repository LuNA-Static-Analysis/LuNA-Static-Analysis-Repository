#pragma once

#include "enums.hpp"
#include "ids.hpp"
#include "../../parser/ast.hpp"

#include <regex>

// abstract class that represents nodes in an AST
// required to store information about expressions in LuNA code
// terminals are Identifiers
class Expression {

    private:

        ExpressionType _type; // type of a node [operation] (i.e. add, subtract, assign, identifier, ... )
        Expression* _leftExpr; // left operand
        Expression* _rightExpr; // right operand
        Identifier* _identifier; // nullptr if not an identifier (type will also be not "identifierNode")
        std::string _constant; // int, string, real LuNA constant
        expr* _ASTexpr;
        Vertex* _vertex;

    public:

        ExpressionType getType() { return _type; };

        expr* getASTExpr() { return _ASTexpr; };

        std::string getConstant() { return _constant; };

        Expression binOp(); //todo rename

        Identifier* getAsIdentifier();

        Expression getAsConstant();

        Vertex* getVertex() { return _vertex; };

        void markAsUse(Vertex* currentVertex, int size);

        void markAsDef(Vertex* currentVertex, int size);

        bool isIndexable();

        Expression(std::string constant, ExpressionType type, Vertex* currentVertex) :
          _ASTexpr(nullptr), _constant(constant), _identifier(nullptr), _leftExpr(nullptr), _rightExpr(nullptr), _type(type), _vertex(currentVertex) {};

        Expression(expr* ASTexpr, std::map<std::string, Identifier*> nameTable, Vertex* currentVertex);//todo

};
