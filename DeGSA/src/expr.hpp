#pragma once

#include "enums.hpp"
#include "ids.hpp"
#include "../../parser/ast.hpp"

#include "../Exprtk/exprtk.cpp"
typedef exprtk::expression<double> ExprtkExpressionDouble;
typedef exprtk::expression<int> ExprtkExpressionInt;
typedef exprtk::expression<std::string> ExprtkExpressionString;

#include <regex>

// abstract class that represents nodes in an AST
// required to store information about expressions in LuNA code
// terminals are Identifiers
class Expression {

private:

  ExpressionType _expressionType; // type of a node [operation] (i.e. add, subtract, assign, identifier, ... )
  ValueType _valueType; // type of a value, if this type can be calculated
  Expression* _leftExpr; // left operand
  Expression* _rightExpr; // right operand
  Identifier* _identifier; // nullptr if not an identifier (type will also be not "identifierNode")
  std::string _constant; // int, string, real LuNA constant
  expr* _ASTexpr;
  Vertex* _vertex;

  void calculateValueType();
  // get a string representing this expression with only true DFs (i.e. only using base names, iterators and main args)
  std::string getAsTrueString();

  ExprtkExpressionDouble* toExprkDouble();
  ExprtkExpressionInt* toExprkInt();
  ExprtkExpressionString* toExprkString();

public:

  // simplify an expression and return a simplified copy
  static Expression* simplify(const Expression* currentExpression, const Vertex* currentVertex);

  // mutually simplify two expressions and return a pair of simplified copies
  static std::pair<Expression*, Expression> mimplify(const Expression* leftExpression, const Vertex* leftVertex, const Expression* rightExpression, const Vertex* rightVertex);
  
  // compare two expressions
  static ExpressionEquality equals(const Expression* leftExpression, const Vertex* leftVertex, const Expression* rightExpression, const Vertex* rightVertex);

  // try getting a constant
  Expression calculateValue();

  ExpressionType getExpressionType() { return _expressionType; };

  ValueType getValueType() {
    if (_valueType == notCalculated)
      calculateValueType();
    return _valueType; 
  };

  expr* getASTExpr() { return _ASTexpr; };

  std::string getConstant() { return _constant; };

  Identifier* getAsIdentifier();

  Expression getAsConstant();

  Vertex* getVertex() { return _vertex; };

  void markAsUse(Vertex* currentVertex, int size);

  void markAsDef(Vertex* currentVertex, int size);

  bool isIndexable();

  // used for creating constants as Expressions
  Expression(std::string constant, ExpressionType expressionType, Vertex* currentVertex) :
    _ASTexpr(nullptr), _constant(constant), _identifier(nullptr), _leftExpr(nullptr), _rightExpr(nullptr), _expressionType(expressionType), _vertex(currentVertex) {};

  // used for creating names (i.e. iterators) as Expressions
  Expression(Identifier* identifier, ExpressionType expressionType, Vertex* currentVertex) :
    _ASTexpr(nullptr), _constant(""), _identifier(identifier), _leftExpr(nullptr), _rightExpr(nullptr), _expressionType(expressionType), _vertex(currentVertex) {};

  // used for anything more complex than a constant and a name
  Expression(expr* ASTexpr, std::map<std::string, Identifier*> nameTable, Vertex* currentVertex);

};
