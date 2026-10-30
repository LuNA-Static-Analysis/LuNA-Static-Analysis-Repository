#pragma once

#include "enums.hpp"
#include "ids.hpp"
#include "../../parser/ast.hpp"

#include <regex>
#include <z3++.h>

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

  static inline z3::context* pZ3Context {nullptr};
  static inline z3::solver* pZ3Solver {nullptr};

  void calculateValueType();

  void initializeZ3();

public:

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

  // this function returns all of the identifiers that are present in the tree of this expression
  // it recursively resolves intermediate identifiers (like sub args and let variables)
  // so it returns a set of only "while" and "for" iterators + DFs
  std::set<Identifier*> getAllIdentifiers();

  Vertex* getVertex() { return _vertex; };

  void markAsUse(Vertex* currentVertex, int size);

  void markAsDef(Vertex* currentVertex, int size);

  bool isIndexable();

  // used for creating constants as Expressions
  Expression(std::string constant, ExpressionType expressionType, Vertex* currentVertex) :
    _ASTexpr(nullptr), _constant(constant), _identifier(nullptr), _leftExpr(nullptr), _rightExpr(nullptr), _expressionType(expressionType), _vertex(currentVertex)
    {
      initializeZ3();
    };

  // used for creating names (i.e. iterators) as Expressions
  Expression(Identifier* identifier, ExpressionType expressionType, Vertex* currentVertex) :
    _ASTexpr(nullptr), _constant(""), _identifier(identifier), _leftExpr(nullptr), _rightExpr(nullptr), _expressionType(expressionType), _vertex(currentVertex)
    {
      initializeZ3();
    };

  // used for anything more complex than a constant and a name
  Expression(expr* ASTexpr, std::map<std::string, Identifier*> nameTable, Vertex* currentVertex);

};
