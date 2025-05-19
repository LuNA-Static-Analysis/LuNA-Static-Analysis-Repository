#pragma once

#include "enums.hpp"
#include "ids.hpp"
#include "../../parser/ast.hpp"

#include "../symengine-0.14.0/symengine/expression.h"
#include "../symengine-0.14.0/symengine/logic.h"

#include <regex>

using Sympression = SymEngine::RCP<const SymEngine::Basic>;
//using SympressionAdd = SymEngine::RCP<const SymEngine::Add>;

// class-singleton for storing unique aliases for every identifier, including IndexedDF
class AliasTable final {
  public:

    enum OperationType {
      Equal = 0,
      NotEqual,
      Greater,
      GreaterOrEqual,
      Lesser,
      LesserOrEqual
    };

    struct ExpressionAssumption {
      public:

        ExpressionAssumption(std::string left, std::string right, OperationType operation) : leftAlias(left), rightAlias(right), operationType(operation) {};

        bool Equals(const ExpressionAssumption& other) {
          if ((operationType == Equal && other.operationType == Equal) ||
              (operationType == NotEqual && other.operationType == NotEqual)
          ){ // symmetrical
            if ((leftAlias == other.leftAlias && rightAlias == other.rightAlias) ||
                (leftAlias == other.rightAlias && rightAlias == other.leftAlias)
            ){
              return true;
            } else {
              return false;
            }
          } else { // non-symmetrical
            if (leftAlias == other.leftAlias && rightAlias == other.rightAlias)
              return true;
            else
              return false;
          }
        }

        std::string leftAlias;
        std::string rightAlias;
        OperationType operationType;
    };

    static std::optional<std::pair<std::string, Identifier*>> GetIdentifierByAlias(const std::string& iAlias);

    static std::optional<std::pair<std::string, Identifier*>> GetAliasByIdentifier(Identifier* iIdentifier);

    static std::optional<std::string> CreateAlias(Identifier* iIdentifier);

    static bool AddRule(std::string iLeft, std::string iRight, AliasTable::OperationType iOperation);
  
  private:
    static unsigned int nextNumber;
    static std::vector<std::pair<std::string, Identifier*>> table;

    // this is pretty weird architecture, but will do
    // so, we create a vector of structs leftAlias, rightAlias, opType
    // and ONLY when refining the Expression, we will actually
    // convert this to Assumptions object for a one-time use
    static std::vector<AliasTable::ExpressionAssumption> ruleSet;
};

// class that represents nodes in an AST
// required to store information about expressions in LuNA code
// terminals are Identifiers or constants
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

public:

  Sympression toSympression();

  // get a string representing this expression with only true DFs (i.e. only using base names, iterators and main args)
  std::string getAsTrueString();

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

  void markAsUse(Vertex* currentVertex, const std::vector<Expression*>& indexesExpression);

  void markAsDef(Vertex* currentVertex, const std::vector<Expression*>& indexesExpression);

  bool isIndexable();

  TrueIndexedDFData getTrueBaseNameIndexedDF(TrueIndexedDFData currentIndexedDF);

  // used for creating constants as Expressions
  Expression(std::string constant, ExpressionType expressionType, Vertex* currentVertex) :
    _ASTexpr(nullptr), _constant(constant), _identifier(nullptr), _leftExpr(nullptr), _rightExpr(nullptr), _expressionType(expressionType), _vertex(currentVertex) {};

  // used for creating names (i.e. iterators) as Expressions
  Expression(Identifier* identifier, ExpressionType expressionType, Vertex* currentVertex) :
    _ASTexpr(nullptr), _constant(""), _identifier(identifier), _leftExpr(nullptr), _rightExpr(nullptr), _expressionType(expressionType), _vertex(currentVertex) {};

  // used for anything more complex than a constant and a name
  Expression(expr* ASTexpr, std::map<std::string, Identifier*> nameTable, Vertex* currentVertex);

};
