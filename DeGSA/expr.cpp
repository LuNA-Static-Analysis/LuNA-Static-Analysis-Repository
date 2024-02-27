#include "expr.hpp"

ExpressionType Expression::getType(){
    return noneNode;
}

expr* Expression::getExpr(){
    return this->ASTexpr;
}

Expression::Expression(expr* ASTexpr){
    this->ASTexpr = ASTexpr;
}
