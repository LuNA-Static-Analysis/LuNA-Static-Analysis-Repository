#include <iostream>

#include "../symengine-0.14.0/symengine/expression.h"
#include "../symengine-0.14.0/symengine/logic.h"

//using SymEngine::Expression;
using SymEngine::RCP;

void logs(std::string message)
{
    std::cout << "DEBUG LOG: " << message << std::endl;
}

void logd(double message)
{
    std::cout << "DEBUG LOG: " << message << std::endl;
}

int main(){

    logs("started");

    RCP<const SymEngine::Basic> left, right;
    RCP<const SymEngine::Integer> i = SymEngine::integer(-1);
    RCP<const SymEngine::Symbol> x = SymEngine::symbol("x");
    RCP<const SymEngine::Symbol> y = SymEngine::symbol("y");
    RCP<const SymEngine::Symbol> z = SymEngine::symbol("z");

    //r = SymEngine::div(SymEngine::integer(12), pow(SymEngine::integer(195), SymEngine::div(SymEngine::integer(1), SymEngine::integer(2))));
    left = SymEngine::div(SymEngine::symbol("x"), SymEngine::symbol("y"));
    right = SymEngine::div(SymEngine::symbol("x"), SymEngine::symbol("z"));

    // equals
    RCP<const SymEngine::Basic> resultEq = Eq(x, y);
    //auto result = SymEngine::Eq(left, right);//todo
    //auto result = SymEngine::Equality(left, right);//todo
    //RCP<const SymEngine::Equality> result = SymEngine::make_rcp<SymEngine::Equality>(x, y);
    logs(resultEq->__str__());

    // not equals
    RCP<const SymEngine::Basic> resultNonEq = Ne(x, y);
    logs(resultNonEq->__str__());

    // lesser
    RCP<const SymEngine::Basic> resultLess = Lt(x, y);
    logs(resultLess->__str__());

    // lesser or equal
    RCP<const SymEngine::Basic> resultLessOrEq = Le(x, y);
    logs(resultLessOrEq->__str__());

    // greater and greater or equal must be implemented using lesser

    logs("ended");
}
