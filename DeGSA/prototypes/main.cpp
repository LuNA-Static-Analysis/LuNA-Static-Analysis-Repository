#include <iostream>

#include "../symengine-0.14.0/symengine/expression.h"

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

    RCP<const SymEngine::Basic> r, r1, r2;
    RCP<const SymEngine::Integer> i = SymEngine::integer(-1);
    RCP<const SymEngine::Symbol> x = SymEngine::symbol("x");
    RCP<const SymEngine::Symbol> y = SymEngine::symbol("y");
    RCP<const SymEngine::Symbol> z = SymEngine::symbol("z");

    r = SymEngine::div(SymEngine::integer(12), pow(SymEngine::integer(195), SymEngine::div(SymEngine::integer(1), SymEngine::integer(2))));
    logs(r->__str__());

    logs("ended");
}
