#include <iostream>

#include "../Exprtk/exprtk.cpp"

typedef exprtk::symbol_table<double> symbol_table_t;
typedef exprtk::expression<double>   expression_t;
typedef exprtk::parser<double>       parser_t;

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

    double x = 0;
    double y = 0;

    symbol_table_t symbol_table;
    expression_t   expression;
    parser_t       parser;

    std::string expression_string = "x * y + 3";

    symbol_table.add_variable("x",x);
    symbol_table.add_variable("y",y);

    expression.register_symbol_table(symbol_table);

    parser.compile(expression_string,expression);

    x = 1.0;
    y = 2.0;
    logd(expression.value()); // 1 * 2 + 3

    x = 3.7;
    logd(expression.value()); // 3.7 * 2 + 3

    y = -9.0;
    logd(expression.value()); // 3.7 * -9 + 3

    // 'x * -9 + 3' for x in range of [0,100) in steps of 0.0001
    /*for (x = 0.0; x < 100.0; x += 0.0001)
    {
        expression.value(); // x * -9 + 3
    }*/
    logs("ended");
}
