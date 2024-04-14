#!/bin/bash
OPTION=$1

lex -o ./parser/lex.yy.cpp ./parser/lexics.l 

bison -d ./parser/grammar.ypp -o ./parser/grammar.tab.cpp

g++ -std=c++20 ./parser/lex.yy.cpp ./parser/grammar.tab.cpp adapt.cpp ./ast_analyzer/threadpool/threadpool.cpp -o adapt.out -O2 

if [[ "$OPTION" != "-nocleanup" ]];
then
rm parser/grammar.tab.cpp
rm parser/grammar.tab.hpp
rm parser/lex.yy.cpp
fi