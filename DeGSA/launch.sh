#!/bin/sh
bison -d ../parser/grammar.ypp
flex ../parser/lexics.l
python3 ../parser/pp.py source.fa -o prepr.fa
g++ -std=gnu++0x lex.yy.c grammar.tab.cpp main.cpp -o a.out
rm grammar.tab.cpp
#rm grammar.tab.hpp
rm lex.yy.c
./a.out prepr.fa
rm prepr.fa
rm a.out