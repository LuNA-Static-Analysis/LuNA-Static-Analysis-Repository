#!/bin/bash

OPTION=$1

bison -d ../parser/grammar.ypp
flex ../parser/lexics.l
python3 ../parser/pp.py luna/source.fa -o prepr.fa
g++ -std=gnu++0x lex.yy.c grammar.tab.cpp main.cpp -o degsa.out
./degsa.out prepr.fa luna/source.fa

if [[ "$OPTION" != "-nocleanup" ]];
then
rm grammar.tab.cpp
rm grammar.tab.hpp
rm lex.yy.c
rm prepr.fa
rm degsa.out
fi