#!/bin/bash
rm debug_launch_degsa_output.txt

bison -d ../parser/grammar.ypp
flex ../parser/lexics.l

python3 ../parser/pp.py luna/launchable/source.fa -o prepr.fa
g++ lex.yy.c -c -o lex.yy.o
g++ grammar.tab.cpp -c -o grammar.tab.o
g++ -std=gnu++0x src/main.cpp -O2 -c -o degsa.o
g++ Exprtk/exprtk.o lex.yy.o grammar.tab.o degsa.o -o degsa.out
./degsa.out prepr.fa luna/launchable/source.fa

rm grammar.tab.cpp
rm grammar.tab.hpp
rm degsa.o
rm grammar.tab.o
rm lex.yy.o
rm lex.yy.c
rm prepr.fa
rm degsa.out