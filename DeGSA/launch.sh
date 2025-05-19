#!/bin/bash
rm debug_launch_degsa_output.txt

bison -d ../parser/grammar.ypp
flex ../parser/lexics.l

python3 ../parser/pp.py luna/launchable/source.fa -o prepr.fa
g++ lex.yy.c -c -o lex.yy.o
g++ grammar.tab.cpp -c -o grammar.tab.o
g++ -std=c++20 src/main.cpp -O2 -c -o degsa.o
g++ -std=c++20 symengine-0.14.0/build/symengine/libsymengine.a lex.yy.o grammar.tab.o degsa.o -lsymengine -lgmp -o degsa.out
./degsa.out prepr.fa luna/launchable/source.fa

rm grammar.tab.cpp
rm grammar.tab.hpp
rm degsa.o
rm grammar.tab.o
rm lex.yy.o
rm lex.yy.c
rm prepr.fa
rm degsa.out