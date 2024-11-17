#!/bin/bash
rm debug_launch_degsa_output.txt

bison -d ../parser/grammar.ypp
flex ../parser/lexics.l

python3 ../parser/pp.py luna/tests/syn/syn6_2.fa -o prepr.fa
g++ -std=gnu++0x lex.yy.c grammar.tab.cpp src/main.cpp -O2 -o degsa.out
./degsa.out prepr.fa luna/tests/syn/syn6_2.fa

rm grammar.tab.cpp
rm grammar.tab.hpp
rm lex.yy.c
rm prepr.fa
rm degsa.out