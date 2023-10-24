bison -d ..\parser\grammar.ypp
flex ..\parser\lexics.l
py ..\parse\pp.py source.fa -o prepr.fa
g++ -std=gnu++0x lex.yy.c grammar.tab.cpp main.cpp -o a.out
del grammar.tab.cpp
::del grammar.tab.hpp
del lex.yy.c
.\a.out prepr.fa
del prepr.fa
del a.out