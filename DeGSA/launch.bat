win_flex_bison\win_bison.exe -d grammar.ypp
win_flex_bison\win_flex.exe lexics.l
py pp.py source.fa -o prepr.fa
g++ -std=gnu++0x lex.yy.c grammar.tab.cpp main.cpp -o a.out
del grammar.tab.cpp
::del grammar.tab.hpp
del lex.yy.c
.\a.out prepr.fa
del prepr.fa
del a.out
::TODO use /parser/, ask why obsolete lexics.c is valid, but new is not