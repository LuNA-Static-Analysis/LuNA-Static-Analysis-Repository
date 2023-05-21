all: bison flex analyzer

flex: lexics.l
	lex -o lex.yy.cpp lexics.l

bison: grammar.ypp
	bison -d grammar.ypp

analyzer: grammar.tab.cpp lex.yy.cpp main.cpp
	g++ -std=c++11 lex.yy.cpp grammar.tab.cpp main.cpp ast_analyzer.cpp -o a.out -O3

clean: 
	rm grammar.tab.cpp  grammar.tab.hpp lex.yy.cpp

