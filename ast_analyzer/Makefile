all: bison flex analyzer

flex: ../parser/lexics.l
	lex -o lex.yy.cpp ../parser/lexics.l

bison:../parser/grammar.ypp
	bison -d ../parser/grammar.ypp

analyzer: grammar.tab.cpp lex.yy.cpp main.cpp
	g++ -std=c++17 lex.yy.cpp grammar.tab.cpp main.cpp ./threadpool/threadpool.cpp -o a.out -O3 

clean: 
	rm grammar.tab.cpp  grammar.tab.hpp lex.yy.cpp 

