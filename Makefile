all: bison flex analyzer

prepr: $1 
	python3 ./parser/pp.py source.fa -o prepr.fa

flex: ./parser/lexics.l
	lex -o ./parser/lex.yy.cpp ./parser/lexics.l 

bison:./parser/grammar.ypp
	bison -d ./parser/grammar.ypp -o ./parser/grammar.tab.cpp

analyzer: ./parser/grammar.tab.cpp ./parser/lex.yy.cpp main.cpp
	g++ -std=c++20 ./parser/lex.yy.cpp ./parser/grammar.tab.cpp main.cpp ./ast_analyzer/threadpool/threadpool.cpp -o a.out -O3 

clean: 
	rm grammar.tab.cpp  grammar.tab.hpp lex.yy.cpp 