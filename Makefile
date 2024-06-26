all: bison flex analyzer

flex: ./parser/lexics.l
	lex -o ./parser/lex.yy.cpp ./parser/lexics.l 

bison:./parser/grammar.ypp
	bison -d ./parser/grammar.ypp -o ./parser/grammar.tab.cpp

analyzer: ./parser/grammar.tab.cpp ./parser/lex.yy.cpp adapt.cpp
	g++ -std=c++20 ./parser/lex.yy.cpp ./parser/grammar.tab.cpp adapt.cpp ./ast_analyzer/threadpool/threadpool.cpp -o ./bin/ast-ddg-analyzer -O2

clean:
	rm grammar.tab.cpp  grammar.tab.hpp lex.yy.cpp 