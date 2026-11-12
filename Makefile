all: bison flex analyzer

flex: ./parser/lexics.l
	lex -o ./parser/lex.yy.cpp ./parser/lexics.l 

bison:./parser/grammar.ypp
	bison -d ./parser/grammar.ypp -o ./parser/grammar.tab.cpp

analyzer: ./parser/grammar.tab.cpp ./parser/lex.yy.cpp adapt.cpp ./parser/globals.cpp
	g++ -std=c++20 ./parser/lex.yy.cpp ./parser/grammar.tab.cpp adapt.cpp ./parser/globals.cpp ./ast_analyzer/threadpool/threadpool.cpp -o ./bin/ast-ddg-analyzer -O2

test-prolog-analyzer:
	python3 prolog-analyzer/tests/runner.py prolog-analyzer/tests/

test-hybrid-debugger:
	python3 hybrid-debugger/tests/runner.py hybrid-debugger/tests/

test: test-prolog-analyzer test-hybrid-debugger

fftest-prolog-analyzer:
	python3 prolog-analyzer/tests/runner.py --first-fail prolog-analyzer/tests/

fftest-hybrid-debugger:
	python3 hybrid-debugger/tests/runner.py --first-fail hybrid-debugger/tests/

fftest: fftest-prolog-analyzer fftest-hybrid-debugger

clean:
	rm grammar.tab.cpp  grammar.tab.hpp lex.yy.cpp 