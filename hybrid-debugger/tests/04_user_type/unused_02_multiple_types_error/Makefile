LUNA_COMPILER = luna


all: libucodes.so


libucodes.so: jacobi.fa ucodes.cpp
	CXX_FLAGS="-O3 -flto -march=native" $(LUNA_COMPILER) -O -c jacobi.fa


clean:
	rm -f libucodes.so runfa
