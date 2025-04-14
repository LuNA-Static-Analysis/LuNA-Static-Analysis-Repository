#!/bin/bash

g++ -std=gnu++0x main.cpp -O2 -c -o prototype.o
g++ ../symengine-0.14.0/build/symengine/libsymengine.a prototype.o -lsymengine -lgmp -o prototype.out
#g++ -std=gnu++0x main.cpp ../symengine-0.14.0/build/symengine/libsymengine.a prototype.o -O2 -o prototype.out
./prototype.out source.fa
rm prototype.o
rm prototype.out
