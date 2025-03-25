#!/bin/bash

g++ -std=gnu++0x main.cpp -O2 -c -o prototype.o
g++ ../Exprtk/exprtk.o prototype.o -o prototype.out
./prototype.out source.fa
rm prototype.o
rm prototype.out
