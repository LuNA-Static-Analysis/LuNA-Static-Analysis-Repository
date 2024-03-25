#!/bin/bash
LUNAPROG=$1

python3 ./parser/pp.py $LUNAPROG -o prepr.fa

./a.out prepr.fa