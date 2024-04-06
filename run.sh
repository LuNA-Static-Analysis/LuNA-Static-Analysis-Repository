#!/bin/bash
LUNAPROG=$1
ARG1=$2
ARG2=$3
ARG3=$4
ARG4=$5

python3 ./parser/pp.py $LUNAPROG -o prepr.fa

./adapt.out prepr.fa $ARG1 $ARG2 $ARG3 $ARG4

rm prepr.fa