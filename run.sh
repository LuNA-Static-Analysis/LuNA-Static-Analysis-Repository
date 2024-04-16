#!/bin/bash
LUNAPROG=$1
ARG1=$2
ARG2=$3
ARG3=$4
ARG4=$5

python3 ./parser/pp.py $LUNAPROG -o prepr.fa

rm ./reporter/found_errors.json

./adapt.out prepr.fa $LUNAPROG $ARG1 $ARG2 $ARG3 $ARG4 1>/dev/null 2>/dev/null || exit 1

PROLOG_ANALYZER_HOME=./prolog-analyzer
$PROLOG_ANALYZER_HOME/bin/prolog-analyzer \
  --project-dir="$(dirname "$LUNAPROG")" \
  --errors-file=./reporter/found_errors.json \
  $LUNAPROG \
  # TODO check args and set if needed
  # --no-cleanup \
   1>/dev/null \
   || exit 1

cd ./reporter

python3 adapt_output_generator.py
cat ./adapt_output.txt

cd ..

rm prepr.fa