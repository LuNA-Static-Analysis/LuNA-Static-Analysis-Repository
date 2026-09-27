#!/bin/bash

if [ -z "$1" ]; then
    echo "launch error: no source file path provided"
    exit 1
fi

INITIAL_DIR=$(pwd)
SRC_FILE_PATH="$1"
SRC_DIR_PATH=$(dirname "$1")

cd ..

echo -e "\nCompiling...\n"

make

echo -e "\nCompilation finished! Running...\n"

adapt --run degsa --no-cleanup $1

echo -e "Running finished!\nSee adapt_degsa_output.txt for additional info\n"

cd "$SRC_DIR_PATH"

rm -rf ".BiLangIR"
rm -rf ".luna-build"
rm -rf ".mc-analyzer"
rm -rf ".prolog-analyzer"

mv ".adapt/adapt_degsa_output.txt" "$INITIAL_DIR"

rm -rf ".adapt"