#1/bin/bash

ocamlc ./main.ml

./cool --type "$1"

./a.out "$1-type"
mv "$1-tac" ./my-out
./cool --tac "$1"
mv "$1-tac" ./ref-out

nvim -d ./my-out ./ref-out

rm "$1-type"
