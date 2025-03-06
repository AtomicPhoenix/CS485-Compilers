./cool --parse "$1"
ocamlc main.ml
./a.out "$1-ast"
mv "$1-type" ./out-mine.txt
./cool --type "$1"
mv "$1-type" ./out-ref.txt
nvim -d ./out-mine.txt ./out-ref.txt
