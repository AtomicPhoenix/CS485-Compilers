./cool --parse "$1"
ocamlc main.ml
./a.out "$1-ast"
rm "$1"-*
