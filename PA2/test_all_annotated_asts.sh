#!/bin/bash
ocamlc main.ml
for file in ./good_tests/*; do
	./cool --parse "$file"
	./cool --type "$file"
	mv "$file-type" ./output1
	./a.out "$file-ast"
	mv "$file-type" ./output2
	diff=$(diff -U 0 "./output1" "./output2" | grep -c ^@)
	if [ "$diff" -gt 0 ]; then
		printf "File %s differs in output by %d lines\n" "$file" "$diff"
	fi
	rm ./output*
	rm ./good_tests/*.cl-*
done
