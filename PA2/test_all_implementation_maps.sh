#!/bin/bash
ocamlc main.ml
rm ./output* 2>/dev/null
rm ./good_tests/*.cl-* 2>/dev/null
for file in ./good_tests/*; do
	./cool --parse "$file"
	./cool --imp-map "$file"
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
