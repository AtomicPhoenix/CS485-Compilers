#1/bin/bash

ocamlc ./main.ml

for file in ./test_cases/*; do
	./cool --type "$file"

	./a.out "$file-type"
	mv "$file-tac" ./my-out
	./cool --tac "$file"
	mv "$file-tac" ./ref-out

	diff=$(diff -U 0 "./my-out" "./ref-out" | grep -c ^@)
	if [ "$diff" -gt 0 ]; then
		printf "File %s differs in output by %d lines\n" "$file" "$diff"
	fi

	rm "$file-type"
done
