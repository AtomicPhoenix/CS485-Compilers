#1/bin/bash

ocamlc ./main.ml

for file in ./test_cases/*; do
	./cool --type "$file"
	./a.out "$file-type"
	mv "$file-tac" ./my-out
	./cool --tac "$file"
	mv "$file-tac" ./ref-out

	diff=$(diff -u ./my-out ./ref-out | grep -cE '^\+')
	if [ "$diff" -gt 0 ]; then
		printf "File %s differs in output by %d lines\n" "$file" "$diff"
	else
		printf "File %s is correct!\n" "$file"
	fi

	rm "$file-type"
done
