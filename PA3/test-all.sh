#1/bin/bash

ocamlc ./main.ml

corr=0
incorr=0
for file in ./test_cases/*; do
	./cool --type "$file"
	./a.out "$file-type"
	mv "$file-tac" ./my-out
	./cool --tac "$file"
	mv "$file-tac" ./ref-out

	diff=$(diff -u ./my-out ./ref-out | grep -cE '^\+')
	if [ "$diff" -gt 0 ]; then
		incorr=$((incorr + 1))
		printf "INCORRECT: File %s differs in output by %d lines\n" "$file" "$diff"
	else
		corr=$((corr + 1))
		printf "CORRECT: File %s\n" "$file"
	fi

	rm "$file-type"
done

printf "\nCorrect: %s\nIncorrect: %s\n" $corr $incorr
