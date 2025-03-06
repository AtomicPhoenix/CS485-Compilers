#!/bin/bash
ocamlc main.ml
for file in ./bad_tests/*.cl; do
	./cool --parse "$file"
	ERR="$(./cool "$file")"
	ERR2="$(./a.out "$file-ast")"

	if [ $? -eq 0 ]; then
		echo "$file-ast SUCCEEDED where it should have FAILED"
	fi

	if [ "${ERR:0:10}" != "${ERR2:0:10}" ]; then
		printf "$file-ast's output is incorrect:\n\t%s\n\t%s\n\n" "$ERR" "$ERR2"
	fi
done

for file in ./good_tests/*.cl; do
	./cool --parse "$file"
	OUTPUT=$(./cool --class-map "$file")
	mv "$file-type" "$file-ref"
	OUTPUT2=$(./a.out "$file-ast")
	if [ $? -ne 0 ]; then
		printf "%s FAILED where it should have SUCCEEDED" "$file"
		if [ "$OUTPUT" != "$OUTPUT2" ]; then
			printf ": Output is incorrect:\n\t%s\n\n" "$OUTPUT2"
		fi
	else
		OUTPUTDIFF=$(diff "$file-ref" "$file-type")
		if [ "$OUTPUTDIFF" ]; then
			echo "$file has wrong output file"
			#		echo "$OUTPUTDIFF"
		fi
	fi

	# OUTPUTDATA=$(cat "$file-ref")
	# OUTPUTDATA2=$(cat "$file-type")

done

rm ./good_tests/*.cl-*
rm ./bad_tests/*.cl-*
