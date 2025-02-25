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
		echo "$file FAILED where it should have SUCCEEDED"
	fi
        OUTPUTDATA=$(cat "$file-ref")
        OUTPUTDATA2=$(cat "$file-type")
        OUTPUTDIFF=$(diff "$file-ref" "$file-type")


	if [ "$OUTPUT" != "$OUTPUT2" ]; then
		echo "$file's output is incorrect"
                echo $OUTPUT2
	fi
        if [ "$OUTPUTDIFF" ]; then
            echo "$file has wrong output file"
        fi
done

rm ./good_tests/*.cl-*
rm ./bad_tests/*.cl-*
