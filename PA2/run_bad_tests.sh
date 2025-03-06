#!/bin/bash

for file in ./bad_tests/*; do
	printf "Testing %s:\n" "$file"
	./cool "$file"
	./run_single_test.sh "$file"
	echo ""
done
