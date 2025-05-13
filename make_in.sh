#!/bin/bash

file="./in_files.txt"

while IFS= read -r line; do
	# Process the line here
	echo "Line: $line"
	mv "$line.cl" "in-$line.cl"
done <"$file"
