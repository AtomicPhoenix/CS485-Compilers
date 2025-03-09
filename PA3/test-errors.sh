#!/bin/bash

for file in ./PA3c1/*; do
	# Extract the base name of the file (without extension)
	base_name="${file%.*}"
	# Extract file extension
	extension="${file##*.}"

	# Check if the file is a .cl file and if there's a corresponding .cl-input file
	if [ "$extension" == "cl" ] && [ ! -f "$base_name.cl-input" ]; then
		# Run the .cl file with the 'cool' program
		# echo "Running: $file"
		output=$(./cool "$file" | grep -i "ERROR")
		if [ -n "$output" ]; then
			printf "%s\n" "$file: $output"
		fi
		# else
		# Debugging output: if the file has a matching .cl-input file
		# if [ -f "$base_name.cl-input" ]; then
		# echo "Skipping: $file (corresponding .cl-input file exists)"
		# fi
	fi
done
