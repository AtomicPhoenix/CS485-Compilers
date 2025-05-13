#!/bin/bash

file="./in_files.txt"

while IFS= read -r line; do
	# Process the line here
	echo "Line: $line"
	mv "./Tests/$line" "./Tests/in-$line"
done <"$file"
#
# for file in "$1"/in-*; do
# 	# Get the new file name by removing "in-"
# 	base_name=$(basename "$file")
# 	new_name="${base_name#in-}"
#
# 	# Move (rename) the file
# 	mv "$file" "$1/$new_name"
# done
