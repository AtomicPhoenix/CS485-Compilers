rm ../testing/*.cl-type &>/dev/null
for file in ../testing/*; do
	if [[ "$file" != *".cl-type" ]]; then
		if ! grep -q "cl-type" "$file"; then
			./run.sh "$file" "$1"
			echo ""
		fi
	fi
done
rm ../testing/*.cl-type &>/dev/null
