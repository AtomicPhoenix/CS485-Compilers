rm ../testing/test_cases/*.cl-type
for file in ../testing/test_cases/*; do
	if [[ "$file" != *".cl-type" ]]; then
		if ! grep -q "cl-type" "$file"; then
			./run.sh "$file" "$1"
			echo ""
		fi
	fi
done
rm ../testing/test_cases/*.cl-type
