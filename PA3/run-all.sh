for file in ../testing/test_cases/*; do
	./run.sh "$file"
	echo ""
done

rm "*.s"
