#!/bin/bash

for f in ./tests/good/*.cl; do
	./cool --parse "$f"
done

for f in ./tests/bad/*.cl; do
	./cool --parse "$f"
done

for f in ./tests/bad/*-ast; do
	./a.out "$f" >/dev/null
	if [ $? -eq 0 ]; then
		echo "File $f succeeded where it should have failed"
	fi
done

for f in ./tests/good/*-ast; do
	./a.out "$f" >/dev/null
	if [ $? -eq 1 ]; then
		echo "File $f failed where it should have succeeded"
	fi
done

rm ./tests/bad/*.cl-ast
rm ./tests/good/*.cl-ast
rm ./tests/bad/*.cl-type
rm ./tests/good/*.cl-type
