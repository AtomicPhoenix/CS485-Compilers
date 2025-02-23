#!/bin/bash

for f in ./tests/*.cl; do
	./cool --parse "$f"
done

for f in ./tests/asts/*-ast; do
	./a.out "$f"
done
