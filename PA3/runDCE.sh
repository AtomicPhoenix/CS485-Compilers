rm ../Tests/*.cl-type &>/dev/null
rm ../Tests/*.s &>/dev/null
rm ../Tests/*.cl-tac &>/dev/null

run() {
	cat lib/print.ml lib/parser.ml lib/tac.ml lib/cfg.ml lib/asm.ml lib/intrinsics.ml lib/optimizer.ml bin/main.ml |
		sed 's/Print\.//g
             s/Parser\.//g
             s/Tac\.//g
             s/Asm\.//g
             s/Cfg\.//g
             s/Optimizer\.//g
             s/Intrinsics\.//g
             /^open/d
             /PA3/d
             w ./main.ml' >main.ml

	EXTENSION=$(echo "$1" | cut -d'.' -f4)

	FILE="$1"
	if [ "$EXTENSION" = "cl" ]; then
		../cool --type "$1"
		FILE="$1-type"
	elif [ "$EXTENSION" != "cl-type" ]; then
		echo "Input a cool file, not $1"
		exit 1
	fi

	echo "Running $FILE"
	TESTNAME="./test-case.cl-type"
	cp "$FILE" $TESTNAME

	ocamlc main.ml
	./a.out "$TESTNAME"
	mv "./test-case.cl-tac" "./outputs/our-output.cl-tac"

	../cool --tac $TESTNAME
	mv "./test-case.cl-tac" "./outputs/ref-output.cl-tac"

	if [[ -n $2 ]]; then
		pr -m -t ./outputs/our-output.cl-tac ./outputs/ref-output.cl-tac
	fi

	ourLC=$(wc -l ./outputs/our-output.cl-tac | cut -d' ' -f 1)
	refLC=$(wc -l ./outputs/ref-output.cl-tac | cut -d' ' -f 1)
	percentage="$(printf "%.2f" "$(echo "scale=4; ($ourLC - $refLC)" | bc)")"

	../cool ./outputs/our-output.cl-tac &>./outputs/our-output.txt
	../cool ./outputs/ref-output.cl-tac &>./outputs/ref-output.txt

	diffs=$(diff -U 0 ./outputs/ref-output.txt ./outputs/our-output.txt | tail -n +3 | grep -c '^@')

	if [ "$diffs" != "0" ]; then
		echo ""
		printf "There are %s differences between the reference output and the actual output\n" "$diffs"
	else
		printf "The reference output matches the actual output\n"
		printf "Line Count: %s line difference : %s v.s. %s\n" "$percentage" "$ourLC" "$refLC"

	fi

	if [[ -n $2 ]]; then
		pr -m -t ./outputs/our-output.txt ./outputs/ref-output.txt
	fi

	lcSum=$(echo "$percentage + $lcSum" | bc)
	count=$((count + 1))
}

count=0
lcSum=0
run "$1" "$2"
echo ""
printf "Average LC Difference: %.2f\n" "$(echo "$lcSum / $count" | bc)"
