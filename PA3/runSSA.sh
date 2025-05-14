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
	mv "./test-case.pre-cl-tac" "./outputs/our-output.pre-cl-tac"

	../cool --tac $TESTNAME
	mv "./test-case.cl-tac" "./outputs/ref-output.cl-tac"

	pr -m -t ./outputs/our-output.cl-tac ./outputs/our-output.pre-cl-tac ./outputs/ref-output.cl-tac
}

run "$1" "$2"
echo ""
