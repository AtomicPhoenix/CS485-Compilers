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

TESTNAME="$(basename "$TESTNAME" .cl-type)"
mv "./test-case.s" "./outputs/test-case.s"
gcc -static -fno-pie -ggdb -o program "./outputs/$TESTNAME".s
./program &>./outputs/our-output.txt

TESTNAME="$(basename "$TESTNAME" .s)"
rm "$TESTNAME.cl"* 2>/dev/null
rm main.cm*
if [ -z "$2" ]; then
	../cool "$1" &>./outputs/ref-output.txt
	../cool --x86 "$1"
	FILE="$(basename "$1" .cl)"
	mv "../Tests/$FILE.s" "./outputs/ref.s"
	ourLC=$(wc -l ./outputs/test-case.s | cut -d' ' -f 1)
	refLC=$(wc -l ./outputs/ref.s | cut -d' ' -f 1)
	# diff ./outputs/ref-output.txt ./outputs/our-output.txt
	diffs=$(diff -U 0 ./outputs/ref-output.txt ./outputs/our-output.txt | tail -n +3 | grep -c '^@')
	if [ "$diffs" != "0" ]; then
		printf "There are %s differences between the reference output and the actual output\n" "$diffs"
	else
		printf "The reference output matches the actual output\n"
	fi
	percentage="$(printf "%.2f" "$(echo "scale=4; ($ourLC - $refLC) / $refLC * 100" | bc)")"
	printf "We are %s percent of the size : %s v.s. %s\n" "$percentage" "$ourLC" "$refLC"
fi
# echo "----------------------------------------------------"
echo ""
