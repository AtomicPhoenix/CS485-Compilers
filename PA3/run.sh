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
mv "./test-case.cl-tac" "./outputs/our-tac.cl-tac"
TESTNAME="$(basename "$TESTNAME" .cl-type)"
mv "./test-case.s" "./outputs/our-output.s"
gcc -static -fno-pie -ggdb -o program "./outputs/our-output.s"
mv "./program" "./outputs/our-program"
./outputs/our-program &>./outputs/our-output.txt

TESTNAME="$(basename "$TESTNAME" .s)"
rm "$TESTNAME.cl"* 2>/dev/null
rm main.cm*

../cool "$1" &>./outputs/ref-output.txt
../cool --tac "$1"
FILE="$(basename "$1" .cl)"
mv "../Tests/$FILE.cl-tac" "./outputs/ref-tac.cl-tac"

grep -v "^comment" "./outputs/our-tac.cl-tac" >"./new_output.tac"
mv "./new_output.tac" "./outputs/our-tac.cl-tac"
ourLC=$(wc -l ./outputs/our-tac.cl-tac | cut -d' ' -f 1)
grep -v "^comment" "./outputs/ref-tac.cl-tac" >"./new_output.tac"
mv "./new_output.tac" "./outputs/ref-tac.cl-tac"
refLC=$(wc -l ./outputs/ref-tac.cl-tac | cut -d' ' -f 1)

../cool --x86 "$1"
FILE="$(basename "$1" .cl)"
mv "../Tests/$FILE.s" "./outputs/ref.s"
gcc -static -fno-pie -ggdb -o ref-program "./outputs/ref.s"
mv "./ref-program" "./outputs/ref-program"
refSize=$(stat -c %s "./outputs/ref-program")
ourSize=$(stat -c %s "./outputs/our-program")
rm a.out
# diff ./outputs/ref-output.txt ./outputs/our-output.txt
diffs=$(diff -U 0 ./outputs/ref-output.txt ./outputs/our-output.txt | tail -n +3 | grep -c '^@')
if [ "$diffs" != "0" ]; then
	printf "There are %s differences between the reference output and the actual output\n" "$diffs"
else
	printf "The reference output matches the actual output\n"
fi
percentage="$(printf "%.2f" "$(echo "scale=4; ($ourLC - $refLC)" | bc)")"
printf "Line Count: %s line difference : %s v.s. %s\n" "$percentage" "$ourLC" "$refLC"

percentage="$(printf "%.2f" "$(echo "scale=4; ($ourSize - $refSize) / $refSize * 100" | bc)")"
printf "Size: %s percent of the size : %s v.s. %s\n" "$percentage" "$ourSize" "$refSize"

echo ""
if [[ -n $2 ]]; then
	pr -m -t ./outputs/our-tac.cl-tac ./outputs/ref-tac.cl-tac
	echo ""
fi
# echo "----------------------------------------------------"
