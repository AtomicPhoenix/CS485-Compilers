cat lib/print.ml lib/parser.ml lib/tac.ml lib/cfg.ml lib/asm.ml lib/intrinsics.ml bin/main.ml |
	sed 's/Print\.//
             s/Parser\.//  
	     s/Tac\.// 
	     s/Asm\.//
	     s/Cfg\.//
	     /^open/d
	     /PA3/d
	     s/Asm\.//
	     s/Tac\.// 
	     s/Intrinsics\.//
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
echo "----------------------------------------------------"
TESTNAME="./test-case.cl-type"
cp "$FILE" $TESTNAME

ocamlc main.ml
./a.out "$TESTNAME"

TESTNAME="$(basename "$TESTNAME" .cl-type)"

gcc -static -fno-pie -ggdb -o program "$TESTNAME".s
./program

TESTNAME="$(basename "$TESTNAME" .s)"
rm "$TESTNAME.cl"* 2>/dev/null
rm main.cm*
echo ""
echo "----------------------------------------------------"
if [ -n "$2" ]; then
	echo "Running with COOL Reference Compiler"
	echo "----------------------------------------------------"
	../cool "$1"
	echo ""
	echo "----------------------------------------------------"
fi

echo ""
echo ""
echo ""
