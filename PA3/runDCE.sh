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
	     s/Cfg\.//
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

TESTNAME="./test-case.cl-type"
cp "$FILE" $TESTNAME

echo "Running $FILE"
echo "----------------------------------------------------"

ocamlc main.ml
./a.out "$TESTNAME"

TESTNAME="$(basename "$TESTNAME" .cl-type)"
mv "$TESTNAME".cl-tac ./our-output.cl-tac

../cool --tac "$1" --out "./ref-output"

pr -m -t ./our-output.cl-tac ./ref-output.cl-tac

rm "$TESTNAME.cl"* 2>/dev/null
rm main.cm*
echo ""

if [ -n "$2" ]; then
	../cool --x86 --opt --cfg "$1"
	dot -Tpng main_pre.dot >filename.png
	feh ./filename.png
	rm *".dot"
	rm ./filename.png
fi
