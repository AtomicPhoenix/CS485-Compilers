cat lib/print.ml lib/parser.ml lib/tac.ml lib/cfg.ml lib/asm.ml bin/main.ml |
	sed 's/Print\.//
             s/Parser\.//  
	     s/Tac\.// 
	     s/Asm\.//
	     s/Cfg\.//
	     /^open/d
	     /PA3/d
	     s/Asm\.//
	     w ./main.ml' >main.ml

ocamlc main.ml
cp "$1" .
file=$(basename "$1" .cl-type)
./a.out "$file.cl-type"

gcc -static -fno-pie -g3 -o program "$file".s
./program

rm "./$file".cl* 2>/dev/null
rm "./$file.s" 2>/dev/null
rm main.cm*
