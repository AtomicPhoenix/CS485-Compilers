cat lib/print.ml lib/parser.ml lib/tac.ml lib/cfg.ml lib/asm.ml bin/main.ml |
	sed 's/Print\.//
             s/Parser\.//  
	     s/Tac\.// 
	     s/Asm\.//
	     s/Cfg\.//
	     /^open/d
	     /PA3/d
	     s/Asm\.//
	     w ./main.ml'

ocamlc main.ml
rm main.cm*
./a.out "$1"
