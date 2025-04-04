cat lib/print.ml lib/parser.ml lib/tac.ml lib/asm.ml bin/main.ml |
	sed 's/Print\.//
             s/Parser\.//  
	     s/Tac\.// 
	     s/Asm\.//
	     /^open/d
	     /PA3/d
	     w ./main.ml'
