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
ocamlc main.ml
rm ./a.out
rm ./main.cmi
rm ./main.cmo
