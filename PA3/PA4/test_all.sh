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
rm main.cm*

for file in ../testing/PA3c3/*.cl; do
	../testing/cool --type "$file"
done
for test in ../testing/PA3c3/*.cl-type; do
	cp "$test" .
	file=$(basename "$test" .cl-type)
        cp ../testing/PA3c3/"$file".cl .
	./a.out "$file.cl-type"
	gcc -static -fno-pie -g3 -o program "$file".s
	printf "Result of program %s:\n" "$file"
        out=$(./program 2>/dev/null)
        ./newcool --x86 "$file".cl
        gcc -static -fno-pie -g3 -o program "$file".s
        refout=$(./program 2>/dev/null)
        echo $out
        echo $refout

	rm "./$file".cl* 2>/dev/null
	rm "./$file.s" 2>/dev/null
	echo ""
done
