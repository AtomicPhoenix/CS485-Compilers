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
	echo "$file"
	../testing/cool --type "$file"
done
for test in ../testing/PA3c3/*.cl-type; do
	cp "$test" .
	echo "Trying $test"
	file=$(basename "$test" .cl-type)
	echo "Doing $file"
	./a.out "$file.cl-type"
	gcc -static -fno-pie -g3 -o program "$file".s
	printf "Result of program %s:\n" "$file"
	./program 2>/dev/null
	rm "./$file".cl* 2>/dev/null
	rm "./$file.s" 2>/dev/null
done
