all.run: all.o
	clang -m32 -o all.run main.c all.o

all.o: all.s
	nasm -f elf32 -o all.o all.s

all.s:
	dune build
	dune exec schism_compiler > all.s
	
clean:
	rm all.*