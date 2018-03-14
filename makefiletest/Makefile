%:
	nasm -f elf64 -F stabs -l $(MAKECMDGOALS).lst  $(MAKECMDGOALS).s
	gcc -m64 -g -o $(MAKECMDGOALS)  $(MAKECMDGOALS).o
