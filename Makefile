# Create Assembly code (from .scm file to .s file.)
%:
	echo '(load "loader.scm") (compile-scheme-file "$(MAKECMDGOALS).scm" "$(MAKECMDGOALS).s")' | scheme -q
	nasm -f elf64 -l $(MAKECMDGOALS).lst  $(MAKECMDGOALS).s
	gcc -m64 -o $(MAKECMDGOALS)  $(MAKECMDGOALS).o
