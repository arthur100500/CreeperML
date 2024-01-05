cd cm_build
gcc -c ../lib/bindings.c && nasm -f elf64 program.asm -o program.o && gcc bindings.o program.o -o program