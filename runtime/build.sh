#!/bin/sh

wine ~/Downloads/ISLisp/ISLisp.exe < ~/src/islisp/compiler/compiler.lisp
gcc -g -c OUTPUT.c
gcc -g -Wall -Wextra OUTPUT.o `ls -1 *.c | grep -v OUTPUT.c`
