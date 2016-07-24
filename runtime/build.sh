#!/bin/sh

gcc -g -Wall -Wextra OUTPUT.o `ls -1 *.c | grep -v OUTPUT.c`
