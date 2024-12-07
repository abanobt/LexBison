#!/bin/bash
bison -d -y parser.y -Wcounterexamples
flex lexer.l
gcc -c y.tab.h y.tab.c lex.yy.c
gcc y.tab.o lex.yy.o -o $@ fortran
./fortran < test4.txt
