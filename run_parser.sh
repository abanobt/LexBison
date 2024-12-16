#!/bin/bash
bison -d -y parser.y
flex lexer.l
gcc -c y.tab.h y.tab.c lex.yy.c
gcc y.tab.o lex.yy.o -o $@ fortran
./fortran < test1.txt
./fortran < test2.txt
./fortran < test3.txt
./fortran < test4.txt
./fortran < test5.txt
