#!/bin/bash
flex lexer.l
gcc lex.yy.c -o lexer -lfl
./lexer < test4.txt 
