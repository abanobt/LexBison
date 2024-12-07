%{
//#include <stdio.h>
#include <string.h>
#include "y.tab.h"
char* get_sconst_string_value(const char*);
void lexer_print(const char* str, ...);
%}

Letter [a-zA-Z]
Digit [0-9]

%option case-insensitive 

%%
            /* Comments */
"!"[^\n]*   { lexer_print("Comment: %s\n", yytext); } // Comment is skipped, no token

            /* Reserved Words */
PROGRAM     { lexer_print("Reserved Word: PROGRAM\n"); return PROGRAM; }
END         { lexer_print("Reserved Word: END\n"); return END; }
ELSE        { lexer_print("Reserved Word: ELSE\n"); return ELSE; }
IF          { lexer_print("Reserved Word: IF\n"); return IF; }
THEN        { lexer_print("Reserved Word: THEN\n"); return THEN; }
INTEGER     { lexer_print("Reserved Word: INTEGER\n"); return INTEGER; }
REAL        { lexer_print("Reserved Word: REAL\n"); return REAL; }
CHARACTER   { lexer_print("Reserved Word: CHARACTER\n"); return CHARACTER; }
PRINT       { lexer_print("Reserved Word: PRINT\n"); return PRINT; }
LEN         { lexer_print("Reserved Word: LEN\n"); return LEN; }
        
                                /* Identifiers/Constants */
{Letter}({Letter}|{Digit}|_)*   { 
                                    // Identifiers
                                    lexer_print("IDENT: %s\n", yytext);
                                    yylval.sval = strdup(yytext);
                                    return IDENT;
                                }
{Digit}+                        { 
                                    // Integer constants
                                    int value = atoi(yytext); // Get the numeric value of the token
                                    lexer_print("ICONST: %i\n", value); 
//                                    yylval.ival = value;
                                    yylval.sval = strdup(yytext);
                                    return ICONST;
                                }
({Digit}*)\.({Digit}+)          {
                                    // Real (float) constants
                                    float value = atof(yytext); // Get the numeric value of the token
                                    lexer_print("RCONST: %f\n", value);
//                                    yylval.fval = value;
                                    yylval.sval = strdup(yytext);
                                    return RCONST;
                                }
\"([^"])+\"|\'([^'])+\'         {
                                    // String literals, empty strings not allowed, escaping the
                                    // quotes (\" \') not allowed.
                                    char* value = get_sconst_string_value(yytext);
                                    lexer_print("SCONST: %s\n", value);
                                    // free(value); // Should free in Bison
                                    // yylval.sval = value;
                                    return SCONST;
                                }
        
        /* Operators */
"+"     { lexer_print("Operator: PLUS\n"); return PLUS; }    /* Arithmetic addition or concatenation */
"-"     { lexer_print("Operator: MINUS\n"); return MINUS; }  /* Arithmetic subtraction */
"*"     { lexer_print("Operator: MULT\n"); return MULT; }    /* Multiplication */
"/"     { lexer_print("Operator: DIV\n"); return DIV; }      /* Division */
"**"    { lexer_print("Operator: POW\n"); return POW; }      /* Exponentiation */
"="     { lexer_print("Operator: ASSOP\n"); return ASSOP; }  /* Assignment */
"=="    { lexer_print("Operator: EQ\n"); return EQ; }        /* Equality */
"<"     { lexer_print("Operator: LTHAN\n"); return LTHAN; }  /* Less than */
">"     { lexer_print("Operator: GTHAN\n"); return GTHAN; }  /* Greater than */
"//"    { lexer_print("Operator: CAT\n"); return CAT; }      /* Concatenation */


        /* Delimiters */
","     { lexer_print("Delimiter: COMMA\n"); return COMMA; }   /* Comma */
"("     { lexer_print("Delimiter: LPAREN\n"); return LPAREN; } /* Left Parenthesis */
")"     { lexer_print("Delimiter: RPAREN\n"); return RPAREN; } /* Right Parenthesis */
"::"    { lexer_print("Delimiter: DCOLON\n"); return DCOLON; } /* Double Colons */
"."     { lexer_print("Delimiter: DOT\n"); return DOT; }       /* Dot */
"?"     { lexer_print("Delimiter: DEF\n"); return DEF; }       /* Default mark */    

[ \t\n\r]                         ; /* Ignore whitespace */
.                               { printf("Unrecognized character: %s\n", yytext); }
%%

//int main() {
//    yylex();
//    return 0;
//}
int yywrap()
{
	return 1;
}
void lexer_print(const char* str, ...) {
//    va_list args;
//    printf("LEXER: ");
//    printf(str, args);
}
// Takes in the yytext of an SCONST, removes the quotes, and returns the string value
char* get_sconst_string_value(const char* str) {
    size_t len = strlen(str);
    char* str_value = malloc(len - 1); // Create a string that is 2 characters shorter than str
    strncpy(str_value, str + 1, len - 2); // Copy str ignoring the first and last characters (remove the quotes)
    str_value[len - 2] = '\0'; // Null terminate the string
    return str_value;
}
