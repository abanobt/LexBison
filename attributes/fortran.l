%{
#include <string.h>
#include "y.tab.h"

char* get_sconst_sval(const char*);

%}

Letter [a-zA-Z]
Digit [0-9]

%option case-insensitive 

%%
            /* Comments */
"!"[^\n]*   ; // Comment is skipped, no token

            /* Reserved Words */
PROGRAM     { return PROGRAM; }
END         { return END; }
ELSE        { return ELSE; }
IF          { return IF; }
THEN        { return THEN; }
INTEGER     { return INTEGER; }
REAL        { return REAL; }
CHARACTER   { return CHARACTER; }
PRINT       { return PRINT; }
LEN         { return LEN; }
        
                                /* Identifiers/Constants */
{Letter}({Letter}|{Digit}|_)*   { 
                                    // Identifiers
                                    yylval.sval = strdup(yytext);
                                    return IDENT;
                                }
{Digit}+                        { 
                                    // Integer constants
                                    int value = atoi(yytext); // Get the numeric value of the token
                                    yylval.ival = value;
                                    return ICONST;
                                }
({Digit}*)\.({Digit}+)          {
                                    // Real (float) constants
                                    float value = atof(yytext); // Get the numeric value of the token
                                    yylval.rval = value;
                                    return RCONST;
                                }
\"([^"])+\"|\'([^'])+\'         {
                                    // String literals, empty strings not allowed, escaping the quotes (\" \') not allowed.
                                    char* value = get_sconst_sval(yytext);
                                    yylval.sval = value;
                                    return SCONST;
                                }
        
        /* Operators */
"+"     { return PLUS; }   /* Arithmetic addition or concatenation */
"-"     { return MINUS; }  /* Arithmetic subtraction */
"*"     { return MULT; }   /* Multiplication */
"/"     { return DIV; }    /* Division */
"**"    { return POW; }    /* Exponentiation */
"="     { return ASSOP; }  /* Assignment */
"=="    { return EQ; }     /* Equality */
"<"     { return LTHAN; }  /* Less than */
">"     { return GTHAN; }  /* Greater than */
"//"    { return CAT; }    /* Concatenation */


        /* Delimiters */
","     { return COMMA; }   /* Comma */
"("     { return LPAREN; }  /* Left Parenthesis */
")"     { return RPAREN; }  /* Right Parenthesis */
"::"    { return DCOLON; }  /* Double Colons */
"."     { return DOT; }     /* Dot */
"?"     { return DEF; }     /* Default mark */    

[ \t\n\r]      ; /* Ignore whitespace */

.              { printf("Unrecognized character: %s\n", yytext); }

%%

int yywrap() {
	return 1;
}

// Takes in the yytext of an SCONST, removes the quotes, and returns the string value
char* get_sconst_sval(const char* str) {
    size_t len = strlen(str);
    char* str_value = malloc(len - 1); // Create a string that is 2 characters shorter than str
    strncpy(str_value, str + 1, len - 2); // Copy str ignoring the first and last characters (remove the quotes)
    str_value[len - 2] = '\0'; // Null terminate the string
    return str_value;
}
