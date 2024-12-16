%{
#include <stdio.h>
#include <string.h> 
#include "attributes.h"

int yylex(void);
void yyerror(char*);

// Reused nodes
ASTNode* NODE_PLUS = NULL; 
ASTNode* NODE_MINUS = NULL; 
ASTNode* NODE_POW = NULL;
ASTNode* NODE_MULT = NULL;
ASTNode* NODE_DIV = NULL;
ASTNode* NODE_CAT = NULL;
ASTNode* NODE_EQ = NULL;
ASTNode* NODE_LTHAN = NULL;
ASTNode* NODE_GTHAN = NULL;
ASTNode* NODE_CHARACTER = NULL;
ASTNode* NODE_INTEGER = NULL;
ASTNode* NODE_REAL = NULL;
%}

/* Bison declarations */
// Semantic value
%union {
    int ival;
    float rval;
    char* sval;
    struct ASTNode* astnode;
}

// Tokens
%token PROGRAM END ELSE IF THEN INTEGER REAL CHARACTER PRINT LEN // Reserverd words
%token <sval> IDENT SCONST // Identifiers/ String Constants
%token <ival> ICONST // Integer constant
%token <rval> RCONST // Real constant 
%token COMMA LPAREN RPAREN DCOLON DOT DEF // Delimiters

// Associativity
%right ASSOP // Assignment operator
%left EQ LTHAN GTHAN // Equality, less than, greater than
%left PLUS MINUS CAT // ddition, subtraction, concatenation
%left MULT DIV // Multiplication, division
%left POW // Exponentiation

// Non-terminals
%type <astnode> Var Factor SFactor TermExpr MultExpr Expr RelExpr ExprList AssignStmt SimpleStmt SimpleIfStmt BlockIfStmt PrintStmt Stmt VarDecl VarList Type Decl StmtBlock DeclBlock Prog Start

// Start rule
%start Start

%%

/* Rules */
Start: Prog { $$ = $1; printf("Successfully parsed:\n"); print_ast_node($1, 1); }
;

Prog: PROGRAM IDENT DeclBlock StmtBlock END PROGRAM IDENT { 
            ASTNode* node = create_node(NODE_PROG, 4);
            
            ASTNode* ident = create_node(NODE_IDENT, 0);
            ident->sval = $2;
            node->children[0] = ident;
            
            node->children[1] = $3;
            node->children[2] = $4;
            
            ident = create_node(NODE_IDENT, 0);
            ident->sval = $7;
            node->children[3] = ident;
            $$ = node;
      }
    | PROGRAM IDENT StmtBlock END PROGRAM IDENT { 
            ASTNode* ident1 = create_node(NODE_IDENT, 0);
            ident1->sval = $2;
            ASTNode* ident2 = create_node(NODE_IDENT, 0);
            ident2->sval = $6;
            $$ = create_node3(NODE_PROG, ident1, $3, ident2); 
      }
    | PROGRAM IDENT DeclBlock END PROGRAM IDENT { 
            ASTNode* ident1 = create_node(NODE_IDENT, 0);
            ident1->sval = $2;
            ASTNode* ident2 = create_node(NODE_IDENT, 0);
            ident2->sval = $6;
            $$ = create_node3(NODE_PROG, ident1, $3, ident2); 
      }
    | PROGRAM IDENT END PROGRAM IDENT { 
            ASTNode* ident1 = create_node(NODE_IDENT, 0);
            ident1->sval = $2;
            ASTNode* ident2 = create_node(NODE_IDENT, 0);
            ident2->sval = $5;
            $$ = create_node2(NODE_PROG, ident1, ident2); 
      }
;

// Aditional rule necessary to allow for blocks of 1 or more Decls
DeclBlock: DeclBlock Decl { $$ = expand_node($1, $2); }
         | Decl { $$ = create_node1(NODE_DECL_BLOCK, $1); }
;

// Aditional rule necessary to allow for blocks of 1 or more Stmts
StmtBlock: StmtBlock Stmt { $$ = expand_node($1, $2); }
         | Stmt { $$ = create_node1(NODE_STMT_BLOCK, $1); }
;

Decl: Type DCOLON VarList { $$ = create_node2(NODE_DECL, $1, $3); }
;

Type: INTEGER { $$ = create_node(NODE_TYPE, 0); $$->actual_type = TYPE_INTEGER; }
    | REAL { $$ = create_node(NODE_TYPE, 0); $$->actual_type = TYPE_REAL; }
    | CHARACTER LPAREN LEN ASSOP ICONST RPAREN { 
        ASTNode* len_node = create_node(NODE_LEN, 0);
        len_node->ival = $5;
        $$ = create_node1(NODE_TYPE, len_node); 
        $$->actual_type = TYPE_CHARACTER; 
      }
    | CHARACTER { $$ = create_node(NODE_TYPE, 0); $$->actual_type = TYPE_CHARACTER; }
;

VarList: VarList COMMA VarDecl { $$ = expand_node($1, $3); }
       | VarDecl { $$ = create_node1(NODE_VAR_LIST, $1); }
;

// This is an additional rule necessary to make the VarList rule possible
VarDecl: AssignStmt { $$ = $1; }
       | Var { $$ = $1; }
;

Stmt: AssignStmt { $$ = $1; }
    | BlockIfStmt { $$ = $1; }
    | PrintStmt { $$ = $1; }
    | SimpleIfStmt { $$ = $1; }
;

PrintStmt: PRINT MULT COMMA ExprList { $$ = create_node1(NODE_PRINT_STMT, $4); }
;

BlockIfStmt: IF LPAREN RelExpr RPAREN THEN StmtBlock END IF { $$ = create_node2(NODE_BLOCK_IF_STMT, $3, $6); }
           | IF LPAREN RelExpr RPAREN THEN StmtBlock ELSE StmtBlock END IF { $$ = create_node3(NODE_BLOCK_IF_STMT, $3, $6, $8); }
;

SimpleIfStmt: IF LPAREN RelExpr RPAREN SimpleStmt { $$ = create_node2(NODE_SIMPLE_IF_STMT, $3, $5); }
;

SimpleStmt: AssignStmt { $$ = $1; }
          | PrintStmt { $$ = $1; }
;

AssignStmt: Var ASSOP Expr { $$ = create_node2(NODE_ASSIGN_STMT, $1, $3); }
;

ExprList: ExprList COMMA Expr { $$ = expand_node($1, $3); }
        | Expr { $$ = create_node1(NODE_EXPR_LIST, $1); }
;

RelExpr: RelExpr EQ Expr { $$ = create_node2(NODE_REL_EXPR, $1, $3); $$->op = OP_EQ; }
    | RelExpr LTHAN Expr { $$ = create_node2(NODE_REL_EXPR, $1, $3); $$->op = OP_LTHAN; }
    | RelExpr GTHAN Expr { $$ = create_node2(NODE_REL_EXPR, $1, $3); $$->op = OP_GTHAN; }
    | Expr { $$ = $1; }
;

Expr: Expr PLUS MultExpr { $$ = create_node2(NODE_EXPR, $1, $3); $$->op = OP_PLUS; }
    | Expr MINUS MultExpr { $$ = create_node2(NODE_EXPR, $1, $3); $$->op = OP_MINUS; }
    | Expr CAT MultExpr { $$ = create_node2(NODE_EXPR, $1, $3); $$->op = OP_CAT; }
    | MultExpr { $$ = $1; }
;

MultExpr: MultExpr MULT TermExpr { $$ = create_node2(NODE_MULT_EXPR, $1, $3); $$->op = OP_MULT; }
        | MultExpr DIV TermExpr { $$ = create_node2(NODE_MULT_EXPR, $1, $3); $$->op = OP_DIV; }
        | TermExpr { $$ = $1; }
;

TermExpr: TermExpr POW SFactor { $$ = create_node2(NODE_TERM_EXPR, $1, $3); $$->op = OP_POW; }
        | SFactor { $$ = $1; }
;

SFactor: PLUS Factor { $$ = create_node1(NODE_SFACTOR, $2); $$->op = OP_PLUS; }
       | MINUS Factor { $$ = create_node1(NODE_SFACTOR, $2); $$->op = OP_MINUS; }
       | Factor { $$ = $1; }
;

Factor: IDENT { 
            ASTNode* ident = create_node(NODE_IDENT, 0);
            ident->sval = $1;
            $$ = create_node1(NODE_FACTOR, ident); 
        } 
      | ICONST { 
            ASTNode* iconst = create_node(NODE_ICONST, 0);
            iconst->ival = $1;
            $$ = create_node1(NODE_FACTOR, iconst); 
        } 
      | RCONST { 
            ASTNode* rconst = create_node(NODE_RCONST, 0);
            rconst->rval = $1;
            $$ = create_node1(NODE_FACTOR, rconst); 
        } 
      | SCONST { 
            ASTNode* sconst = create_node(NODE_SCONST, 0);
            sconst->sval = $1;
            $$ = create_node1(NODE_FACTOR, sconst); 
        } 
      | LPAREN Expr RPAREN { $$ = create_node1(NODE_FACTOR, $2); }
;

Var: IDENT { 
            ASTNode* ident = create_node(NODE_IDENT, 0);
            ident->sval = $1;
            $$ = create_node1(NODE_VAR, ident);
     }
;

%%

int main() {  
//    NODE_PLUS = create_node("PLUS(+)", 0);
//    NODE_MINUS = create_node("MINUS(-)", 0);
//    NODE_POW = create_node("POW(**)", 0);
//    NODE_MULT = create_node("MULT(*)", 0);
//    NODE_DIV = create_node("DIV(/)", 0);
//    NODE_CAT = create_node("CAT(//)", 0);
//    NODE_EQ = create_node("EQ(==)", 0);
//    NODE_LTHAN = create_node("LTHAN(<)", 0);
//    NODE_GTHAN = create_node("GTHAN(>)", 0);
//    NODE_CHARACTER = create_node("CHARACTER", 0);
//    NODE_INTEGER = create_node("INTEGER", 0);
//    NODE_REAL = create_node("REAL", 0);
     
	yyparse();
	return 0;
}

void yyerror(char *s) {
	fprintf(stderr, ">> %s\n", s);
}
