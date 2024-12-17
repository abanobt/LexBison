%{
#include <stdio.h>
#include <string.h> 
#include "attributes.h"

int yylex(void);
void yyerror(char*);

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
%token COMMA LPAREN RPAREN DCOLON DOT DEF                        // Delimiters
%token <sval> IDENT SCONST                                       // Identifiers/String Constants
%token <ival> ICONST                                             // Integer constants
%token <rval> RCONST                                             // Real constants

// Operator associativity
%right ASSOP         // Assignment operator
%left EQ LTHAN GTHAN // Equality, less than, greater than
%left PLUS MINUS CAT // ddition, subtraction, concatenation
%left MULT DIV       // Multiplication, division
%left POW            // Exponentiation

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
            node->children[0] = create_ident_node($2);
            node->children[1] = $3;
            node->children[2] = $4;
            node->children[3] = create_ident_node($7);
            $$ = node;
      }
    | PROGRAM IDENT StmtBlock END PROGRAM IDENT { $$ = create_node3(NODE_PROG, create_ident_node($2), $3, create_ident_node($6)); }
    | PROGRAM IDENT DeclBlock END PROGRAM IDENT { $$ = create_node3(NODE_PROG, create_ident_node($2), $3, create_ident_node($6)); }
    | PROGRAM IDENT END PROGRAM IDENT { $$ = create_node2(NODE_PROG, create_ident_node($2), create_ident_node($5));}
;

// Aditional rule necessary to allow for blocks of 1 or more Decls
DeclBlock: DeclBlock Decl { $$ = expand_node($1, $2); }
         | Decl { $$ = create_node1(NODE_DECL_BLOCK, $1); }
;

// Aditional rule necessary to allow for blocks of 1 or more Stmts
StmtBlock: StmtBlock Stmt { $$ = expand_node($1, $2); }
         | Stmt { $$ = create_node1(NODE_STMT_BLOCK, $1); }
;

Decl: Type DCOLON VarList { $$ = create_node2(NODE_DECL, $1, $3); $3->data_type = $1->data_type; }
;

Type: INTEGER { $$ = create_node(NODE_TYPE, 0); $$->data_type = TYPE_INTEGER; }
    | REAL { $$ = create_node(NODE_TYPE, 0); $$->data_type = TYPE_REAL; }
    | CHARACTER LPAREN LEN ASSOP ICONST RPAREN { 
        ASTNode* len_node = create_node(NODE_LEN, 0);
        len_node->ival = $5;
        $$ = create_node1(NODE_TYPE, len_node); 
        $$->data_type = TYPE_CHARACTER; 
      }
    | CHARACTER { $$ = create_node(NODE_TYPE, 0); $$->data_type = TYPE_CHARACTER; }
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

Factor: IDENT { $$ = create_node1(NODE_FACTOR, create_ident_node($1)); } 
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

Var: IDENT { $$ = create_node1(NODE_VAR, create_ident_node($1)); }
;

%%

int main() {       
	yyparse();
	return 0;
}

void yyerror(char *s) {
	fprintf(stderr, ">> %s\n", s);
}
