%{
#include <stdio.h>
#include <string.h> 

typedef struct ASTNode {
//    NodeType type;
    char* sval;
    struct ASTNode** children;
    int childCount;
} ASTNode;

int yylex(void);
void yyerror(char*);
ASTNode* create_nodec(char*, int);
ASTNode* create_node1(char*, ASTNode*);
ASTNode* create_node2(char*, ASTNode*, ASTNode*);
ASTNode* create_node3(char*, ASTNode*, ASTNode*, ASTNode*);
void print_ast_node(ASTNode*, int);

ASTNode* NODE_LPAREN = NULL;
ASTNode* NODE_RPAREN = NULL; 
ASTNode* NODE_PLUS = NULL; 
ASTNode* NODE_MINUS = NULL; 
ASTNode* NODE_POW = NULL;
ASTNode* NODE_MULT = NULL;
ASTNode* NODE_DIV = NULL;
ASTNode* NODE_CAT = NULL;
ASTNode* NODE_EQ = NULL;
ASTNode* NODE_LTHAN = NULL;
ASTNode* NODE_GTHAN = NULL;
ASTNode* NODE_COMMA = NULL;
ASTNode* NODE_ASSOP = NULL;
ASTNode* NODE_DCOLON = NULL;
ASTNode* NODE_IF = NULL;
ASTNode* NODE_THEN = NULL;
ASTNode* NODE_END = NULL;
ASTNode* NODE_ELSE = NULL;
ASTNode* NODE_PRINT = NULL;
ASTNode* NODE_CHARACTER = NULL;
ASTNode* NODE_INTEGER = NULL;
ASTNode* NODE_REAL = NULL;
ASTNode* NODE_LEN = NULL;
ASTNode* NODE_PROGRAM = NULL;

%}

/* Bison declarations */
// Semantic value
%union {
//    int ival;
//    float fval;
    char* sval;
    struct ASTNode* astnode;
}

// Tokens
%token PROGRAM END ELSE IF THEN INTEGER REAL CHARACTER PRINT LEN // Reserverd words
%token <sval> IDENT SCONST // Identifiers/ String Constants
%token <sval> ICONST // Integer constant
%token <sval> RCONST // Real constant 
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
// TODO: reuse ASTNodes for keyword/operator/delimiter nodes

/* Rules */
// The start rule, only here to allow for printing of the AST
Start: Prog { print_ast_node($1, 1); }
;

Prog: PROGRAM IDENT DeclBlock StmtBlock END PROGRAM IDENT { 
            ASTNode* node = create_nodec("Prog", 7);
            node->children[0] = NODE_PROGRAM;
            node->children[1] = create_node1("IDENT", create_nodec($2, 0));
            node->children[2] = $3;
            node->children[3] = $4;
            node->children[4] = NODE_END;
            node->children[5] = NODE_PROGRAM;
            node->children[6] = create_node1("IDENT", create_nodec($7, 0));
            $$ = node;
      }
//    | PROGRAM IDENT StmtBlock END PROGRAM IDENT { printf("Prog\n"); }
//    | PROGRAM IDENT DeclBlock END PROGRAM IDENT { printf("Prog\n"); }
//    | PROGRAM IDENT END PROGRAM IDENT { printf("Prog\n"); }
;

// Aditional rule necessary to allow for blocks of 0 or more Decls
DeclBlock: Decl DeclBlock { $$ = create_node2("DeclBlock", $1, $2); }
         | Decl { $$ = create_node1("DeclBlock", $1); }
//         | { printf("DeclBlock: empty\n"); }
;

// Aditional rule necessary to allow for blocks of 0 or more Stmts
StmtBlock: StmtBlock Stmt { $$ = create_node2("StmtBlock", $1, $2); }
         | Stmt { $$ = create_node1("StmtBlock", $1); }
//         | { printf("StmtBlock: empty\n"); }
;

Decl: Type DCOLON VarList { $$ = create_node3("Decl", $1, NODE_DCOLON, $3); }
;

Type: INTEGER { $$ = create_node1("Type", NODE_INTEGER); }
    | REAL { $$ = create_node1("Type", NODE_REAL); }
    | CHARACTER LPAREN LEN ASSOP ICONST RPAREN { 
        ASTNode* node = create_nodec("Type", 6);
        node->children[0] = NODE_CHARACTER;
        node->children[1] = NODE_LPAREN;
        node->children[2] = NODE_LEN;
        node->children[3] = NODE_ASSOP;
        node->children[4] = create_node1("ICONST", create_nodec($5, 0));
        node->children[5] = NODE_RPAREN;
        $$ = node;
    }
    | CHARACTER { $$ = create_node1("Type", NODE_CHARACTER); }
;

VarList: VarList COMMA VarDecl { $$ = create_node3("VarList", $1, NODE_COMMA, $3); }
       | VarDecl { $$ = create_node1("VarList", $1); }
;

// This is an additional rule necessary to make the VarList rule possible
VarDecl: AssignStmt { $$ = create_node1("VarDecl", $1); }
       | Var { $$ = create_node1("VarDecl", $1); }
;

Stmt: AssignStmt { $$ = create_node1("Stmt", $1); }
    | BlockIfStmt { $$ = create_node1("Stmt", $1); }
    | PrintStmt { $$ = create_node1("Stmt", $1); }
    | SimpleIfStmt { $$ = create_node1("Stmt", $1); }
;

PrintStmt: PRINT MULT COMMA ExprList { 
                ASTNode* node = create_nodec("PrintStmt", 4);
                node->children[0] = NODE_PRINT;
                node->children[1] = NODE_MULT;
                node->children[2] = NODE_COMMA;
                node->children[3] = $4;
                $$ = node;
           }
;

// TODO: BlockIfStmt
BlockIfStmt: IF LPAREN RelExpr RPAREN THEN StmtBlock END IF { 
                ASTNode* node = create_nodec("BlockIfStmt", 8);
                node->children[0] = NODE_IF;
                node->children[1] = NODE_LPAREN;
                node->children[2] = $3;
                node->children[3] = NODE_RPAREN;
                node->children[4] = NODE_THEN;
                node->children[5] = $6;
                node->children[6] = NODE_END;
                node->children[7] = NODE_IF;
                $$ = node;
           }
           | IF LPAREN RelExpr RPAREN THEN StmtBlock ELSE StmtBlock END IF { 
                ASTNode* node = create_nodec("BlockIfStmt", 10);
                node->children[0] = NODE_IF;
                node->children[1] = NODE_LPAREN;
                node->children[2] = $3;
                node->children[3] = NODE_RPAREN;
                node->children[4] = NODE_THEN;
                node->children[5] = $6;
                node->children[6] = NODE_ELSE;
                node->children[7] = $8;
                node->children[8] = NODE_END;
                node->children[9] = NODE_IF;
                $$ = node;
           }
;

SimpleIfStmt: IF LPAREN RelExpr RPAREN SimpleStmt { 
                ASTNode* node = create_nodec("SimpleIfStmt", 5);
                node->children[0] = NODE_IF;
                node->children[1] = NODE_LPAREN;
                node->children[2] = $3;
                node->children[3] = NODE_RPAREN;
                node->children[4] = $5;
                $$ = node;
            }
;

SimpleStmt: AssignStmt { $$ = create_node1("SimpleStmt", $1); }
          | PrintStmt { $$ = create_node1("SimpleStmt", $1); }
;

AssignStmt: Var ASSOP Expr { $$ = create_node3("AssignStmt", $1, NODE_ASSOP, $3); }
;

ExprList: ExprList COMMA Expr { $$ = create_node3("ExprList", $1, NODE_COMMA, $3); }
        | Expr { $$ = create_node1("ExprList", $1); }
;

RelExpr: RelExpr EQ Expr { $$ = create_node3("RelExpr", $1, NODE_EQ, $3); }
    | RelExpr LTHAN Expr { $$ = create_node3("RelExpr", $1, NODE_LTHAN, $3); }
    | RelExpr GTHAN Expr { $$ = create_node3("RelExpr", $1, NODE_GTHAN, $3); }
    | Expr { $$ = create_node1("RelExpr", $1); }
;

Expr: Expr PLUS MultExpr { $$ = create_node3("Expr", $1, NODE_PLUS, $3); }
    | Expr MINUS MultExpr { $$ = create_node3("Expr", $1, NODE_MINUS, $3); }
    | Expr CAT MultExpr { $$ = create_node3("Expr", $1, NODE_CAT, $3); }
    | MultExpr { $$ = create_node1("Expr", $1); }
;

MultExpr: MultExpr MULT TermExpr { $$ = create_node3("MultExpr", $1, NODE_MULT, $3); }
        | MultExpr DIV TermExpr { $$ = create_node3("MultExpr", $1, NODE_DIV, $3); }
        | TermExpr { $$ = create_node1("MultExpr", $1); }
;

TermExpr: TermExpr POW SFactor { $$ = create_node3("TermExpr", $1, NODE_POW, $3); }
        | SFactor { $$ = create_node1("TermExpr", $1); }
;

SFactor: PLUS SFactor { $$ = create_node2("SFactor", NODE_PLUS, $2); }
       | MINUS SFactor { $$ = create_node2("SFactor", NODE_MINUS, $2); }
       | Factor { $$ = create_node1("SFactor", $1); }
;

// I decided to replace IDENT with Var for this rule as it made more sense
Factor: Var { $$ = create_node1("Factor", $1); } 
      | ICONST { $$ = create_node1("Factor", create_node1("ICONST", create_nodec($1, 0))); } 
      | RCONST { $$ = create_node1("Factor", create_node1("RCONST", create_nodec($1, 0))); } 
      | SCONST { $$ = create_node1("Factor", create_node1("SCONST", create_nodec($1, 0))); } 
      | LPAREN Expr RPAREN { $$ = create_node3("Factor", NODE_LPAREN, $2, NODE_RPAREN); }
;

Var: IDENT { $$ = create_node1("Var", create_node1("IDENT", create_nodec($1, 0))); } 
;

%%


ASTNode* create_node3(char* sval, ASTNode* child0, ASTNode* child1, ASTNode* child2) {
    ASTNode* node = create_nodec(sval, 2);
    node->children[0] = child0;
    node->children[1] = child1;
    node->children[2] = child2;
    return node;
}

ASTNode* create_node2(char* sval, ASTNode* child0, ASTNode* child1) {
    ASTNode* node = create_nodec(sval, 2);
    node->children[0] = child0;
    node->children[1] = child1;
    return node;
}

ASTNode* create_node1(char* sval, ASTNode* child) {
    ASTNode* node = create_nodec(sval, 1);
    node->children[0] = child;
    return node;
}

ASTNode* create_nodec(char* sval, int childCount) {
    ASTNode* node = (ASTNode*)malloc(sizeof(ASTNode));
    node->sval = sval;
    node->childCount = childCount;
    node->children = childCount > 0 ? (ASTNode**)malloc(childCount * sizeof(ASTNode*)) : NULL;
    return node;
}

void print_ast_node(ASTNode* node, int indentation) {
    printf("%*c", indentation * 2, ' '); // indent
    printf("↳%s\n", node->sval);
    for (int i = 0; i < node->childCount; i++) {
        print_ast_node(node->children[i], indentation + 1);
    }
}

int main() {
    NODE_LPAREN = create_nodec("LPAREN(()", 0);
    NODE_RPAREN = create_nodec("RPAREN())", 0);
    NODE_PLUS = create_nodec("PLUS(+)", 0);
    NODE_MINUS = create_nodec("MINUS(-)", 0);
    NODE_POW = create_nodec("POW(**)", 0);
    NODE_MULT = create_nodec("MULT(*)", 0);
    NODE_DIV = create_nodec("DIV(/)", 0);
    NODE_CAT = create_nodec("CAT(//)", 0);
    NODE_EQ = create_nodec("EQ(==)", 0);
    NODE_LTHAN = create_nodec("LTHAN(<)", 0);
    NODE_GTHAN = create_nodec("GTHAN(>)", 0);
    NODE_COMMA = create_nodec("COMMA(,)", 0);
    NODE_ASSOP = create_nodec("ASSOP(=)", 0);
    NODE_DCOLON = create_nodec("DCOLON(::)", 0);
    NODE_IF = create_nodec("IF", 0);
    NODE_THEN = create_nodec("THEN", 0);
    NODE_END = create_nodec("END", 0);
    NODE_ELSE = create_nodec("ELSE", 0);
    NODE_PRINT = create_nodec("PRINT", 0);
    NODE_CHARACTER = create_nodec("CHARACTER", 0);
    NODE_INTEGER = create_nodec("INTEGER", 0);
    NODE_REAL = create_nodec("REAL", 0);
    NODE_LEN = create_nodec("LEN", 0);
    NODE_PROGRAM = create_nodec("PROGRAM", 0);
	yyparse();
	return 0;
}

void yyerror(char *s) {
	fprintf(stderr, ">> %s\n", s);
}
