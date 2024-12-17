#include <stdlib.h>

// The type of the AST Node
typedef enum {
    NODE_PROG,
    NODE_DECL_BLOCK,
    NODE_STMT_BLOCK,
    NODE_DECL,
    NODE_TYPE,
    NODE_VAR_LIST,
    NODE_PRINT_STMT,
    NODE_BLOCK_IF_STMT,
    NODE_SIMPLE_IF_STMT,
    NODE_ASSIGN_STMT,
    NODE_EXPR_LIST,
    NODE_REL_EXPR,
    NODE_EXPR,
    NODE_MULT_EXPR,
    NODE_TERM_EXPR,
    NODE_SFACTOR,
    NODE_FACTOR,
    NODE_VAR,
    NODE_IDENT,
    NODE_ICONST,
    NODE_RCONST,
    NODE_SCONST,
    NODE_LEN
} ASTNodeType;

const char* node_type_names[] = { 
    "Prog", "DeclBlock", "StmtBlock", "Decl", "Type", "VarList", "PrintStmt",
    "BlockIfStmt", "SimpleIfStmt", "AssignStmt", "ExprList", "RelExpr", "Expr",
    "MultExpr", "TermExpr", "SFactor", "Factor", "Var", "IDENT", "ICONST", 
    "RCONST", "SCONST", "Len"
};

// Attribute which represents the data type of a node
typedef enum { 
    TYPE_CHARACTER, 
    TYPE_INTEGER, 
    TYPE_REAL
} DataType;

const char* data_type_names[] = { "character", "integer", "real" };

// Attribute which represents the operation of a node
typedef enum {
    OP_PLUS,
    OP_MINUS,
    OP_POW,
    OP_MULT,
    OP_DIV,
    OP_CAT,
    OP_EQ,
    OP_LTHAN,
    OP_GTHAN
} Operation;

const char* op_names[] = { 
    "plus", "minus", "pow", "mult", "div", "cat", "eq", "lthan", "gthan"
};

// Represents a node in the Abstract Syntax Tree
typedef struct ASTNode {
    ASTNodeType type;   
    char* sval;                 // Attribute: string value
    int ival;                   // Attrbute: integer value
    float rval;                 // Attribute: real value
    DataType data_type;         // Attribute: data type
    Operation op;               // Attribute: operation
    struct ASTNode** children; 
    int child_count;
} ASTNode;

// Print the ASTNode and its tree to the console
void print_ast_node(ASTNode* node, int indentation) {
    printf("%*c", indentation * 3, ' '); // indent
    printf("↳%s ", node_type_names[node->type]); // print type name
    
    // Print attribtues
    switch (node->type) {
        case NODE_TYPE:
        case NODE_VAR_LIST:
            printf("(data_type=%s)", data_type_names[node->data_type]);
            break;
        case NODE_REL_EXPR:
        case NODE_EXPR:
        case NODE_MULT_EXPR:
        case NODE_TERM_EXPR:
        case NODE_SFACTOR:
            printf("(op=%s)", op_names[node->op]);
            break;
        case NODE_ICONST:
        case NODE_LEN:
            printf("(ival=%d)", node->ival);
            break;
        case NODE_RCONST:
            printf("(rval=%f)", node->rval);
            break;
        case NODE_IDENT:
        case NODE_SCONST:
            printf("(sval=\"%s\")", node->sval);
            break;
    }
    
    printf("\n");
    
    // Print child nodes
    for (int i = 0; i < node->child_count; i++) {
        print_ast_node(node->children[i], indentation + 1);
    }
}

// Create an ASTNode with the specified type and child count
ASTNode* create_node(ASTNodeType type, int child_count) {
    ASTNode* node = (ASTNode*)malloc(sizeof(ASTNode));
    node->type = type;
    node->child_count = child_count;
    node->children = child_count > 0 ? (ASTNode**)malloc(child_count * sizeof(ASTNode*)) : NULL;
    return node;
}

// Create an ASTNode with the specified type and 3 provided children 
ASTNode* create_node3(ASTNodeType type, ASTNode* child0, ASTNode* child1, ASTNode* child2) {
    ASTNode* node = create_node(type, 3);
    node->children[0] = child0;
    node->children[1] = child1;
    node->children[2] = child2;
    return node;
}

// Create an ASTNode with the specified type and 2 provided children 
ASTNode* create_node2(ASTNodeType type, ASTNode* child0, ASTNode* child1) {
    ASTNode* node = create_node(type, 2);
    node->children[0] = child0;
    node->children[1] = child1;
    return node;
}

// Create an ASTNode with the specified type and 1 provided child
ASTNode* create_node1(ASTNodeType type, ASTNode* child) {
    ASTNode* node = create_node(type, 1);
    node->children[0] = child;
    return node;
}

// Create an ASTNode with NODE_IDENT type and the provided sval
ASTNode* create_ident_node(char* sval) {
    ASTNode* ident = create_node(NODE_IDENT, 0);
    ident->sval = sval;
    return ident;
}

// Expand the ASTNode by adding an additional child node
ASTNode* expand_node(ASTNode* node, ASTNode* add_child) {
    int expanded_count = node->child_count + 1;
    ASTNode** expanded_children = (ASTNode**)malloc(expanded_count * sizeof(ASTNode*));
    if (node->children) {
        // Copy original children
        for (int i = 0; i < node->child_count; i++) {
            expanded_children[i] = node->children[i];
        }
        free(node->children);
    }
    expanded_children[expanded_count - 1] = add_child; // Append the add_child node
    node->children = expanded_children;
    node->child_count = expanded_count;  
    return node; 
}

