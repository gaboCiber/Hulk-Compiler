%{
#include <stdio.h>
#include <stdlib.h>
#include <string>
#include "ast/ASTNode.hpp"
#include <vector>
#include <memory>
#include <utility>

extern int yylex(void);
extern int yyerror(const char*);
extern int yylineno;
extern char* yytext;

ProgramNode* root = new ProgramNode();

%}

%code requires {
#include "ast/ASTNode.hpp"
}

%union {
    float fval;
    bool bval;
    char* sval;
    ASTNode* node;
    BlockNode* block;
    FunctionNode* func;
    std::vector<std::pair<VariableNode*, ASTNode*>>* bindings;
    std::vector<VariableNode*>* args;
    std::vector<ASTNode*>* expr_list;
    std::pair<ASTNode*, ASTNode*>* cond_pair;
    std::vector<std::pair<ASTNode*, ASTNode*>>* cond_list;
}

%token <fval> FLOAT
%token <bval> BOOL
%token <sval> STRING ID
%token PLUS MINUS TIMES DIV POW UMINUS CONCAT
%token GREATER LESS GREATER_THAN LESS_THAN 
%token AND OR NOT
%token LPAREN RPAREN SEMICOLON LKEY RKEY LET IN ASSIGNM COMA LAMBDA FUNCTION WHILE IF ELSE ELIF

%type <node> program expr toplevel_item
%type <block> block_lines statements
%type <func> function
%type <bindings> assingments
%type <args> arguments
%type <expr_list> args expr_list
%type <cond_pair> elif_clause
%type <cond_list> elif_sequence

%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE
%nonassoc ELIF
%nonassoc ASSIGNM DESTRUCTIVE_ASSIGNM LAMBDA
%right NOT
%left AND OR
%nonassoc EQUAL NOEQUAL
%nonassoc GREATER LESS GREATER_THAN LESS_THAN 
%left CONCAT
%left PLUS MINUS
%left TIMES DIV
%right POW
%right UMINUS

%%

program:
    /* vacío */             { $$ = root; }
  | program toplevel_item   { $$ = root; }
  ;

toplevel_item:
    expr SEMICOLON        { root->push_statement($1); }
  | block_lines SEMICOLON { root->push_statement($1); }
  | function SEMICOLON    { root->push_func($1); }
  ;

block_lines:
    LKEY statements RKEY  { $$ = $2; }
  ;

statements:
    /* vacío */               { $$ = new BlockNode(); }
  | statements expr SEMICOLON { $1->push_back($2); $$ = $1; }
  ;

function: 
    FUNCTION ID LPAREN arguments RPAREN block_lines {
        $$ = new FunctionNode($2, *$4, $6, yylineno);
        delete $4;
    }
  | FUNCTION ID LPAREN arguments RPAREN LAMBDA expr {
        BlockNode* block = new BlockNode();
        block->push_back($7);
        $$ = new FunctionNode($2, *$4, block, yylineno);
        delete $4;
    }
  ;

arguments:
    /* vacío */ {
        $$ = new std::vector<VariableNode*>();
    }
  | ID {
        auto* list = new std::vector<VariableNode*>();
        list->push_back(new VariableNode($1, yylineno));
        $$ = list;
    }
  | arguments COMA ID {
        $1->push_back(new VariableNode($3, yylineno));
        $$ = $1;
    }
  ;

args:
    /* vacío */ { $$ = new std::vector<ASTNode*>(); }
  | expr_list   { $$ = $1; }
  ;

expr_list:
    expr { 
        auto* list = new std::vector<ASTNode*>();
        list->push_back($1);
        $$ = list;
    }
  | expr_list COMA expr {
        $1->push_back($3);
        $$ = $1;
    }
  ;

assingments:
    ID ASSIGNM expr {
        auto* list = new std::vector<std::pair<VariableNode*, ASTNode*>>();
        list->emplace_back(new VariableNode($1, yylineno), $3);
        $$ = list;
    }
  | assingments COMA ID ASSIGNM expr {
        $1->emplace_back(new VariableNode($3, yylineno), $5);
        $$ = $1;
    }
  ;

elif_clause:
    ELIF LPAREN expr RPAREN expr        { $$ = new std::pair<ASTNode*, ASTNode*>($3, $5); }
  | ELIF LPAREN expr RPAREN block_lines { $$ = new std::pair<ASTNode*, ASTNode*>($3, $5); }
  ;

elif_sequence:
    elif_clause {
        $$ = new std::vector<std::pair<ASTNode*, ASTNode*>>();
        $$->push_back(*$1);
        delete $1;
    }
  | elif_sequence elif_clause {
        $1->push_back(*$2);
        delete $2;
        $$ = $1;
    }
  ;

expr:
    FLOAT                           { $$ = new FloatNode($1, yylineno); }
  | BOOL                            { $$ = new BoolNode($1, yylineno); }
  | STRING                          { $$ = new StringNode($1, yylineno); }
  | ID                              { $$ = new VariableNode($1, yylineno); }
  | ID LPAREN args RPAREN           { $$ = new CallFuncNode($1, *$3, yylineno); delete $3; }
  | NOT expr                        { $$ = new UnaryOpNode("!", $2, yylineno); }
  | MINUS expr %prec UMINUS         { $$ = new UnaryOpNode("-", $2, yylineno); }
  | expr PLUS expr                  { $$ = new BinOpNode("+", $1, $3, yylineno); }
  | expr MINUS expr                 { $$ = new BinOpNode("-", $1, $3, yylineno); }
  | expr TIMES expr                 { $$ = new BinOpNode("*", $1, $3, yylineno); }
  | expr DIV expr                   { $$ = new BinOpNode("/", $1, $3, yylineno); }
  | expr POW expr                   { $$ = new BinOpNode("^", $1, $3, yylineno); }
  | expr GREATER expr               { $$ = new BinOpNode(">", $1, $3, yylineno); }
  | expr LESS expr                  { $$ = new BinOpNode("<", $1, $3, yylineno); }
  | expr GREATER_THAN expr          { $$ = new BinOpNode(">=", $1, $3, yylineno); }
  | expr LESS_THAN expr             { $$ = new BinOpNode("<=", $1, $3, yylineno); }
  | expr EQUAL expr                 { $$ = new BinOpNode("==", $1, $3, yylineno); }
  | expr NOEQUAL expr               { $$ = new BinOpNode("!=", $1, $3, yylineno); }
  | expr AND expr                   { $$ = new BinOpNode("&", $1, $3, yylineno); }
  | expr OR expr                    { $$ = new BinOpNode("|", $1, $3, yylineno); }
  | expr CONCAT expr                { $$ = new BinOpNode("@", $1, $3, yylineno); }
  | LPAREN expr RPAREN              { $$ = $2; }
  | ID DESTRUCTIVE_ASSIGNM expr     { 
        ASTNode* lhs = new VariableNode($1, yylineno); 
        $$ = new BinOpNode(":=", lhs, $3, yylineno); 
    }
  | LET assingments IN expr         { 
        BlockNode* block = new BlockNode(); 
        block->push_back($4); 
        $$ = new LetInNode(*$2, block, yylineno); 
        delete $2; 
    }
  | LET assingments IN block_lines  { 
        $$ = new LetInNode(*$2, $4, yylineno); 
        delete $2; 
    }
  | WHILE LPAREN expr RPAREN block_lines {
        $$ = new WhileNode($3, $5, yylineno);
    }
  | WHILE LPAREN expr RPAREN LAMBDA expr {
        BlockNode* block = new BlockNode();
        block->push_back($6);
        $$ = new WhileNode($3, block, yylineno);
    }
  | IF LPAREN expr RPAREN expr ELSE expr {
        $$ = new IfNode($3, $5, $7, yylineno);
    }
  | IF LPAREN expr RPAREN block_lines ELSE expr {
        $$ = new IfNode($3, $5, $7, yylineno);
    }
  | IF LPAREN expr RPAREN expr ELSE block_lines {
        $$ = new IfNode($3, $5, $7, yylineno);
    }
  | IF LPAREN expr RPAREN block_lines ELSE block_lines {
        $$ = new IfNode($3, $5, $7, yylineno);
    }
  | IF LPAREN expr RPAREN expr %prec LOWER_THAN_ELSE {
        $$ = new IfNode($3, $5, nullptr, yylineno);
    }
  | IF LPAREN expr RPAREN block_lines %prec LOWER_THAN_ELSE {
        $$ = new IfNode($3, $5, nullptr, yylineno);
    }
  | IF LPAREN expr RPAREN expr elif_sequence ELSE expr {
        IfNode* ifNode = new IfNode($3, $5, $8, yylineno);
        for (const auto& elif : *$6) {
            ifNode->addElifBranch(elif.first, elif.second);
        }
        delete $6;
        $$ = ifNode;
    }
  | IF LPAREN expr RPAREN block_lines elif_sequence ELSE expr {
        IfNode* ifNode = new IfNode($3, $5, $8, yylineno);
        for (const auto& elif : *$6) {
            ifNode->addElifBranch(elif.first, elif.second);
        }
        delete $6;
        $$ = ifNode;
    }
  | IF LPAREN expr RPAREN expr elif_sequence ELSE block_lines {
        IfNode* ifNode = new IfNode($3, $5, $8, yylineno);
        for (const auto& elif : *$6) {
            ifNode->addElifBranch(elif.first, elif.second);
        }
        delete $6;
        $$ = ifNode;
    }
  | IF LPAREN expr RPAREN block_lines elif_sequence ELSE block_lines {
        IfNode* ifNode = new IfNode($3, $5, $8, yylineno);
        for (const auto& elif : *$6) {
            ifNode->addElifBranch(elif.first, elif.second);
        }
        delete $6;
        $$ = ifNode;
    }
  ;

%%

int yyerror(const char* msg) {
    fprintf(stderr, "[Line %d] Parser error: cerca de '%s'\n", yylineno, yytext);
    return 1;
}