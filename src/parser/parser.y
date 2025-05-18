%{
#include <stdio.h>
#include <stdlib.h>
#include <string>
#include "ast/ASTNode.hpp"
#include <vector>
#include <memory>

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
    std::vector<std::pair<VariableNode*, ASTNode*>>* bindings;
    std::vector<VariableNode*>* args;
}

%token <fval> FLOAT
%token <bval> BOOL
%token <sval> STRING ID
%token PLUS MINUS TIMES DIV POW UMINUS CONCAT
%token GREATER LESS GREATER_THAN LESS_THAN 
%token AND OR NOT
%token LPAREN RPAREN SEMICOLON LKEY RKEY LET IN ASSIGNM COMA LAMBDA FUNCTION

%type <node> toplevel_item block_lines expr line funtion
%type <block> statements
%type <bindings> assingments
%type <args> arguments

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
    /* vacío */             { /* Root ya inicializado */ }
  | program toplevel_item   { /* Las acciones están en toplevel_item */ }
  ;

toplevel_item:
    line SEMICOLON        { root->push_line($1); }
  | block_lines           { root->push_block(static_cast<BlockNode*>($1)); }
  | funtion               { root->push_func(static_cast<FunctionNode*>($1)); }
  ;

line:
    expr                  { $$ = $1; }
  ;

block_lines:
    LKEY statements RKEY  { $$ = $2; }
  ;

statements:
    /* vacío */           { $$ = new BlockNode(); }
  | statements line       { $1->push_back($2); $$ = $1; }
  ;

funtion: 
    FUNCTION ID LPAREN arguments RPAREN block_lines {
        $$ = new FunctionNode($2, *$4, static_cast<BlockNode*>($6), yylineno);
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

expr:
    FLOAT                           { $$ = new FloatNode($1, yylineno); }
  | BOOL                            { $$ = new BoolNode($1, yylineno); }
  | STRING                          { $$ = new StringNode($1, yylineno); }
  | ID                              { $$ = new VariableNode($1, yylineno); }
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
        $$ = new LetInNode(*$2, static_cast<BlockNode*>($4), yylineno); 
        delete $2; 
    }
  ;

%%

int yyerror(const char* msg) {
    fprintf(stderr, "[Line %d] Parser error: cerca de '%s'\n", yylineno, yytext);
    return 1;
}