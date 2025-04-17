%{
  #include <stdio.h>
  #include <stdlib.h>
  #include "ast/ASTNode.hpp"
 
  extern int yylex(void);
  extern int yyerror(const char*);

  ASTNode* root;
%}

%code requires {
  #include "ast/ASTNode.hpp"
}

%union {
    float fval;
    ASTNode* node;
}

%token <fval> FLOAT
%token PLUS MINUS TIMES DIV POW
%token LPAREN RPAREN SEMICOLON

%type <node> expr

%left PLUS MINUS
%left TIMES DIV
%right POW  // asociatividad derecha para potenciación

%%

program:
    expr SEMICOLON { root = $1; }
    ;

expr:
      FLOAT             { $$ = new FloatNode($1); }
    | expr PLUS expr    { $$ = new BinOpNode("+", $1, $3); }
    | expr MINUS expr   { $$ = new BinOpNode("-", $1, $3); }
    | expr TIMES expr   { $$ = new BinOpNode("*", $1, $3); }
    | expr DIV expr     { $$ = new BinOpNode("/", $1, $3); }
    | expr POW expr     { $$ = new BinOpNode("^", $1, $3); }
    | LPAREN expr RPAREN { $$ = $2; }
    ;

%%

int yyerror(const char* msg) {
    fprintf(stderr, "Syntax error: %s\n", msg);
    return 1;
}
