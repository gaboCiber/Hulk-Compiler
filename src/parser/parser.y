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

  ASTNode* root;

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
    std::vector<std::pair<std::string, ASTNode*>>* bindings;
}

%token <fval> FLOAT
%token <bval> BOOL
%token <sval> STRING ID
%token PLUS MINUS TIMES DIV POW UMINUS CONCAT
%token GREATER LESS GREATER_THAN LESS_THAN 
%token AND OR NOT
%token LPAREN RPAREN SEMICOLON LKEY RKEY LET IN ASSIGNM COMA

%type <node> program block_lines expr line
%type <block> statements
%type <bindings> assingments

%right NOT
%left AND OR
%nonassoc EQUAL NOEQUAL ASSIGNM
%nonassoc GREATER LESS GREATER_THAN LESS_THAN 
%left CONCAT
%left PLUS MINUS
%left TIMES DIV
%right POW
%right UMINUS


%%

program:
    line                    { root = $1; }
  | block_lines             { root = $1; }
  ;

line:
  expr SEMICOLON            { $$ = $1; }
  ;

block_lines:
  LKEY statements RKEY { $$ = static_cast<ASTNode*>($2); }
  ;

statements:
    /* vacío */             { $$ = new BlockNode(); }
  | statements line         { $1->push_back($2); $$ = $1;}
  ;

assingments:
    ID ASSIGNM expr                   { $$ = new std::vector<std::pair<std::string, ASTNode*>>(); $$->emplace_back($1, $3); }
  | assingments COMA ID ASSIGNM expr  { $1->emplace_back($3, $5); $$ = $1; }


expr:
      FLOAT                   { $$ = new FloatNode($1, yylineno); }
    | BOOL                    { $$ = new BoolNode($1, yylineno); }
    | STRING                  { $$ = new StringNode($1, yylineno); }

    | ID                      { $$ = new VariableNode($1, yylineno);}

    | NOT expr                { $$ = new UnaryOpNode("!", $2, yylineno); }
    | MINUS expr %prec UMINUS { $$ = new UnaryOpNode("-", $2, yylineno); }
    
    | expr PLUS expr          { $$ = new BinOpNode("+", $1, $3, yylineno); }
    | expr MINUS expr         { $$ = new BinOpNode("-", $1, $3, yylineno); }
    | expr TIMES expr         { $$ = new BinOpNode("*", $1, $3, yylineno); }
    | expr DIV expr           { $$ = new BinOpNode("/", $1, $3, yylineno); }
    | expr POW expr           { $$ = new BinOpNode("^", $1, $3, yylineno); }

    | expr GREATER expr       { $$ = new BinOpNode(">", $1, $3, yylineno);}
    | expr LESS expr          { $$ = new BinOpNode("<", $1, $3, yylineno);}
    | expr GREATER_THAN expr  { $$ = new BinOpNode(">=", $1, $3, yylineno);}
    | expr LESS_THAN expr     { $$ = new BinOpNode("<=", $1, $3, yylineno);}
    | expr EQUAL expr         { $$ = new BinOpNode("==", $1, $3, yylineno);}
    | expr NOEQUAL expr       { $$ = new BinOpNode("!=", $1, $3, yylineno);}
    | expr AND expr           { $$ = new BinOpNode("&", $1, $3, yylineno); }
    | expr OR expr            { $$ = new BinOpNode("|", $1, $3, yylineno); }
    | expr CONCAT expr        { $$ = new BinOpNode("@", $1, $3, yylineno); }

    | LPAREN expr RPAREN      { $$ = $2; }
    | LET assingments IN block_lines { $$ = new LetInNode(*$2, static_cast<BlockNode*>($4), yylineno); delete $2; }
    ;

%%

int yyerror(const char* msg) {
    fprintf(stderr, "[Line %d] Parser error: cerca de '%s'\n", yylineno, yytext);
    return 1;
}


