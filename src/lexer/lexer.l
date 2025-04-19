%{
#include "parser.tab.h"
#include <stdlib.h>
#include "ast/ASTNode.hpp"
%}

%%

[ \t\n\r]+            ;  // Ignorar espacios
[0-9]+\.[0-9]+        { yylval.fval = atof(yytext); return FLOAT; }
[0-9]+                { yylval.fval = atof(yytext); return FLOAT; }
true                  { yylval.bval = true; return BOOL; }
false                 { yylval.bval = false; return BOOL; }
"+"                   return PLUS;
"-"                   return MINUS;
"*"                   return TIMES;
"/"                   return DIV;
"^"                   return POW;
"("                   return LPAREN;
")"                   return RPAREN;
";"                   return SEMICOLON;
">"                   return GREATER;
"<"                   return LESS;
">="                  return GREATER_THAN;
"<="                  return LESS_THAN;
"=="                  return EQUAL;
"!="                  return NOEQUAL;
"&"                   return AND;
"|"                   return OR;
"!"                   return NOT;
.                     return yytext[0];

%%
int yywrap() { return 1; }
