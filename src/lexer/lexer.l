%{
#include "parser.tab.h"
#include <stdlib.h>
#include "ast/ASTNode.hpp"
%}

%option yylineno

%%

[ \t\r]+              ;  // Ignorar espacios
\n                    { /* Incrementa automáticamente yylineno */ }
[0-9]+\.[0-9]+        { yylval.fval = atof(yytext); return FLOAT; }
[0-9]+                { yylval.fval = atof(yytext); return FLOAT; }
\"(?:\\.|[^"])*\"     { yylval.sval = strdup(yytext + 1); yylval.sval[strlen(yylval.sval) - 1] = '\0'; return STRING;}
true                  { yylval.bval = true; return BOOL; }
false                 { yylval.bval = false; return BOOL; }
"+"                   return PLUS;
"-"                   return MINUS;
"*"                   return TIMES;
"/"                   return DIV;
"^"                   return POW;
"("                   return LPAREN;
")"                   return RPAREN;
"{"                   return LKEY;
"}"                   return RKEY;
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
"@"                   return CONCAT;
.                     { fprintf(stderr, "[Line %d] Error léxico: símbolo no reconocido '%s'\n", yylineno, yytext); exit(1);}


%%
int yywrap() { return 1; }
