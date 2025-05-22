%{
#include "parser.tab.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

extern int yylineno;
%}

%option yylineno

%%
[ \t\r]+              ;
\n                    ;

[_]+[a-zA-Z0-9_]+     { fprintf(stderr, "[Line %d] Lexer error: identificador inválido '%s'\n", yylineno, yytext); exit(1); }
[0-9]+[a-zA-Z_]+      { fprintf(stderr, "[Line %d] Lexer error: número mal formado '%s'\n", yylineno, yytext); exit(1); }

\"([^\"\\]|\\.)*\"     { yylval.sval = strdup(yytext + 1); yylval.sval[strlen(yylval.sval) - 1] = '\0'; return STRING; }
"let"                 return LET;
"in"                  return IN;
"function"            return FUNCTION;
"while"               return WHILE;
"if"                  return IF;
"else"                return ELSE;
"elif"                return ELIF;
"true"                { yylval.bval = true; return BOOL; }
"false"               { yylval.bval = false; return BOOL; }

[a-zA-Z_][a-zA-Z0-9_]* { yylval.sval = strdup(yytext); return ID; }
[0-9]+\.[0-9]+         { yylval.fval = atof(yytext); return FLOAT; }
[0-9]+                 { yylval.fval = atof(yytext); return FLOAT; }

"**"|"^"              return POW;
"+"                   return PLUS;
"-"                   return MINUS;
"*"                   return TIMES;
"/"                   return DIV;
"("                   return LPAREN;
")"                   return RPAREN;
"{"                   return LKEY;
"}"                   return RKEY;
";"                   return SEMICOLON;
","                   return COMA;
">="                  return GREATER_THAN;
"<="                  return LESS_THAN;
">"                   return GREATER;
"<"                   return LESS;
"=="                  return EQUAL;
"!="                  return NOEQUAL;
"=>"                  return LAMBDA;
":="                  return DESTRUCTIVE_ASSIGNM;
"="                   return ASSIGNM;
"&"                   return AND;
"|"                   return OR;
"!"                   return NOT;
"@"                   return CONCAT;

.                     { fprintf(stderr, "[Line %d] Lexer error: símbolo no reconocido '%s'\n", yylineno, yytext); exit(1); }

%%

int yywrap() { return 1; }
