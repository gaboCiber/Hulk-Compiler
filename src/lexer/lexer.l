%{
#include "parser.tab.h"
#include <stdlib.h>
#include "ast/ASTNode.hpp"
%}

%%

[ \t\n\r]+             ;  // Ignorar espacios
[0-9]+\.[0-9]+         { yylval.fval = atof(yytext); return FLOAT; }
[0-9]+                 { yylval.fval = atof(yytext); return FLOAT; }

"+"                   return PLUS;
"-"                   return MINUS;
"*"                   return TIMES;
"/"                   return DIV;
"^"                   return POW;
"("                   return LPAREN;
")"                   return RPAREN;
";"                   return SEMICOLON;
.                     return yytext[0];

%%
int yywrap() { return 1; }
