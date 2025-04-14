%{
#include <stdio.h>
%}

%%
[0-9]+    { printf("Número: %s\n", yytext); }
[a-zA-Z]+ { printf("Identificador: %s\n", yytext); }
.         { printf("Otro: %s\n", yytext); }

%%

int yywrap() { return 1; }

