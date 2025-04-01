%{
#include <stdio.h>
int yylex(void);               // Declaración de la función del analizador léxico (Flex)
int yyerror(const char *s);    // Declaración de la función para manejar errores (tipo int)
%}

%token TOKEN

%%
input:
    TOKEN { printf("Token reconocido\n"); }
    ;
%%

int yyerror(const char *s) {
    fprintf(stderr, "Error: %s\n", s);
    return 0;
}
