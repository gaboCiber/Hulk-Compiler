/* main.cpp */
#include <iostream>
extern int yyparse();

int main() {
    std::cout << "Iniciando compilador Hulk..." << std::endl;
    return yyparse();
}