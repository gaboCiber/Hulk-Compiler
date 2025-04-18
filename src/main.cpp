#include <iostream>
#include "ast/ASTNode.hpp"
#include "semantic/SemanticChecker.hpp"

// Declaraciones necesarias del parser
extern int yyparse();
extern ASTNode* root;

int main() {
    std::cout << "🔍 Analizando entrada...\n";

    if (yyparse() == 0 && root) {
        std::cout << "✅ Análisis sintáctico exitoso. AST:\n";
        root->print();

        std::cout << "\n🔎 Iniciando análisis semántico...\n";
        SemanticChecker checker;
        root->accept(checker);
    } else {
        std::cerr << "❌ Error de sintaxis.\n";
    }

    return 0;
}
