#include <iostream>
#include "ast/ASTNode.hpp"

// Declaraciones necesarias del parser
extern int yyparse();
extern ASTNode* root;

int main() {
    std::cout << "🔍 Analizando entrada...\n";

    if (yyparse() == 0) {
        std::cout << "✅ Análisis sintáctico exitoso. AST:\n";
        if (root) {
            root->print();
        } else {
            std::cerr << "⚠️ AST vacío.\n";
        }
    } else {
        std::cerr << "❌ Error de sintaxis.\n";
    }

    return 0;
}
