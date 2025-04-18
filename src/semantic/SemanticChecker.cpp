#include <iostream>
#include "semantic/SemanticChecker.hpp"
#include "ast/ASTNode.hpp"

void SemanticChecker::visit(FloatNode& node) {
    std::cout << "✔️ Chequeando FloatNode: " << node.value << "\n";
}

void SemanticChecker::visit(BinOpNode& node) {
    std::cout << "✔️ Chequeando BinOpNode: " << node.op << "\n";

    if (node.left) node.left->accept(*this);
    if (node.right) node.right->accept(*this);

    // === Check: División por cero ===
    if (node.op == "/") {
        FloatNode* rightLiteral = dynamic_cast<FloatNode*>(node.right);
        if (rightLiteral && rightLiteral->value == 0.0f) {
            std::cerr << "❌ Error semántico: división por cero detectada.\n";
        }
    }
}
