#include "semantic/TypeCheckerVisitor.hpp"
#include "ast/ASTNode.hpp"

void TypeCheckerVisitor::visit(FloatNode& node) {
    lastType = Type::Float;
}

void TypeCheckerVisitor::visit(BoolNode& node) {
    lastType = Type::Bool;
}

void TypeCheckerVisitor::visit(BinOpNode& node) {
    // Primero chequea el hijo izquierdo
    node.left->accept(*this);
    Type leftT = lastType;
    if (errorFlag) return;  // corto si ya hubo error

    // Luego el derecho
    node.right->accept(*this);
    Type rightT = lastType;
    if (errorFlag) return;

    // Ahora, según el operador:
    if (node.op == "+" || node.op == "-" || node.op == "*" ||
        node.op == "/" || node.op == "^") {
        // Aritméticos: ambos deben ser float
        if (leftT != Type::Float || rightT != Type::Float) {
            errorFlag = true;
            errorMsg = "Error semántico: operador '" + node.op + "' requiere operandos de tipo float.";
            return;
        }
        lastType = Type::Float;
    }
    else if (node.op == ">" || node.op == "<" || node.op == ">=" || node.op == "<=") {
        // Relacionales: ambos deben ser float, resultado bool
        if (leftT != Type::Float || rightT != Type::Float) {
            errorFlag = true;
            errorMsg = "Error semántico: operador '" + node.op + "' requiere operandos de tipo float.";
            return;
        }
        lastType = Type::Bool;
    }
    else if (node.op == "==" || node.op == "!=") {
        // Solo permitimos comparaciones entre floats por ahora
        if (leftT != rightT) {
            errorFlag = true;
            errorMsg = "Error semántico: operador '" + node.op +
                    "' requiere operandos del mismo tipo.";
            return;
        }
        lastType = Type::Bool;
    }

    else {
        errorFlag = true;
        errorMsg = "Error semántico: operador desconocido '" + node.op + "'.";
    }
}
