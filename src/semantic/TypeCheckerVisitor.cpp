#include "semantic/TypeCheckerVisitor.hpp"
#include "ast/ASTNode.hpp"

void TypeCheckerVisitor::visit(FloatNode& node) {
    lastType = Type::Float;
}

void TypeCheckerVisitor::visit(BoolNode& node) {
    lastType = Type::Bool;
}

void TypeCheckerVisitor::visit(StringNode& node) {
    lastType = Type::String;
}

void TypeCheckerVisitor::visit(UnaryOpNode& node) {
    // Primero chequea el hijo
    node.node->accept(*this);
    Type childT = lastType;
    if (errorFlag) return;  // corto si ya hubo error

    // Ahora, según el operador:
    if (node.op == "-") {
        // Negación: solo float
        if (childT != Type::Float) {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: operador '" + node.op + "' requiere un operando de tipo float.";
            return;
        }
        lastType = Type::Float;
    }
    else if (node.op == "!") {
        // Negación lógica: solo bool
        if (childT != Type::Bool) {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: operador '" + node.op + "' requiere un operando booleano.";
            return;
        }
        lastType = Type::Bool;
    }
    else {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: operador desconocido '" + node.op + "'.";
    }
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
            errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: operador '" + node.op + "' requiere operandos de tipo float.";
            return;
        }
        lastType = Type::Float;
    }
    else if (node.op == ">" || node.op == "<" || node.op == ">=" || node.op == "<=") {
        // Relacionales: ambos deben ser float, resultado bool
        if (leftT != Type::Float || rightT != Type::Float) {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: operador '" + node.op + "' requiere operandos de tipo float.";
            return;
        }
        lastType = Type::Bool;
    }
    else if (node.op == "==" || node.op == "!=") {
        // Solo permitimos comparaciones entre floats por ahora
        if (leftT != rightT) {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: operador '" + node.op + "' requiere operandos del mismo tipo.";
            return;
        }
        lastType = Type::Bool;
    }
    else if (node.op == "&" || node.op == "|") {
        if (leftT != Type::Bool || rightT != Type::Bool) {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: operador '" + node.op + "' requiere operandos booleanos.";
            return;
        }
        lastType = Type::Bool;
    }
    else if (node.op == "@") {
        // Concatenación: solo string
        if (leftT != Type::String || rightT != Type::String) {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: operador '" + node.op + "' requiere operandos de tipo string.";
            return;
        }
        lastType = Type::String;
    }
    else {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: operador desconocido '" + node.op + "'.";
    }
}

void TypeCheckerVisitor::visit(BlockNode& node) {
    for (auto& child : node.statements) {
        child->accept(*this);
        if (errorFlag) return;  // corto si ya hubo error
    }
}