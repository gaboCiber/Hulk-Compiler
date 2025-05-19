#include "semantic/TypeCheckerVisitor.hpp"
#include "ast/ASTNode.hpp"

TypeCheckerVisitor::TypeCheckerVisitor(Context& context) : ctx(context) {}

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
        if (childT != Type::Float && childT != Type::Unknown) {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: operador '" + node.op + "' requiere un operando de tipo float.";
            return;
        }
        lastType = Type::Float;
    }
    else if (node.op == "!") {

        if (childT != Type::Bool && childT != Type::Unknown) {
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

    bool isUnknow = leftT == Type::Unknown || rightT == Type::Unknown;

    // Ahora, según el operador:
    if (node.op == "+" || node.op == "-" || node.op == "*" ||
        node.op == "/" || node.op == "^") {
        // Aritméticos: ambos deben ser float
        if ( !isUnknow && (leftT != Type::Float || rightT != Type::Float)) {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: operador '" + node.op + "' requiere operandos de tipo float.";
            return;
        }
        lastType = Type::Float;
    }
    else if (node.op == ">" || node.op == "<" || node.op == ">=" || node.op == "<=") {
        // Relacionales: ambos deben ser float, resultado bool
        if ( ! isUnknow && (leftT != Type::Float || rightT != Type::Float)) {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: operador '" + node.op + "' requiere operandos de tipo float.";
            return;
        }
        lastType = Type::Bool;
    }
    else if (node.op == "==" || node.op == "!=") {
        if (!isUnknow && leftT != rightT) {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: operador '" + node.op + "' requiere operandos del mismo tipo.";
            return;
        }
        lastType = Type::Bool;
    }
    else if( node.op == ":=")
    {
        if (!isUnknow && leftT != rightT) {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: operador '" + node.op + "' requiere operandos del mismo tipo.";
            return;
        }
        else if (leftT == Type::Unknown)
            lastType = rightT;
        else if (rightT == Type::Unknown)
            lastType = leftT;
        else
            lastType = rightT;
    }
    else if (node.op == "&" || node.op == "|") {
        if (!isUnknow && (leftT != Type::Bool || rightT != Type::Bool)) {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: operador '" + node.op + "' requiere operandos booleanos.";
            return;
        }
        lastType = Type::Bool;
    }
    else if (node.op == "@") {
        // Concatenación: solo string
        if (!isUnknow && (leftT != Type::String || rightT != Type::String)) {
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

void TypeCheckerVisitor::visit(VariableNode& node) {
    SymbolInfo* info = ctx.currentScope()->lookup(node.name);
    
    // if (info == nullptr) {
    //     errorFlag = true;
    //     errorMsg = "Error: Variable '" + node.name + "' no definida. Línea " + std::to_string(node.line);
    // }
    
    lastType = info->type;
}

void TypeCheckerVisitor::visit(LetInNode& node) {
    ctx.pushScope(node.scope);
    for (auto& pair: node.bindings)
    {
        pair.second->accept(*this);

        if(errorFlag)
            return;

        SymbolInfo* info = ctx.currentScope()->lookup(pair.first->name);
        info->type = lastType;
    }
        
    node.block->accept(*this);
    ctx.popScope();
}

void TypeCheckerVisitor::visit(FunctionNode& node) {
    ctx.pushScope(node.scope);
    node.block->accept(*this);
    FunctionInfo* info = ctx.lookupFunction(node.name);
    info->returnType = lastType;
    ctx.popScope();
}

void TypeCheckerVisitor::visit(ProgramNode& node) {
    for (auto stmt : node.functions) {
        stmt->accept(*this);
        if(errorFlag)
            return;
    }

    for (auto stmt : node.statements) {
        stmt->accept(*this);
        if(errorFlag)
            return;
    }
} 