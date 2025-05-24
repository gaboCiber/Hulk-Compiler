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
        if (leftT != rightT) {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: operador '" + node.op + "' requiere operandos del mismo tipo.";
            return;
        }
        lastType = Type::Bool;
    }
    else if( node.op == ":=")
    {
        if (leftT != rightT) {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: operador '" + node.op + "' requiere operandos del mismo tipo.";
            return;
        }

        lastType = rightT;
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

void TypeCheckerVisitor::visit(CallFuncNode& node){
    FunctionInfo* info = ctx.lookupFunction(node.functionName);

    if ( node.arguments.size() != info->node->args.size() ) {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: la función '" + node.functionName + "' requiere " + std::to_string(info->node->args.size()) + " argumentos. Se le pasaron " + std::to_string(node.arguments.size()) + ".\n";
        return;
    }


    for (size_t i = 0; i < node.arguments.size(); ++i) {
        
        node.arguments[i]->accept(*this);
        if(errorFlag)
            return;

        Type actualType = lastType;

        VariableNode* expectedArg = info->node->args[i];
        SymbolInfo* expectedInfo = info->node->scope->lookup(expectedArg->name);

        if (!expectedInfo) {
            errorFlag = true;
            errorMsg = "[Internal error] Argumento '" + expectedArg->name + "' no está definido en el scope de la función '" + info->node->name + "'.\n";
            return;
        }

        if (expectedInfo->type != actualType) {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: el argumento '" + expectedArg->name + "' de la funcion '" + info->node->name + "' es de tipo '" + TypeToString(expectedInfo->type) + "' . No de tipo '" + TypeToString(actualType) +  "' .\n";
            return;
        }
    }

    lastType = info->returnType;
}

void TypeCheckerVisitor::visit(WhileNode& node) {
    node.condition->accept(*this);
    if(errorFlag)
            return;

    if(lastType != Type::Bool)
    {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: la condición del while debe ser de tipo booleano.\n";
        return;
    }

    node.body->accept(*this);
    if(errorFlag)
            return;

    if(lastType != node.returnType)
    {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: el tipo inferido del bloque while es diferente a su tipo chequeado.\n";
        return;
    }
    
    //lastType = *node.returnType;
}

void TypeCheckerVisitor::visit(IfNode& node) {
    for(auto& pair: node.getBranches()){
        pair.first->accept(*this);
        if(errorFlag)
            return;

        if(lastType != Type::Bool)
        {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(pair.first->line) + "] Error semántico: la condición del if/elif debe ser de tipo booleano.\n";
            return;
        }

        pair.second->accept(*this);
        if(errorFlag)
            return;

        if(lastType != node.returnType)
        {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(pair.second->line) + "] Error semántico: todos los bloque if/elif deben retornar el mismo tipo.\n";
            return;
        }
    }

    if(ASTNode* br = node.getElseBranch()) {
        br->accept(*this);
        if(errorFlag)
            return;

        if(lastType != node.returnType)
        {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(br->line) + "] Error semántico: todos los bloque else deben retornar el mismo tipo.\n";
            return;
        }
    }

}