#include "semantic/TypeInferenceVisitor.hpp"
#include "ast/ASTNode.hpp"
#include "TypeInferenceVisitor.hpp"


TypeInferenceVisitor::TypeInferenceVisitor(Context& context) : ctx(context) {}

void TypeInferenceVisitor::putTypeOnVariables(ASTNode* node, Type type){
    if (VariableNode* var = dynamic_cast<VariableNode*>(node)) {
        SymbolInfo* info = ctx.currentScope()->lookup(var->name);
        if (info && info->type == Type::Unknown)
            info->type = type;
        else if(info->type != Type::Unknown && info->type != type)
        {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(var->line) + "] Error semántico: variable '" + var->name + "' ya definida como '" + TypeToString(info->type) + "' y se intenta asignar tipo '" + TypeToString(type) + "'.\n";
        }

    }

    
}

void TypeInferenceVisitor::visit(FloatNode& node) {
    lastType = Type::Float;
}

void TypeInferenceVisitor::visit(BoolNode& node) {
    lastType = Type::Bool;
}

void TypeInferenceVisitor::visit(StringNode& node) {
    lastType = Type::String;
}

void TypeInferenceVisitor::visit(UnaryOpNode& node) {
    // Primero chequea el hijo
    node.node->accept(*this);
    if (errorFlag) return;  // corto si ya hubo error

    Type nodeT;
    if (node.op == "-") {
        nodeT = Type::Float;
    }
    else if (node.op == "!") {
        nodeT = Type::Bool;
    }    
    else {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: operador desconocido '" + node.op + "'.";
    }
    
    putTypeOnVariables(node.node, nodeT);

    lastType = nodeT;
}

void TypeInferenceVisitor::visit(BinOpNode& node) {
    // Primero chequea el hijo izquierdo
    node.left->accept(*this);
    Type leftT = lastType;
    if (errorFlag) return;  // corto si ya hubo error

    // Luego el derecho
    node.right->accept(*this);
    Type rightT = lastType;
    if (errorFlag) return;

    Type typeL, typeR, typeNode;
    
    if (node.op == "+" || node.op == "-" || node.op == "*" || node.op == "/" || node.op == "^" || node.op == "%") {
        typeL = typeR = typeNode = Type::Float;
    }
    else if (node.op == ">" || node.op == "<" || node.op == ">=" || node.op == "<=") {
        typeL = typeR = Type::Float;
        typeNode = Type::Bool;
    }
    else if (node.op == "==" || node.op == "!=") {
        typeL = leftT;
        typeR = rightT;
        typeNode = Type::Bool;
    }
    else if( node.op == ":="){
        typeL = leftT;
        typeR = typeNode = rightT;
    }
    else if (node.op == "&" || node.op == "|") {
        typeL = typeR = typeNode = Type::Bool;
    }
    else if (node.op == "@") {
        typeL = leftT;
        typeR = rightT;
        typeNode = Type::String;
    }
    else {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: operador desconocido '" + node.op + "'.";
    }
    
    putTypeOnVariables(node.left, typeL);
    putTypeOnVariables(node.right, typeR);

    lastType = typeNode;
}

void TypeInferenceVisitor::visit(BlockNode& node) {
    for (auto& child : node.statements) {
        child->accept(*this);
        if (errorFlag) return;  // corto si ya hubo error
    }
}

void TypeInferenceVisitor::visit(VariableNode& node) {
    SymbolInfo* info = ctx.currentScope()->lookup(node.name);
    
    if (info == nullptr) {
        errorFlag = true;
        errorMsg = "Error: Variable '" + node.name + "' no definida. Línea " + std::to_string(node.line);
    }
    
    lastType = info->type;
}

void TypeInferenceVisitor::visit(LetInNode& node) {
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

void TypeInferenceVisitor::visit(FunctionNode& node) {
    ctx.pushScope(node.scope);
    
    node.block->accept(*this);
    
    for (auto& arg: node.args)
    {
        SymbolInfo* info = ctx.currentScope()->lookup(arg->name);
        if(info->type == Type::Unknown)
        {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(arg->line) + "] Error semántico: no fue posible inferir el tipo del argumento '" + arg->name + "'.\n";
            return;
        }
    }

    if(lastType == Type::Unknown)
    {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: no fue posible inferir el tipo de la funicon '" + node.name + "'.\n";
        return;
    }
    

    FunctionInfo* info = ctx.lookupFunction(node.name);
    
    if(info->returnType != Type::Unknown && info->returnType != Type::Any)
        lastType = info->returnType;
    else
        info->returnType = lastType;

    ctx.popScope();
}

void TypeInferenceVisitor::visit(ProgramNode& node) {
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

void TypeInferenceVisitor::visit(CallFuncNode& node){
    FunctionInfo* info = ctx.lookupFunction(node.functionName);

    size_t i = 0;
    for (auto args : node.arguments) {
        args->accept(*this);
        
        if(errorFlag)
            return;

        if (info->isBuiltin)
        {
            if(info->builtinInfo->argTypes[i] != Type::Any)
                putTypeOnVariables(args, info->builtinInfo->argTypes[i]);
        }

        i++;
    } 

    if(info->returnType != Type::Any)
    {
        lastType = info->returnType;
    }
    
}

void TypeInferenceVisitor::visit(WhileNode& node) {
    node.condition->accept(*this);
    if(errorFlag)
            return;

    node.body->accept(*this);
    if(errorFlag)
            return;

    node.returnType = lastType;   
}

void TypeInferenceVisitor::visit(IfNode& node) {
    
    for(auto& pair: node.getBranches()){
        pair.first->accept(*this);
        if(errorFlag)
            return;

        pair.second->accept(*this);
        if(errorFlag)
            return;
        
        if(lastType == Type::Unknown)
        {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(pair.second->line) + "] Error semántico: no fue posible inferir el tipo del bloque if/elif.\n";
            return;
        }
    }

    if(ASTNode* br = node.getElseBranch()) {
        br->accept(*this);
        if(errorFlag)
            return;

        if(lastType == Type::Unknown)
        {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(br->line) + "] Error semántico: no fue posible inferir el tipo del bloque else.\n";
            return;
        }
    }

    node.returnType = lastType;
}

void TypeInferenceVisitor::visit(TypeMember& node){

}

void TypeInferenceVisitor::visit(TypeNode& node){

}

void TypeInferenceVisitor::visit(InheritsNode& node){

}

void TypeInferenceVisitor::visit(AttributeNode& node){

}

void TypeInferenceVisitor::visit(MethodNode& node){

}

void TypeInferenceVisitor::visit(NewNode& node){

}

void TypeInferenceVisitor::visit(MemberAccessNode& node){

}

void TypeInferenceVisitor::visit(SelfNode& node){

}

void TypeInferenceVisitor::visit(BaseNode& node){

}

void TypeInferenceVisitor::visit(MethodCallNode& node){

}
    
