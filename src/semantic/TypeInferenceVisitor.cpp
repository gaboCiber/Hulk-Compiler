#include "semantic/TypeInferenceVisitor.hpp"
#include "ast/ASTNode.hpp"
#include "TypeInferenceVisitor.hpp"


TypeInferenceVisitor::TypeInferenceVisitor(Context& context) : ctx(context) {}

void TypeInferenceVisitor::putTypeOnVariables(ASTNode* node, Type* type){
    if (VariableNode* var = dynamic_cast<VariableNode*>(node)) {
        SymbolInfo* info = ctx.currentScope()->lookup(var->name);
        if(info)
        {
            if (info->type == nullptr)
                info->type = type;
            else if(info->type != ctx.object_type)
                info->type = ctx.type_registry.findLowestCommonAncestor(info->type, type);
        }
    }
}

void TypeInferenceVisitor::visit(FloatNode& node) {
    lastType = ctx.number_type;
}

void TypeInferenceVisitor::visit(BoolNode& node) {
    lastType = ctx.boolean_type;
}

void TypeInferenceVisitor::visit(StringNode& node) {
    lastType = ctx.string_type;
}

void TypeInferenceVisitor::visit(UnaryOpNode& node) {
    // Primero chequea el hijo
    node.node->accept(*this);
    if (errorFlag) return;  // corto si ya hubo error

    Type* nodeT;
    if (node.op == "-") {
        nodeT = ctx.number_type;
    }
    else if (node.op == "!") {
        nodeT = ctx.boolean_type;
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
    Type* leftT = lastType;
    if (errorFlag) return;  // corto si ya hubo error

    // Luego el derecho
    node.right->accept(*this);
    Type* rightT = lastType;
    if (errorFlag) return;

    Type* typeL, *typeR, *typeNode;
    
    if (node.op == "+" || node.op == "-" || node.op == "*" || node.op == "/" || node.op == "^" || node.op == "%") {
        typeL = typeR = typeNode = ctx.number_type;
    }
    else if (node.op == ">" || node.op == "<" || node.op == ">=" || node.op == "<=") {
        typeL = typeR = ctx.number_type;
        typeNode = ctx.boolean_type;
    }
    else if (node.op == "==" || node.op == "!=") {
        typeL = leftT;
        typeR = rightT;
        typeNode = ctx.boolean_type;
    }
    else if( node.op == ":="){
        // typeL = leftT;
        // typeR = typeNode = rightT;
        typeL = typeR = typeNode = rightT; // El lado izquierdo debe adaptarse al derecho
    }
    else if (node.op == "&" || node.op == "|") {
        typeL = typeR = typeNode = ctx.boolean_type;
    }
    else if (node.op == "@") {
        typeL = leftT;
        typeR = rightT;
        typeNode = ctx.string_type;
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
    
    if(node.declared_type != "")
        lastType = ctx.type_registry.get_type(node.declared_type);
    else {
        SymbolInfo* info = ctx.currentScope()->lookup(node.name);
    
        if (info->type == nullptr) {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: " + "tipo no inferido para variable '" +  node.name + "' \n" ;
        }
    
        lastType = info->type;
    }

    
}

void TypeInferenceVisitor::visit(LetInNode& node) {
    ctx.pushScope(node.scope);
    for (auto& pair: node.bindings)
    {
        pair.second->accept(*this);

        if(errorFlag)
            return;

        if(VariableNode* var = dynamic_cast<VariableNode*>(pair.second))
        {
            if(var->declared_type != "")
                lastType = ctx.type_registry.get_type(var->declared_type);
            
            SymbolInfo* info = ctx.currentScope()->lookup(pair.first->name);
            info->type = lastType;
        }
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
        if(info->type == nullptr)
        {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(arg->line) + "] Error semántico: no fue posible inferir el tipo del argumento '" + arg->name + "'.\n";
            return;
        }
    }

    FunctionInfo* info = ctx.lookupFunction(node.name);

    if(node.declared_type != "")
    {
        lastType = ctx.type_registry.get_type(node.declared_type);
        info->returnType = lastType;
    }

    if(lastType == nullptr)
    {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: no fue posible inferir el tipo de la funicon '" + node.name + "'.\n";
        return;
    }
    
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
            if(info->builtinInfo->argTypes[i] != nullptr)
                putTypeOnVariables(args, info->builtinInfo->argTypes[i]);
        }

        i++;
    } 

    lastType = info->returnType;
    
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
    
    std::vector<Type*> branchTypes;

    for(auto& pair: node.getBranches()){
        pair.first->accept(*this);
        if(errorFlag)
            return;

        pair.second->accept(*this);
        if(errorFlag)
            return;
        
        if(lastType == nullptr)
        {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(pair.second->line) + "] Error semántico: no fue posible inferir el tipo del bloque if/elif.\n";
            return;
        }

        branchTypes.push_back(lastType);
    }

    if(ASTNode* br = node.getElseBranch()) {
        br->accept(*this);
        if(errorFlag)
            return;

        if(lastType == nullptr)
        {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(br->line) + "] Error semántico: no fue posible inferir el tipo del bloque else.\n";
            return;
        }

        branchTypes.push_back(lastType);
    }

    // Inferir tipo común
    lastType = branchTypes.empty() ? ctx.object_type : branchTypes[0];
    for (size_t i = 1; i < branchTypes.size(); ++i) {
        lastType = ctx.type_registry.findLowestCommonAncestor(lastType, branchTypes[i]);
    }

    node.returnType = lastType;
}

void TypeInferenceVisitor::visit(TypeMember& node){
    node.accept(*this);
}

void TypeInferenceVisitor::visit(TypeNode& node){
    
    // Registrar el tipo ya se hizo en DefinitionVisitor

    push_current_type(node.name);
    ctx.pushScope(node.scope);
    
    if(node.inherits != nullptr)
    {
        node.inherits->accept(*this);
        if(errorFlag)
                return;
    }

    for (auto member : node.members) {
        member->accept(*this);
        if (errorFlag) return;
    }

    for (auto arg : *node.type_args){
        arg->accept(*this);
        if(errorFlag)
            return;
    }
    
    pop_current_type();
    ctx.popScope();

}

void TypeInferenceVisitor::visit(InheritsNode& node){
    if (node.parent_args) {
        for (auto arg : *node.parent_args) {
            arg->accept(*this);
            if (errorFlag) return;
        }
    }

}

void TypeInferenceVisitor::visit(AttributeNode& node){
    if (node.initializer) {
        node.initializer->accept(*this);
        if (errorFlag) return;
    }

    if (!node.declared_type.empty())
    {
        Type* declared = ctx.type_registry.get_type(node.declared_type);
        if (!declared) {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(node.line) + "] Error: el tipo al atributo '" + node.getName() + "' no pudo ser inferido";
            return;
        }

        lastType = declared;
    }

    auto attrs_info = get_current_type()->object_data.attributes;

    attrs_info[node.getName()] = lastType;
}

void TypeInferenceVisitor::visit(MethodNode& node){
    
    std::string method_full_name = get_current_type()->name + "." + node.getName();

    FunctionInfo* func = ctx.lookupFunction(method_full_name);
    
    func->node->accept(*this);

    ctx.pushScope(func->node->scope);
    
    auto method_info = get_current_type()->object_data.methods[node.getName()];
    
    size_t i = 0;
    for (auto& arg: func->node->args)
    {
        SymbolInfo* info = ctx.currentScope()->lookup(arg->name);
        method_info->parameter_types.at(i) = info->type;
        
        i++;
    }

    method_info->return_type = func->returnType;

    ctx.popScope();
}

void TypeInferenceVisitor::visit(NewNode& node){
    
    Type* type = ctx.type_registry.get_type(node.type_name);

    for (auto arg : *node.arguments) {
        arg->accept(*this);
        if (errorFlag) return;
    }

    lastType = type;

}

void TypeInferenceVisitor::visit(MemberAccessNode& node){
    node.object->accept(*this);
    if (errorFlag) 
        return;

    lastType = get_current_type()->object_data.attributes[node.member_name];
}

void TypeInferenceVisitor::visit(SelfNode& node){
    Type* current = get_current_type();
    if (!current) {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error: 'self' usado fuera de tipo";
        return;
    }
    lastType = current;
}

void TypeInferenceVisitor::visit(BaseNode& node){
    Type* current = get_current_type();
    if (!current || !current->object_data.parent) {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error: 'base' inválido en este contexto";
        return;
    }
    
    // Procesar argumentos si existen
    if (node.arguments) {
        for (auto arg : *node.arguments) {
            arg->accept(*this);
            if (errorFlag) return;
        }
    }
    
    
    lastType = current->object_data.parent->object_data.methods[ctx.currentScope()->functionName]->return_type;
}

void TypeInferenceVisitor::visit(MethodCallNode& node){

    node.object->accept(*this);
    if (errorFlag) 
            return;

    for (auto arg : node.arguments) {
        arg->accept(*this);
        if (errorFlag) 
            return;
    }

    lastType = get_current_type()->object_data.methods[node.getMethodName()]->return_type;

}
    
