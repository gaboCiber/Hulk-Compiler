#include "semantic/UsageCheckerVisitor.hpp"

UsageCheckerVisitor::UsageCheckerVisitor(Context& context) : ctx(context) {}

void UsageCheckerVisitor::visit(FloatNode&) {}
void UsageCheckerVisitor::visit(BoolNode&) {}
void UsageCheckerVisitor::visit(StringNode&) {}

void UsageCheckerVisitor::visit(UnaryOpNode& node) {
    node.node->accept(*this);
}

void UsageCheckerVisitor::visit(BinOpNode& node) {
    node.left->accept(*this);

    if(errorFlag)
        return;

    node.right->accept(*this);
}

void UsageCheckerVisitor::visit(VariableNode& node) {
    SymbolInfo* info = ctx.currentScope()->lookup(node.name);
    if (info == nullptr) {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: variable '" + node.name +  "' no definida en este scope.\n";
    }

    if(!node.declared_type.empty())
    {
        if(!ctx.type_registry.has_type(node.declared_type))
        {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: el tipo '" + node.declared_type +  "' no esta definido.\n";
        }

        info->type = ctx.type_registry.get_type(node.declared_type);
    }
}

void UsageCheckerVisitor::visit(LetInNode& node) {
    ctx.pushScope(node.scope);
    for (auto& pair: node.bindings)
    {   
        pair.first->accept(*this);
        if(errorFlag)
            return;

        pair.second->accept(*this);

        if(errorFlag)
            return;
    }

    node.block->accept(*this);
    ctx.popScope();
}

void UsageCheckerVisitor::visit(BlockNode& node) {
    for (auto stmt : node.statements) {
        stmt->accept(*this);
        
        if (errorFlag) 
            return;
    }
}

void UsageCheckerVisitor::visit(FunctionNode& node) {
    
    if(!node.declared_type.empty())
    {
        if(!ctx.type_registry.has_type(node.declared_type))
        {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: el tipo '" + node.declared_type +  "' no esta definido.\n";
            return;
        }
    }
    
    ctx.pushScope(node.scope);

    for (auto var : node.args)
    {
        var->accept(*this);
        if (errorFlag) 
            return;
    }

    node.block->accept(*this);
    ctx.popScope();
}

void UsageCheckerVisitor::visit(ProgramNode& node) {
    for (auto stmt : node.functions_and_types) {
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

void UsageCheckerVisitor::visit(CallFuncNode& node){
    
    if(ctx.lookupFunction(node.functionName) == nullptr){
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: función '" + node.functionName +  "' no definida.\n";
        return;
    }

    for (auto args : node.arguments) {
        args->accept(*this);
        if(errorFlag)
            return;
    }
}

void UsageCheckerVisitor::visit(WhileNode& node) {
    node.condition->accept(*this);
    if(errorFlag)
            return;

    node.body->accept(*this);
    if(errorFlag)
            return;
}

void UsageCheckerVisitor::visit(IfNode& node) {
    for(auto& pair: node.getBranches()){
        pair.first->accept(*this);
        if(errorFlag)
            return;

        pair.second->accept(*this);
        if(errorFlag)
            return;
    }

    if(ASTNode* br = node.getElseBranch()) {
        br->accept(*this);
        if(errorFlag)
            return;
    }
}

void UsageCheckerVisitor::visit(IsNode& node){

    SymbolInfo* info = ctx.currentScope()->lookup(node.variable_name);
    if(!info)
    {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: variable '" + node.variable_name +  "' no definida en este scope.\n";
        return;
    }

    if(!ctx.type_registry.has_type(node.type_name))
    {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: tipo '" + node.type_name +  "' no definido.\n";
        return;
    }
}

void UsageCheckerVisitor::visit(AsNode& node){
    
    SymbolInfo* info = ctx.currentScope()->lookup(node.variable_name);
    if(!info)
    {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: variable '" + node.variable_name +  "' no definida en este scope.\n";
        return;
    }

    if(!ctx.type_registry.has_type(node.type_name))
    {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: tipo '" + node.type_name +  "' no definido.\n";
        return;
    }

}

void UsageCheckerVisitor::visit(TypeMember& node){
    node.accept(*this);
}

void UsageCheckerVisitor::visit(TypeNode& node){
    
    if(!ctx.type_registry.has_type(node.name))
    {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: el tipo '" + node.name +  "' no esta definido.\n";
        return;
    }


    push_current_type(ctx.type_registry.get_type(node.name));
    ctx.pushScope(node.scope);

    for (auto arg : *node.type_args){
        arg->accept(*this);
        if(errorFlag)
            return;
    }

    if(node.inherits != nullptr)
    {
        node.inherits->accept(*this);
        if(errorFlag)
            return;

        // Verificar que no se herede de tipos primitivos
        Type* parent = ctx.type_registry.get_type(node.inherits->parent_type);
        if (parent->is_primitive()) {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: No se puede heredar de tipo primitivo";
            return;
        }
    }
    else
    {
        Type* current = get_current_type();
        current->object_data.parent = ctx.object_type;
    }

    for(auto member: node.members){
        member->accept(*this);
        if(errorFlag)
            return;
    }

    pop_current_type();
    ctx.popScope();

}

void UsageCheckerVisitor::visit(InheritsNode& node){
    
    if(!ctx.type_registry.has_type(node.parent_type))
    {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: el tipo '" + node.parent_type +  "' no esta definido.\n";
        return;
    }

    Type* type = ctx.type_registry.get_type(node.parent_type);

    if(type->is_primitive())
    {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: no se puede heredar del tipo primitivo '" + node.parent_type + "' .\n";
        return;
    }

    Type* current = get_current_type();

    if (type->is_subtype_of(current))
    {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: herencia circular entre '" + node.parent_type + "' y " + current->name + "\n";
        return;
    }

    current->object_data.parent = type;

    if (node.parent_args) {
        for (auto arg : *node.parent_args) {
            arg->accept(*this);
            if (errorFlag) 
                return;
        }
    }

}

void UsageCheckerVisitor::visit(AttributeNode& node){
    // Procesar el inicializador si existe
    if (node.initializer) {
        node.initializer->accept(*this);
        if (errorFlag) return;
    }
}

void UsageCheckerVisitor::visit(MethodNode& node){
    
    if(!node.declared_type.empty() && !ctx.type_registry.has_type(node.declared_type))
    {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: el tipo '" + node.declared_type +  "' no esta definido.\n";
        return;
    }

    std::string method_full_name = get_current_type()->name + "." + node.getName();

    FunctionInfo* func = ctx.lookupFunction(method_full_name);
    
    func->node->accept(*this);

}

void UsageCheckerVisitor::visit(NewNode& node){
    
    if(!ctx.type_registry.has_type(node.type_name))
    {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: el tipo '" + node.type_name +  "' no esta definido.\n";
        return;
    }

    Type* type = ctx.type_registry.get_type(node.type_name);
    if (type->is_primitive()) {
        errorMsg = "[Line " + std::to_string(node.line) + "] Error: no se puede instanciar tipo primitivo '" + node.type_name + "'";
        errorFlag = true;
        return;
    }


    for(auto arg : *node.arguments)
    {
        arg->accept(*this);
        if (errorFlag) 
            return;
    }

}

void UsageCheckerVisitor::visit(MemberAccessNode& node){

    if(SelfNode* var = dynamic_cast<SelfNode*>(node.object))
    {
        var->accept(*this);
        if (errorFlag) 
            return;

    }
    else
    {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: los atributos de un tipo son privados. \n'";
        return;
    }
    
    if(get_current_type() == nullptr){
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: la instrucción ' self ' solo puede ser utilizado en una declaración de tipos. \n";
    }

    auto attrs_info = get_current_type()->object_data.attributes;
    
    if(attrs_info.find(node.member_name) == attrs_info.end())
    {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: el tipo ' " +  get_current_type()->name + "' no posee un atributo llamado ' " + node.member_name + " ' .\n'";
        return;
    }

}

void UsageCheckerVisitor::visit(SelfNode& node){
    
    if(get_current_type() == nullptr){
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: la instrucción ' self ' solo puede ser utilizado en una declaración de tipos. \n";
    }
}

void UsageCheckerVisitor::visit(BaseNode& node){

    if(get_current_type() == nullptr){
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: la instrucción ' self ' solo puede ser utilizado en una declaración de tipos. \n";
        return;
    }

    Type* current = get_current_type();
    
    if(current->object_data.parent == nullptr)
    {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: la instrucción ' base ' solo puede ser utilizado en tipos hijos diferentes del 'Object'. \n";
        return;
    }

    if (node.arguments) {
        for (auto arg : *node.arguments) {
            arg->accept(*this);
            if (errorFlag) 
                return;
        }
    }

}

// void UsageCheckerVisitor::visit(MethodCallNode& node){

//     std::cout<<"Calling "<< node.getMethodName() <<std::endl;

//     std::cout<<"1"<<std::endl;
//     node.object->accept(*this);
//     if (errorFlag) 
//             return;

//     std::cout<<"2"<<std::endl;
//     for (auto arg : node.arguments) {
//         arg->accept(*this);
//         if (errorFlag) 
//             return;
//     }

//     std::cout<<"3"<<std::endl;
//     if(get_current_type() == nullptr){
//         errorFlag = true;
//         errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: la instrucción ' self ' solo puede ser utilizado en una declaración de tipos. \n";
//     }

//     std::cout<<"4"<<std::endl;
//     auto methods_info = get_current_type()->object_data.methods;

//     std::cout<<"5"<<std::endl;
    
//     if(methods_info.find(node.getMethodName()) == methods_info.end())
//     {
//         errorFlag = true;
//         errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: el tipo ' " +  get_current_type()->name + "' no posee un método llamado ' " + node.getMethodName() + " ' .\n'";
//         return;
//     }
    
//     std::cout<<"Calling end "<< node.getMethodName() <<std::endl;
// }

void UsageCheckerVisitor::visit(MethodCallNode& node) {

    // 1. Evaluar el objeto primero
    node.object->accept(*this);
    if (errorFlag) return;

    // 4. Verificar argumentos
    for (auto arg : node.arguments) {
        arg->accept(*this);
        if (errorFlag) return;
    }
}
