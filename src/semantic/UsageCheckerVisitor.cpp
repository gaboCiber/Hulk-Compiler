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

    if(node.declared_type != "")
    {
        if(!ctx.type_registry.has_type(node.declared_type))
        {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: el tipo '" + node.declared_type +  "' no esta definido.\n";
        }
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
    
    if(node.declared_type != "")
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

void UsageCheckerVisitor::visit(TypeMember& node){
    throw std::runtime_error("Se metio en TypeMember");
}

void UsageCheckerVisitor::visit(TypeNode& node){
    
    push_current_type(node.name);
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
    
    if(!ctx.type_registry.has_type(node.declared_type))
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
        errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: los atributos de un tipo son providos. \n'";
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

void UsageCheckerVisitor::visit(MethodCallNode& node){

    node.object->accept(*this);
    if (errorFlag) 
            return;

    for (auto arg : node.arguments) {
        arg->accept(*this);
        if (errorFlag) 
            return;
    }
    
    if(get_current_type() == nullptr){
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: la instrucción ' self ' solo puede ser utilizado en una declaración de tipos. \n";
    }

    auto methods_info = get_current_type()->object_data.methods;
    
    if(methods_info.find(node.getMethodName()) == methods_info.end())
    {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: el tipo ' " +  get_current_type()->name + "' no posee un método llamado ' " + node.getMethodName() + " ' .\n'";
        return;
    }
    
}

void TypeInferenceVisitor::visit(MethodCallNode& node) {
    node.object->accept(*this);
    if (errorFlag) return;
    
    Type* object_type = lastType;
    if (!object_type || object_type->is_primitive()) {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] No se puede llamar métodos en tipos primitivos";
        return;
    }

    // Buscar el método en la jerarquía de tipos
    Type* current = object_type;
    FunctionType* method_type = nullptr;
    while (current != nullptr) {
        auto it = current->object_data.methods.find(node.getMethodName());
        if (it != current->object_data.methods.end()) {
            method_type = it->second;
            break;
        }
        current = current->object_data.parent;
    }

    if (!method_type) {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] El tipo '" + 
                  object_type->name + "' no tiene método '" + node.getMethodName() + "'";
        return;
    }

    // Procesar argumentos
    if (node.arguments.size() != method_type->parameter_types.size()) {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Número incorrecto de argumentos para '" + 
                  node.getMethodName() + "'. Esperados: " + std::to_string(method_type->parameter_types.size()) + 
                  ", recibidos: " + std::to_string(node.arguments.size());
        return;
    }

    for (size_t i = 0; i < node.arguments.size(); i++) {
        node.arguments[i]->accept(*this);
        if (errorFlag) return;
        // La compatibilidad de tipos se verificará en el TypeChecker
    }

    lastType = method_type->return_type;
}
    
