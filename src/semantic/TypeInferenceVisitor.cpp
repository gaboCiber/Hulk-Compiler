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
            else if((info->type->is_primitive() || type->is_primitive()) && info->type != type && type != ctx.object_type)
            {
                errorFlag = true;
                errorMsg = "[Line " + std::to_string(var->line) + "] Error semántico: no se pudo inferir el tipo de la varaible '" + var->name + 
                "'. Tipo esperado: '" + Type::TypeToString(info->type) + "'. Tipo inferido: '" + Type::TypeToString(type) + ".\n";

            }
            else if(info->type != ctx.object_type && !info->type->is_primitive())
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
        if(MemberAccessNode* member = dynamic_cast<MemberAccessNode*>(node.left))
            typeL = typeR = typeNode = leftT;
        else
            typeL = typeR = typeNode = rightT; 
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
    
        if (checkVariableType && info->type == nullptr) {
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

        if(pair.first->declared_type != "")
            lastType = ctx.type_registry.get_type(pair.first->declared_type);
        
        SymbolInfo* info = ctx.currentScope()->lookup(pair.first->name);
        info->type = lastType;
    }
        
    node.block->accept(*this);
    ctx.popScope();
}

void TypeInferenceVisitor::visit(FunctionNode& node) {
    ctx.pushScope(node.scope);
    
    for (auto& arg: node.args)
    {
        if(!arg->declared_type.empty())
        {
            SymbolInfo* info = ctx.currentScope()->lookup(arg->name);
            putTypeOnVariables(arg, ctx.type_registry.get_type(arg->declared_type));
        }
    }

    node.block->accept(*this);
    
    Type* inferedType = lastType;

    checkVariableType = true;
    for (auto& arg: node.args)
    {
        arg->accept(*this);
        if(errorFlag)
            return;
    }
    checkVariableType = false;

    FunctionInfo* info = ctx.lookupFunction(node.name);
    
    if(node.declared_type != "")
        lastType = ctx.type_registry.get_type(node.declared_type);
    else
        lastType = inferedType;

    if(lastType == nullptr)
    {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: no fue posible inferir el tipo de la función '" + node.name + "'.\n";
        return;
    }

    info->returnType = lastType;
    
    ctx.popScope();
}

void TypeInferenceVisitor::visit(ProgramNode& node) {
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

    if(node.functionName != "print")
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
    for (size_t i = 0; i < branchTypes.size(); i++) {
        lastType = ctx.type_registry.findLowestCommonAncestor(lastType, branchTypes[i]);
    }

    node.returnType = lastType;
}

void TypeInferenceVisitor::visit(TypeMember& node){
    node.accept(*this);
}

void TypeInferenceVisitor::visit(TypeNode& node){
    
    // Registrar el tipo ya se hizo en DefinitionVisito

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


    checkVariableType = true;
    for (auto arg : *node.type_args){
        arg->accept(*this);
        if(errorFlag)
            return;
    }
    checkVariableType = false;
    
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
    
    node.initializer->accept(*this);
    if (errorFlag) 
        return;

    Type* attributeType = lastType;

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
    else if(!lastType)
    {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error: no se pudo inferir el tipo del atributo '" + node.getName() + "'";
        return;
    }

    if (!get_current_type()) {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + 
                  "] Error: no hay tipo actual para definir atributos";
        return;
    }

    get_current_type()->object_data.attributes[node.getName()] = attributeType;
    lastType = attributeType;
}

void TypeInferenceVisitor::visit(MethodNode& node){
    
    std::string method_full_name = get_current_type()->name + "." + node.getName();

    FunctionInfo* func = ctx.lookupFunction(method_full_name);
    if (!func) {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Método no registrado: " + method_full_name;
        return;
    }
    
    func->node->accept(*this);
    if(errorFlag)
        return;
    
    Type* inferedReturn = lastType;

    ctx.pushScope(func->node->scope);
    
    auto method_info = get_current_type()->object_data.methods[node.getName()];
    
    checkVariableType = true;
    for (auto& arg: func->node->args)
    {
        arg->accept(*this);
        if(errorFlag)
            return;

    }
    checkVariableType = false;
    

    ctx.popScope();

    lastType = method_info->return_type ? method_info->return_type : inferedReturn;
}

void TypeInferenceVisitor::visit(NewNode& node){
    
    Type* type = ctx.type_registry.get_type(node.type_name);

    for (auto arg : *node.arguments) {
        arg->accept(*this);
        if (errorFlag) return;
    }

    lastType = type;

}


void TypeInferenceVisitor::visit(MemberAccessNode& node) {
    node.object->accept(*this);
    if (errorFlag) return;

    Type* objectType = lastType;
    
    if (!objectType) {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error: el objeto no es de un tipo válido";
        return;
    }

    // Verifica que el atributo exista
    auto& attributes = objectType->object_data.attributes;
    if (attributes.find(node.member_name) == attributes.end()) {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error: el atributo '" + node.member_name + "' no existe en el tipo '" + objectType->name + "'";
        return;
    }

    lastType = attributes[node.member_name];
    if (!lastType) {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error: no se pudo inferir el tipo del atributo '" + node.member_name + "'";
    }
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
        errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: 'base' inválido en este contexto";
        return;
    }
    
    // Procesar argumentos si existen
    if (node.arguments) {
        for (auto arg : *node.arguments) {

            arg->accept(*this);
            if (errorFlag) return;
        }
    }
    
    std::string realFuncName = ctx.currentScope()->functionName.substr(get_current_type()->name.size() + 1);
    
    lastType = current->object_data.parent->object_data.methods[realFuncName]->return_type;
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
        errorMsg = "[Line " + std::to_string(node.line) + "] El tipo '" + object_type->name + "' no tiene método '" + node.getMethodName() + "'";
        return;
    }

    lastType = method_type->return_type;
}