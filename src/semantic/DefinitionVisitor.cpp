#include "semantic/DefinitionVisitor.hpp"

DefinitionVisitor::DefinitionVisitor(Context& context) : ctx(context) {}

void DefinitionVisitor::visit(FloatNode&) {}
void DefinitionVisitor::visit(BoolNode&) {}
void DefinitionVisitor::visit(StringNode&) {}

void DefinitionVisitor::visit(UnaryOpNode& node) {
    node.node->accept(*this);
}

void DefinitionVisitor::visit(BinOpNode& node) {
    node.left->accept(*this);

    if(errorFlag)
        return;

    node.right->accept(*this);
}

void DefinitionVisitor::visit(VariableNode&) {
    // No hace nada aquí, se chequeará en la segunda pasada
}

void DefinitionVisitor::visit(LetInNode& node) {
    node.scope = new Scope(ctx.currentScope());
    ctx.pushScope(node.scope);
    for(auto& pair : node.bindings)
    {
        pair.second->accept(*this);
        if(errorFlag)
            return;

        node.scope->define(pair.first->name, pair.second);
            
    }
    
    node.block->accept(*this);
    ctx.popScope();
}

void DefinitionVisitor::visit(BlockNode& node) {
    // DUDA: un bloque de codigo crea un nuevo scope
    for (auto stmt : node.statements) {
        stmt->accept(*this);
        if(errorFlag)
            return;
    }
}

void DefinitionVisitor::visit(FunctionNode& node) {
    node.scope = new Scope(ctx.currentScope());
    ctx.pushScope(node.scope);
    for(auto& arg : node.args)
    {
        node.scope->define(arg->name, nullptr);  
    }
    
    node.block->accept(*this);
    ctx.popScope();
}

void DefinitionVisitor::visit(ProgramNode& node) {
    for (auto stmt : node.functions) {
        
        if (!ctx.defineFunction(stmt->name, stmt)) {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(stmt->line) + "] Error semántico: la función '" + stmt->name + "' ya fue definida.";
            return;
        }
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

void DefinitionVisitor::visit(CallFuncNode& node){
    
    for (auto args : node.arguments) {
        args->accept(*this);
        if(errorFlag)
            return;
    }
}

void DefinitionVisitor::visit(WhileNode& node) {
    node.condition->accept(*this);
    if(errorFlag)
            return;

    node.body->accept(*this);
    if(errorFlag)
            return;
}

void DefinitionVisitor::visit(IfNode& node) {
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

void DefinitionVisitor::visit(TypeMember& node){
    throw std::runtime_error("Se metio en TypeMember");
}

void DefinitionVisitor::visit(TypeNode& node){

    auto msg = ctx.type_registry.register_user_type(node.name, node.inherits ? node.inherits->parent_type : "Object");

    if (msg != "") {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: " + msg;
        return;
    }

    push_current_type(node.name);

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
        
    for(auto member: node.members){
        member->accept(*this);
        if(errorFlag)
            return;
    }

    pop_current_type();

}

void DefinitionVisitor::visit(InheritsNode& node){
    // La validación de herencia ya se hizo en TypeNode
    // Solo necesitamos procesar los argumentos del padre si existen
    if (node.parent_args) {
        for (auto arg : *node.parent_args) {
            arg->accept(*this);
            if (errorFlag) 
                return;
        }
    }
}

void DefinitionVisitor::visit(AttributeNode& node){
    // Registrar el atributo en el TypeRegistry (tipo se inferirá más tarde)
    ctx.type_registry.register_attribute(get_current_type(), node.getName(), nullptr);
    
    // Procesar el inicializador si existe
    if (node.initializer) {
        node.initializer->accept(*this);
        if (errorFlag) return;
    }
}

void DefinitionVisitor::visit(MethodNode& node) {
    std::string method_full_name = get_current_type() + "." + node.getName();
    
    // Registrar la función como una función normal
    auto func_node = new FunctionNode(method_full_name, *node.parameters, node.body,node.line );
    
    if (!ctx.defineFunction(method_full_name, func_node)) {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Método ya definido: " + node.getName();
        return;
    }
    
    // Registrar el método en el TypeRegistry
    FunctionType* method_type = new FunctionType();
    method_type->return_type = nullptr; // Se inferirá más tarde
    
    for (auto param : *node.parameters) {
        method_type->parameter_types.push_back(nullptr); // Tipos temporales
    }
    
    ctx.type_registry.register_method(get_current_type(), node.getName(), method_type);
    
    // Procesar la función
    func_node->accept(*this);
    if (errorFlag) {
        return;
    }
}

void DefinitionVisitor::visit(NewNode& node){
    // Procesar argumentos del constructor
    if (node.arguments) {
        for (auto arg : *node.arguments) {
            arg->accept(*this);
            if (errorFlag) return;
        }
    }
}

void DefinitionVisitor::visit(MemberAccessNode& node){
    // Procesar el objeto base
    node.object->accept(*this);
    if (errorFlag) 
        return;

}

void DefinitionVisitor::visit(SelfNode& node){
    // No hay nada que registrar en la fase de definición
    // La validación se hará en TypeCheckerVisitor
}

void DefinitionVisitor::visit(BaseNode& node){
    // Procesar argumentos si existen
    if (node.arguments) {
        for (auto arg : *node.arguments) {
            arg->accept(*this);
            if (errorFlag) return;
        }
    }
}

void DefinitionVisitor::visit(MethodCallNode& node){
    // Procesar el objeto receptor
    node.object->accept(*this);
    if (errorFlag) return;
    
    // Procesar argumentos
    for (auto arg : node.arguments) {
        arg->accept(*this);
        if (errorFlag) return;
    }
}
    
