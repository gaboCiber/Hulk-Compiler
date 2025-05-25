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
        errorMsg = "[Line " + std::to_string(stmt->line) + "] Error: la función '" + stmt->name + "' ya fue definida.";
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

}

void DefinitionVisitor::visit(TypeNode& node){

}

void DefinitionVisitor::visit(InheritsNode& node){

}

void DefinitionVisitor::visit(AttributeNode& node){

}

void DefinitionVisitor::visit(MethodNode& node){

}

void DefinitionVisitor::visit(NewNode& node){

}

void DefinitionVisitor::visit(MemberAccessNode& node){

}

void DefinitionVisitor::visit(SelfNode& node){

}

void DefinitionVisitor::visit(BaseNode& node){

}

void DefinitionVisitor::visit(MethodCallNode& node){

}
    
