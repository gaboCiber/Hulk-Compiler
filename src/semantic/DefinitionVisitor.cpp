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
    for (auto stmt : node.statements) {
        stmt->accept(*this);
        if(errorFlag)
            return;
    }
}
