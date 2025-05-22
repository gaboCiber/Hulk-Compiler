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
}

void UsageCheckerVisitor::visit(LetInNode& node) {
    ctx.pushScope(node.scope);
    for (auto& pair: node.bindings)
    {
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
    ctx.pushScope(node.scope);
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

}