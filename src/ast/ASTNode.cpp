#include "ASTNode.hpp"
#include "Visitor.hpp"

// FloatNode implementation
FloatNode::FloatNode(float v, int l) : ASTNode(l), value(v) {}

void FloatNode::print(int indent) const {
    std::cout << std::string(indent, ' ') << "Float(" << value << ")\n";
}

void FloatNode::accept(Visitor& visitor) {
    visitor.visit(*this);
}

// BoolNode implementation
BoolNode::BoolNode(bool v, int l) : ASTNode(l), value(v) {}

void BoolNode::print(int indent) const {
    std::cout << std::string(indent, ' ') << "Bool(" << value << ")\n";
}

void BoolNode::accept(Visitor& visitor) {
    visitor.visit(*this);
}

// StringNode implementation
StringNode::StringNode(const std::string& v, int l) : ASTNode(l), value(v) {}

void StringNode::print(int indent) const {
    std::cout << std::string(indent, ' ') << "String(\"" << value << "\")\n";
}

void StringNode::accept(Visitor& visitor) {
    visitor.visit(*this);
}

// UnaryOpNode implementation
UnaryOpNode::UnaryOpNode(const std::string& o, ASTNode* n, int l) 
: ASTNode(l), op(o), node(n) {}

void UnaryOpNode::print(int indent) const {
    std::cout << std::string(indent, ' ') << "UnaryOp(" << op << ")\n";
    node->print(indent + 2);
}

void UnaryOpNode::accept(Visitor& visitor) {
    visitor.visit(*this);
}

UnaryOpNode::~UnaryOpNode() {
    delete node;
}


// BinOpNode implementation
BinOpNode::BinOpNode(const std::string& o, ASTNode* l, ASTNode* r, int ln) 
    : ASTNode(ln), op(o), left(l), right(r) {}

void BinOpNode::print(int indent) const {
    std::cout << std::string(indent, ' ') << "BinOp(" << op << ")\n";
    left->print(indent + 2);
    right->print(indent + 2);
}

void BinOpNode::accept(Visitor& visitor) {
    visitor.visit(*this);
}

BinOpNode::~BinOpNode() {
    delete left;
    delete right;
}

// BlockNode implementation
void BlockNode::push_back(ASTNode* node) {
    statements.push_back(node);
}

void BlockNode::print(int indent) const {
    std::cout << std::string(indent, ' ') << "Block:\n";
    for (const auto& statement : statements) {
        statement->print(indent + 2);
    }
}

void BlockNode::accept(Visitor& visitor) {
    visitor.visit(*this);
}

BlockNode::~BlockNode() {
    for (auto stmt : statements) {
        delete stmt;
    }
}

// VariableNode Implementation
VariableNode::VariableNode(const std::string& n, int l) : name(n), ASTNode(l) {}

void VariableNode::print(int indent) const {
    std::cout << std::string(indent, ' ') << "Variable(" << name << ")\n";
}

void VariableNode::accept(Visitor& visitor) {
    visitor.visit(*this);
}

// LetInNode Implementation
LetInNode::LetInNode(const std::vector<std::pair<VariableNode*, ASTNode*>>& b, BlockNode* blk, int l)
    : ASTNode(l), bindings(b), block(blk) {}

void LetInNode::print(int indent) const {
    std::cout << std::string(indent, ' ') << "Let\n";
    for (const auto& pair : bindings) {
        std::cout << std::string(indent + 2, ' ') << pair.first->name << " =\n";
        pair.second->print(indent + 4);
    }
    std::cout << std::string(indent, ' ') << "In\n";
    block->print(indent + 2);
}

void LetInNode::accept(Visitor& visitor) {
    visitor.visit(*this);
}

LetInNode::~LetInNode() {
    for (auto& pair : bindings) {
        delete pair.first;
        delete pair.second;
    }
    delete block;
}

// FunctionNode Implementation
FunctionNode::FunctionNode(const std::string& n, const std::vector<VariableNode*>& a, BlockNode* blk, int l)
    : name(n), args(a), block(blk), ASTNode(l) {}

void FunctionNode::print(int indent) const {
    std::cout << std::string(indent, ' ') << "Function " << name << " ( ";
    for (const auto& a : args) {
        std::cout << a->name << " ";
    }
    std::cout << std::string(indent, ' ') << ")\n";
    block->print(indent + 2);
}

void FunctionNode::accept(Visitor& visitor) {
    visitor.visit(*this);
}

FunctionNode::~FunctionNode() {
    for (auto& a : args) {
        delete a;
    }

    delete block;
} 

// En ASTNode.cpp - Implementación de ProgramNode

void ProgramNode::print(int indent) const {
    std::cout << std::string(indent, ' ') << "Program:\n";
    
    // Imprimir funciones primero
    for (const auto& func : functions) {
        func->print(indent + 2);
    }
    
    // Imprimir statements (bloques y líneas)
    for (const auto& stmt : statements) {
        stmt->print(indent + 2);
    }
}

void ProgramNode::accept(Visitor& visitor) {
    visitor.visit(*this);
}

ProgramNode::~ProgramNode() {
    // Liberar memoria de funciones
    for (auto func : functions) {
        delete func;
    }
    
    // Liberar memoria de statements
    for (auto stmt : statements) {
        delete stmt;
    }
}

void CallFuncNode::print(int indent) const {
    std::cout << std::string(indent, ' ') << "CallFunc: " << functionName << "\n";
    for (const auto& arg : arguments) {
        arg->print(indent + 2);
    }
}

void CallFuncNode::accept(Visitor& visitor) {
    visitor.visit(*this);
}

CallFuncNode::~CallFuncNode() {
    for (auto arg : arguments) {
        delete arg;
    }
}

WhileNode::WhileNode(ASTNode* cond, BlockNode* body, int line)
    : ASTNode(line), condition(cond), body(body) {}

void WhileNode::print(int indent) const {
    std::cout << std::string(indent, ' ') << "While:\n";
    std::cout << std::string(indent + 2, ' ') << "Condition:\n";
    condition->print(indent + 4);
    std::cout << std::string(indent + 2, ' ') << "Body:\n";
    body->print(indent + 4);
}

void WhileNode::accept(Visitor& visitor) {
    visitor.visit(*this);
}

WhileNode::~WhileNode() {
    delete condition;
    delete body;
}