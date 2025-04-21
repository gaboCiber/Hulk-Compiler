#include "ASTNode.hpp"
#include "Visitor.hpp"

// FloatNode implementation
FloatNode::FloatNode(float v) : value(v) {}

void FloatNode::print(int indent) const {
    std::cout << std::string(indent, ' ') << "Float(" << value << ")\n";
}

void FloatNode::accept(Visitor& visitor) {
    visitor.visit(*this);
}

// BoolNode implementation
BoolNode::BoolNode(bool v) : value(v) {}

void BoolNode::print(int indent) const {
    std::cout << std::string(indent, ' ') << "Bool(" << value << ")\n";
}

void BoolNode::accept(Visitor& visitor) {
    visitor.visit(*this);
}

// StringNode implementation
StringNode::StringNode(const std::string& v) : value(v) {}

void StringNode::print(int indent) const {
    std::cout << std::string(indent, ' ') << "String(\"" << value << "\")\n";
}

void StringNode::accept(Visitor& visitor) {
    visitor.visit(*this);
}

// UnaryOpNode implementation
UnaryOpNode::UnaryOpNode(const std::string& o, ASTNode* n)
    : op(o), node(n) {}

void UnaryOpNode::print(int indent) const {
    std::cout << std::string(indent, ' ') << "UnaryOp(" << op << ")\n";
    node->print(indent + 2);
}

void UnaryOpNode::accept(Visitor& visitor) {
    visitor.visit(*this);
}


// BinOpNode implementation
BinOpNode::BinOpNode(const std::string& o, ASTNode* l, ASTNode* r)
    : op(o), left(l), right(r) {}

void BinOpNode::print(int indent) const {
    std::cout << std::string(indent, ' ') << "BinOp(" << op << ")\n";
    left->print(indent + 2);
    right->print(indent + 2);
}

void BinOpNode::accept(Visitor& visitor) {
    visitor.visit(*this);
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