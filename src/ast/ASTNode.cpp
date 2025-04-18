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