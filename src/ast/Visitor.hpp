#pragma once

class FloatNode;
class BoolNode;
class UnaryOpNode;
class BinOpNode;

class Visitor {
public:
    virtual ~Visitor() = default;

    virtual void visit(FloatNode& node) = 0;
    virtual void visit(BoolNode& node) = 0;
    virtual void visit(BinOpNode& node) = 0;
    virtual void visit(UnaryOpNode& node) = 0;
};
