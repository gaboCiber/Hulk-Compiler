#pragma once

class FloatNode;
class BinOpNode;

class Visitor {
public:
    virtual ~Visitor() = default;

    virtual void visit(FloatNode& node) = 0;
    virtual void visit(BinOpNode& node) = 0;
};
