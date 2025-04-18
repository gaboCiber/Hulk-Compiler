#pragma once
#include "ast/Visitor.hpp"

class SemanticChecker : public Visitor {
public:
    void visit(FloatNode& node) override;
    void visit(BinOpNode& node) override;
};
