#pragma once

#include "ast/Visitor.hpp"
#include <string>

enum class Type { Float, Bool, String };

class TypeCheckerVisitor : public Visitor {
public:
    // Visitor interface
    void visit(FloatNode& node) override;
    void visit(BoolNode& node) override;
    void visit(StringNode& node) override;
    void visit(UnaryOpNode& node) override;
    void visit(BinOpNode& node) override;

    bool hasError() const { return errorFlag; }
    const std::string& getError() const { return errorMsg; }
    Type getType() const { return lastType; }

private:
    Type lastType = Type::Float;
    bool errorFlag = false;
    std::string errorMsg;
};
