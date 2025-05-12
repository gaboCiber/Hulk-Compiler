#pragma once
#include "ast/Visitor.hpp"
#include "semantic/Context.hpp"

class UsageCheckerVisitor : public Visitor {
public:
    Context& ctx;

    UsageCheckerVisitor(Context& context);

    void visit(FloatNode& node) override;
    void visit(BoolNode& node) override;
    void visit(StringNode& node) override;
    void visit(UnaryOpNode& node) override;
    void visit(BinOpNode& node) override;
    void visit(VariableNode& node) override;
    void visit(LetInNode& node) override;
    void visit(BlockNode& node) override;

    bool hasError() const { return errorFlag; }
    const std::string& getError() const { return errorMsg; }

private:
    bool errorFlag = false;
    std::string errorMsg;
};
