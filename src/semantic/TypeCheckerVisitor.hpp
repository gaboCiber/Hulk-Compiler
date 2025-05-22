#pragma once

#include "ast/Visitor.hpp"
#include "semantic/Context.hpp"
#include <string>

class TypeCheckerVisitor : public Visitor {
public:

    Context& ctx;

    TypeCheckerVisitor(Context& context);

    // Visitor interface
    void visit(FloatNode& node) override;
    void visit(BoolNode& node) override;
    void visit(StringNode& node) override;
    void visit(UnaryOpNode& node) override;
    void visit(BinOpNode& node) override;
    void visit(BlockNode& node) override;
    void visit(VariableNode& node) override;
    void visit(LetInNode& node) override;
    void visit(FunctionNode& node) override;
    void visit(ProgramNode& node) override;
    void visit(CallFuncNode& node) override;

    bool hasError() const { return errorFlag; }
    const std::string& getError() const { return errorMsg; }
    Type getType() const { return lastType; }

private:
    Type lastType = Type::Float;
    bool errorFlag = false;
    std::string errorMsg;
};
