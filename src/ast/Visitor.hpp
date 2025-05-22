#pragma once

class FloatNode;
class BoolNode;
class StringNode;
class UnaryOpNode;
class BinOpNode;
class BlockNode;
class VariableNode;
class LetInNode;
class FunctionNode;
class ProgramNode;
class CallFuncNode;
class WhileNode;

class Visitor {
public:
    virtual ~Visitor() = default;

    virtual void visit(FloatNode& node) = 0;
    virtual void visit(BoolNode& node) = 0;
    virtual void visit(StringNode& node) = 0;
    virtual void visit(BinOpNode& node) = 0;
    virtual void visit(UnaryOpNode& node) = 0;
    virtual void visit(BlockNode& node) = 0;
    virtual void visit(VariableNode& node) = 0;
    virtual void visit(LetInNode& node) = 0;
    virtual void visit(FunctionNode& node) = 0;
    virtual void visit(ProgramNode& node) = 0;
    virtual void visit(CallFuncNode& node) = 0;
    virtual void visit(WhileNode& node) = 0;
    
};
