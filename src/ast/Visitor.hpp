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
class IfNode;
class TypeMember;
class TypeNode;
class InheritsNode;
class AttributeNode;
class MethodNode;
class NewNode;
class MemberAccessNode;
class SelfNode;
class BaseNode;
class MethodCallNode;
class IsNode;
class AsNode;


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
    virtual void visit(IfNode& node) = 0;
    virtual void visit(TypeMember& node) = 0;
    virtual void visit(TypeNode& node) = 0;
    virtual void visit(InheritsNode& node) = 0;
    virtual void visit(AttributeNode& node) = 0;
    virtual void visit(MethodNode& node) = 0;
    virtual void visit(NewNode& node) = 0;
    virtual void visit(MemberAccessNode& node) = 0;
    virtual void visit(SelfNode& node) = 0;
    virtual void visit(BaseNode& node) = 0;
    virtual void visit(MethodCallNode& node) = 0;
    virtual void visit(IsNode& node) = 0;
    virtual void visit(AsNode& node) = 0;
    
};
