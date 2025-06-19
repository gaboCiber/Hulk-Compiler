#pragma once
#include "ast/Visitor.hpp"
#include "semantic/Context.hpp"
#include <stack>

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
    void visit(FunctionNode& node) override;
    void visit(ProgramNode& node) override;
    void visit(CallFuncNode& node) override;
    void visit(WhileNode& node) override;
    void visit(IfNode& node) override;
    void visit(TypeMember& node) override;
    void visit(TypeNode& node) override;
    void visit(InheritsNode& node) override;
    void visit(AttributeNode& node) override;
    void visit(MethodNode& node) override;
    void visit(NewNode& node) override;
    void visit(MemberAccessNode& node) override;
    void visit(SelfNode& node) override;
    void visit(BaseNode& node) override;
    void visit(MethodCallNode& node) override;
    void visit(IsNode& node) override;
    void visit(AsNode& node) override;

    bool hasError() const { return errorList.size() > 0; }
    void printError() { 
        std::string finalMsg = "";
        for (auto msg : errorList)
            finalMsg += "❌ " + msg;  

        std::cerr<<finalMsg;  
    }

private:
    bool errorFlag = false;
    std::string errorMsg;
    std::vector<std::string> errorList;


    std::stack<Type*> current_type_stack;
    
    void push_current_type(Type* type) {
        current_type_stack.push(type);
    }
    
    void pop_current_type() {
        current_type_stack.pop();
    }
    
    Type* get_current_type() const {
        return current_type_stack.empty() ? nullptr : current_type_stack.top();
    }
};
