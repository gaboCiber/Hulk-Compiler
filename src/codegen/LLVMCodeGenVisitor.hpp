#pragma once

#include "ast/Visitor.hpp"
#include "semantic/Context.hpp"

#include <llvm/IR/LLVMContext.h>  // ✅ define llvm::LLVMContext
#include <llvm/IR/IRBuilder.h>    // ✅ define llvm::IRBuilder
#include <llvm/IR/Module.h>       // ✅ define llvm::Module
#include <llvm/IR/Value.h>        // ✅ define llvm::Value
#include <llvm/IR/Type.h>

#include <string>
#include <memory>  // para std::unique_ptr

// Definir constantes para los tipos
enum HulkType {
    HULK_FLOAT = 0,
    HULK_BOOL = 1,
    HULK_STRING = 2
};

class LLVMCodeGenVisitor : public Visitor {
public:
    llvm::LLVMContext context;
    llvm::IRBuilder<> builder;
    std::unique_ptr<llvm::Module> module;

    llvm::Value* result;

    Context& ctx;

    LLVMCodeGenVisitor(const std::string& moduleName, Context& c);
    llvm::Type* llvmType(ASTNode& node,Type t);

    void visit(FloatNode& node) override;
    void visit(StringNode& node) override;
    void visit(BoolNode& node) override;
    void visit(BinOpNode& node) override;
    void visit(UnaryOpNode& node) override;
    void visit(BlockNode& node) override;
    void visit(VariableNode& node) override;
    void visit(LetInNode& node) override;
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

    llvm::Module* getModule() const;

private:
    llvm::Value* generateBuiltinCall(const std::string& name, const std::vector<llvm::Value*>& args);
    llvm::Function* getPrintFunctionForType(llvm::Type *type);
    llvm::Function* getBuiltinFunction(const std::string& name, llvm::Type* returnType, const std::vector<llvm::Type*>& argTypes);
    llvm::Value* getTypeCode(llvm::Type* type);

};
