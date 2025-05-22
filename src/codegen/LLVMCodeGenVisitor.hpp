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

class LLVMCodeGenVisitor : public Visitor {
public:
    llvm::LLVMContext context;
    llvm::IRBuilder<> builder;
    std::unique_ptr<llvm::Module> module;

    llvm::Value* result;

    Context& ctx;

    LLVMCodeGenVisitor(const std::string& moduleName, Context& c);
    llvm::Type* llvmType(Type t);

    std::string llvmTypeName(Type t);

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

    llvm::Module* getModule() const;
};
