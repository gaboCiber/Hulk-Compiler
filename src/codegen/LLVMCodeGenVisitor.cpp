#include "codegen/LLVMCodeGenVisitor.hpp"
#include "ast/ASTNode.hpp"
#include <llvm/IR/Constants.h>
#include <llvm/IR/Instructions.h>
#include <cmath>

LLVMCodeGenVisitor::LLVMCodeGenVisitor(const std::string& moduleName)
    : builder(context), module(std::make_unique<llvm::Module>(moduleName, context)), result(nullptr)
{
    // Crear función float @main()
    llvm::FunctionType* funcType = llvm::FunctionType::get(builder.getFloatTy(), false);
    llvm::Function* mainFunc = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, "main", module.get()
    );

    // Crear bloque de entrada
    llvm::BasicBlock* entry = llvm::BasicBlock::Create(context, "entry", mainFunc);
    builder.SetInsertPoint(entry);
}

llvm::Module* LLVMCodeGenVisitor::getModule() const {
    return module.get();
}

void LLVMCodeGenVisitor::visit(FloatNode& node) {
    result = llvm::ConstantFP::get(context, llvm::APFloat(node.value));
}

void LLVMCodeGenVisitor::visit(BoolNode& node) {
    // Convertimos bool a float (0.0 o 1.0)
    result = llvm::ConstantFP::get(context, llvm::APFloat(node.value ? 1.0f : 0.0f));
}

void LLVMCodeGenVisitor::visit(UnaryOpNode& node) {
    node.node->accept(*this);
    llvm::Value* operand = result;

    if (node.op == "-") {
        result = builder.CreateFNeg(operand, "negtmp");
    }
    else if (node.op == "!") {
        // Negación lógica: convertimos de float a i1
        llvm::Value* cmp = builder.CreateFCmpONE(operand, llvm::ConstantFP::get(context, llvm::APFloat(0.0f)), "cmptmp");
        llvm::Value* notVal = builder.CreateNot(cmp, "nottmp");
        result = builder.CreateUIToFP(notVal, builder.getFloatTy(), "bool2float");
    }
}

void LLVMCodeGenVisitor::visit(BinOpNode& node) {
    node.left->accept(*this);
    llvm::Value* lhs = result;

    node.right->accept(*this);
    llvm::Value* rhs = result;

    if (node.op == "+")
        result = builder.CreateFAdd(lhs, rhs, "addtmp");
    else if (node.op == "-")
        result = builder.CreateFSub(lhs, rhs, "subtmp");
    else if (node.op == "*")
        result = builder.CreateFMul(lhs, rhs, "multmp");
    else if (node.op == "/")
        result = builder.CreateFDiv(lhs, rhs, "divtmp");
    else if (node.op == "^") {
        // Potencia: no hay instrucción directa en LLVM IR
        llvm::Function* powf = llvm::Intrinsic::getDeclaration(module.get(), llvm::Intrinsic::pow, { builder.getFloatTy() });
        result = builder.CreateCall(powf, { lhs, rhs }, "powtmp");
    }
    else if (node.op == ">") {
        llvm::Value* cmp = builder.CreateFCmpOGT(lhs, rhs, "cmptmp");
        result = builder.CreateUIToFP(cmp, builder.getFloatTy(), "bool2float");
    }
    else if (node.op == "<") {
        llvm::Value* cmp = builder.CreateFCmpOLT(lhs, rhs, "cmptmp");
        result = builder.CreateUIToFP(cmp, builder.getFloatTy(), "bool2float");
    }
    else if (node.op == ">=") {
        llvm::Value* cmp = builder.CreateFCmpOGE(lhs, rhs, "cmptmp");
        result = builder.CreateUIToFP(cmp, builder.getFloatTy(), "bool2float");
    }
    else if (node.op == "<=") {
        llvm::Value* cmp = builder.CreateFCmpOLE(lhs, rhs, "cmptmp");
        result = builder.CreateUIToFP(cmp, builder.getFloatTy(), "bool2float");
    }
    else if (node.op == "==") {
        llvm::Value* cmp = builder.CreateFCmpOEQ(lhs, rhs, "eqtmp");
        result = builder.CreateUIToFP(cmp, builder.getFloatTy(), "bool2float");
    }
    else if (node.op == "!=") {
        llvm::Value* cmp = builder.CreateFCmpONE(lhs, rhs, "neqtmp");
        result = builder.CreateUIToFP(cmp, builder.getFloatTy(), "bool2float");
    }
    else if (node.op == "&") {
        // Comparaciones booleanas: convertimos de float a i1
        llvm::Value* lhsBool = builder.CreateFCmpONE(lhs, llvm::ConstantFP::get(context, llvm::APFloat(0.0f)), "lhs_bool");
        llvm::Value* rhsBool = builder.CreateFCmpONE(rhs, llvm::ConstantFP::get(context, llvm::APFloat(0.0f)), "rhs_bool");
        llvm::Value* andVal = builder.CreateAnd(lhsBool, rhsBool, "andtmp");
        result = builder.CreateUIToFP(andVal, builder.getFloatTy(), "bool2float");
    }
    else if (node.op == "|") {
        llvm::Value* lhsBool = builder.CreateFCmpONE(lhs, llvm::ConstantFP::get(context, llvm::APFloat(0.0f)), "lhs_bool");
        llvm::Value* rhsBool = builder.CreateFCmpONE(rhs, llvm::ConstantFP::get(context, llvm::APFloat(0.0f)), "rhs_bool");
        llvm::Value* orVal = builder.CreateOr(lhsBool, rhsBool, "ortmp");
        result = builder.CreateUIToFP(orVal, builder.getFloatTy(), "bool2float");
    }  
}
