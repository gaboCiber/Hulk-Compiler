#include "codegen/LLVMCodeGenVisitor.hpp"
#include "ast/ASTNode.hpp"
#include <llvm/IR/Constants.h>
#include <llvm/IR/Instructions.h>
#include <cmath>

LLVMCodeGenVisitor::LLVMCodeGenVisitor(const std::string& moduleName)
    : builder(context), module(std::make_unique<llvm::Module>(moduleName, context)), result(nullptr)
{
    // Crear función float @main()
    llvm::FunctionType* funcType = llvm::FunctionType::get(builder.getInt32Ty(), false);
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

void LLVMCodeGenVisitor::visit(StringNode& node) {
    // Crea una constante global de tipo string
    llvm::Constant* strConstant = llvm::ConstantDataArray::getString(context, node.value, true);

    llvm::ArrayType* strType = llvm::ArrayType::get(llvm::Type::getInt8Ty(context), node.value.size() + 1);
    llvm::GlobalVariable* strVar = new llvm::GlobalVariable(
        *module,
        strType,
        true,  // constante
        llvm::GlobalValue::PrivateLinkage,
        strConstant,
        ".str"
    );

    // Obtener puntero a primer carácter (i8*) con GEP
    llvm::Value* strPtr = builder.CreateInBoundsGEP(
        strType,
        strVar,
        { llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0),
          llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0) },
        "strptr"
    );

    result = strPtr;  // guarda puntero a string como resultado
}


void LLVMCodeGenVisitor::visit(UnaryOpNode& node) {
    node.node->accept(*this);
    llvm::Value* operand = result;

    if (node.op == "-") {
        result = builder.CreateFNeg(operand, "negtmp");
    }
    else if (node.op == "!") {
        llvm::Value* isTrue = builder.CreateFCmpONE(
            operand,
            llvm::ConstantFP::get(context, llvm::APFloat(0.0f)),
            "isTrue"
        );
        result = builder.CreateNot(isTrue, "nottmp");
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
        result = builder.CreateFCmpOGT(lhs, rhs, "cmptmp");
    }
    else if (node.op == "<") {
        result = builder.CreateFCmpOLT(lhs, rhs, "cmptmp");
    }
    else if (node.op == ">=") {
        result = builder.CreateFCmpOGE(lhs, rhs, "cmptmp");
    }
    else if (node.op == "<=") {
        result = builder.CreateFCmpOLE(lhs, rhs, "cmptmp");
    }
    else if (node.op == "==") {
        result = builder.CreateFCmpOEQ(lhs, rhs, "eqtmp");
    }
    else if (node.op == "!=") {
        result = builder.CreateFCmpONE(lhs, rhs, "neqtmp");
    }
    else if (node.op == "&") {
        result = builder.CreateAnd(lhs, rhs, "andtmp");
    }
    else if (node.op == "|") {
        result = builder.CreateOr(lhs, rhs, "ortmp");
    }  
    else if (node.op == "@") {
        // Convertir resultado de lhs y rhs
        node.left->accept(*this);
        llvm::Value* lhs = result;

        node.right->accept(*this);
        llvm::Value* rhs = result;

        // Funciones necesarias
        auto getFunc = [&](const std::string& name, llvm::FunctionType* type) -> llvm::Function* {
            llvm::Function* f = module->getFunction(name);
            if (!f)
                f = llvm::Function::Create(type, llvm::Function::ExternalLinkage, name, module.get());
            return f;
        };

        llvm::Function* strlenFunc = getFunc("strlen", llvm::FunctionType::get(builder.getInt64Ty(), { builder.getInt8PtrTy() }, false));
        llvm::Function* mallocFunc = getFunc("malloc", llvm::FunctionType::get(builder.getInt8PtrTy(), { builder.getInt64Ty() }, false));
        llvm::Function* strcpyFunc = getFunc("strcpy", llvm::FunctionType::get(builder.getInt8PtrTy(), { builder.getInt8PtrTy(), builder.getInt8PtrTy() }, false));
        llvm::Function* strcatFunc = getFunc("strcat", llvm::FunctionType::get(builder.getInt8PtrTy(), { builder.getInt8PtrTy(), builder.getInt8PtrTy() }, false));

        // strlen(lhs) + strlen(rhs) + 1
        llvm::Value* lhsLen = builder.CreateCall(strlenFunc, lhs, "lhs_len");
        llvm::Value* rhsLen = builder.CreateCall(strlenFunc, rhs, "rhs_len");
        llvm::Value* totalLen = builder.CreateAdd(lhsLen, rhsLen, "sum_len");
        llvm::Value* one = llvm::ConstantInt::get(builder.getInt64Ty(), 1);
        llvm::Value* finalLen = builder.CreateAdd(totalLen, one, "total_len");

        // malloc
        llvm::Value* buffer = builder.CreateCall(mallocFunc, finalLen, "str_buf");

        // strcpy(buf, lhs)
        builder.CreateCall(strcpyFunc, { buffer, lhs });

        // strcat(buf, rhs)
        builder.CreateCall(strcatFunc, { buffer, rhs });

        result = buffer;  // buffer contiene la cadena resultante
    }

}

void LLVMCodeGenVisitor::visit(BlockNode& node) {
    for (auto& statement : node.statements) {
        statement->accept(*this);
    }
}