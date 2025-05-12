#include "codegen/LLVMCodeGenVisitor.hpp"
#include "ast/ASTNode.hpp"
#include <llvm/IR/Constants.h>
#include <llvm/IR/Instructions.h>
#include <cmath>


LLVMCodeGenVisitor::LLVMCodeGenVisitor(const std::string& moduleName, Context& c)
    : builder(context), module(std::make_unique<llvm::Module>(moduleName, context)), result(nullptr), ctx(c)
{
    llvm::FunctionType* funcType = llvm::FunctionType::get(builder.getInt32Ty(), false);
    llvm::Function* mainFunc = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, "main", module.get()
    );

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
    if (node.op == ":=") {
        VariableNode* var = dynamic_cast<VariableNode*>(node.left);

        SymbolInfo* info = ctx.currentScope()->lookup(var->name);

        node.right->accept(*this);  // Evalúa el nuevo valor
        llvm::Value* newVal = result;

        // Hacer el store en el alloca
        builder.CreateStore(newVal, info->llvmValue);

        result = newVal;  // := devuelve el valor asignado
        return;
    }


}

void LLVMCodeGenVisitor::visit(BlockNode& node) {
    for (auto& statement : node.statements) {
        statement->accept(*this);
    }
}

void LLVMCodeGenVisitor::visit(VariableNode& node) {
    SymbolInfo* info = ctx.currentScope()->lookup(node.name);

    // Asumimos que la variable fue almacenada como float, bool o string (pointer i8*)
    llvm::Type* varType;
    switch (info->type) {
        case Type::Float:  varType = builder.getFloatTy(); break;
        case Type::Bool:   varType = builder.getFloatTy(); break; // bools representados como float 0.0 o 1.0
        case Type::String: varType = builder.getInt8PtrTy(); break;
        default:
            std::cerr << "[Line " << node.line << "] Error: tipo desconocido en variable '" << node.name << "'.\n";
            result = nullptr;
            return;
    }

    result = builder.CreateLoad(varType, info->llvmValue, "loadtmp");
}

void LLVMCodeGenVisitor::visit(LetInNode& node) {
    Scope* local = node.scope;
    ctx.pushScope(local);
    //node.scope = local;  // lo guardamos por si se usa en el futuro

    for (auto& binding : node.bindings) {
        VariableNode* var = binding.first;
        ASTNode* valueExpr = binding.second;

        // Evaluar expresión de la variable
        valueExpr->accept(*this);
        llvm::Value* initValue = result;

        if (!initValue) {
            std::cerr << "[Line " << var->line << "] Error: no se pudo generar código para expresión de '" << var->name << "'.\n";
            ctx.popScope();
            return;
        }

        // Obtener tipo desde contexto (debería estar definido en análisis de tipos)
        SymbolInfo* info = local->lookup(var->name);

        // Crear alocación en la pila
        llvm::Type* llvmTy;
        switch (info->type) {
            case Type::Float:  llvmTy = builder.getFloatTy(); break;
            case Type::Bool:   llvmTy = builder.getFloatTy(); break;  // seguimos con float para bools
            case Type::String: llvmTy = builder.getInt8PtrTy(); break;
            default:
                std::cerr << "[Line " << var->line << "] Error: tipo desconocido.\n";
                ctx.popScope();
                return;
        }

        llvm::Value* alloca = builder.CreateAlloca(llvmTy, nullptr, var->name);
        builder.CreateStore(initValue, alloca);
        //info->value = valueExpr;
        info->llvmValue = alloca;
    }

    // Evaluar el cuerpo del let
    node.block->accept(*this);

    ctx.popScope();
}

