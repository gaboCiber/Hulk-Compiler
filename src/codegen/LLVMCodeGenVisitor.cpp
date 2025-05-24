#include "codegen/LLVMCodeGenVisitor.hpp"
#include "ast/ASTNode.hpp"
#include <llvm/IR/Constants.h>
#include <llvm/IR/Instructions.h>
#include <cmath>
#include <iostream>
#include "LLVMCodeGenVisitor.hpp"


LLVMCodeGenVisitor::LLVMCodeGenVisitor(const std::string& moduleName, Context& c)
    : builder(context), module(std::make_unique<llvm::Module>(moduleName, context)), result(nullptr), ctx(c)
{
    // Nada más aquí. El IR se construye desde ProgramNode
}

llvm::Type* LLVMCodeGenVisitor::llvmType(ASTNode& node, Type t) {
    switch (t) {
        case Type::Float:  
            return builder.getFloatTy();
        case Type::Bool:
           return builder.getInt1Ty();  // bools siguen como floats
        case Type::String:
            return builder.getInt8PtrTy();
        default: 
            std::cerr << "[Line " << node.line << "] Error: tipo desconocido.\n";
            exit(1);  // o simplemente: result = nullptr; y maneja eso después
    }

    return nullptr;
}

llvm::Module* LLVMCodeGenVisitor::getModule() const {
    return module.get();
}

void LLVMCodeGenVisitor::visit(FloatNode& node) {
    result = llvm::ConstantFP::get(context, llvm::APFloat(node.value));
}

void LLVMCodeGenVisitor::visit(BoolNode& node) {
    result = llvm::ConstantInt::get(builder.getInt1Ty(), node.value);  // i1
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
        if (operand->getType()->isIntegerTy(1)) {
            result = builder.CreateNot(operand, "nottmp");
        } 
        else {
            llvm::Value* isTrue = builder.CreateFCmpONE(
                operand,
                llvm::ConstantFP::get(context, llvm::APFloat(0.0f)),
                "isTrue"
            );
            result = builder.CreateNot(isTrue, "nottmp");
        }
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
    else if (node.op == "&" || node.op == "|") {
        if (!lhs->getType()->isIntegerTy(1))
            lhs = builder.CreateFCmpONE(lhs, llvm::ConstantFP::get(context, llvm::APFloat(0.0f)), "lhs_bool");
        if (!rhs->getType()->isIntegerTy(1))
            rhs = builder.CreateFCmpONE(rhs, llvm::ConstantFP::get(context, llvm::APFloat(0.0f)), "rhs_bool");

        if (node.op == "&")
            result = builder.CreateAnd(lhs, rhs, "andtmp");
        else
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
    llvm::Type* varType = llvmType(node, info->type);
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
        llvm::Type* llvmTy = llvmType(node, info->type);

        llvm::Value* alloca = builder.CreateAlloca(llvmTy, nullptr, var->name);
        builder.CreateStore(initValue, alloca);
        
        //info->value = valueExpr;
        info->llvmValue = alloca;
    }

    // Evaluar el cuerpo del let
    node.block->accept(*this);

    ctx.popScope();
}


void LLVMCodeGenVisitor::visit(FunctionNode& node) {
    // Obtener información de tipos desde contexto
    FunctionInfo* info = ctx.lookupFunction(node.name);

    // Obtener tipos de los argumentos
    std::vector<llvm::Type*> argTypes;
    for (VariableNode* arg : node.args) {
        SymbolInfo* argInfo = node.scope->lookup(arg->name);
        argTypes.push_back(llvmType(node, argInfo->type));
    }

    // Crear tipo de retorno real
    llvm::Type* retTy = llvmType(node, info->returnType);
    llvm::FunctionType* funcType = llvm::FunctionType::get(retTy, argTypes, false);

    llvm::Function* func = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, node.name, module.get());
    llvm::BasicBlock* entry = llvm::BasicBlock::Create(context, "entry", func);
    builder.SetInsertPoint(entry);

    ctx.pushScope(node.scope);

    // Mapear argumentos
    unsigned i = 0;
    for (auto& llvmArg : func->args()) {
        VariableNode* arg = node.args[i];
        llvmArg.setName(arg->name);

        SymbolInfo* argInfo = node.scope->lookup(arg->name);
        llvm::Type* llvmTy = llvmType(node, argInfo->type);

        llvm::AllocaInst* alloca = builder.CreateAlloca(llvmTy, nullptr, arg->name);
        builder.CreateStore(&llvmArg, alloca);
        argInfo->llvmValue = alloca;

        i++;
    }

    node.block->accept(*this);
    builder.CreateRet(result);
    ctx.popScope();
}

void LLVMCodeGenVisitor::visit(ProgramNode& node) {
    // Primero declarar todas las funciones
    for (auto func : node.functions)
        func->accept(*this);

    // Opcional: generar main implícito si hay líneas/bloques fuera de funciones
    if (!node.statements.empty()) {
        llvm::FunctionType* mainType = llvm::FunctionType::get(builder.getInt32Ty(), false);
        llvm::Function* mainFunc = llvm::Function::Create(mainType, llvm::Function::ExternalLinkage, "main", module.get());

        llvm::BasicBlock* entry = llvm::BasicBlock::Create(context, "entry", mainFunc);
        builder.SetInsertPoint(entry);

        ctx.pushScope(ctx.globalScope);  // usar global como scope del main

        for (auto stmt : node.statements)
            stmt->accept(*this);

        builder.CreateRet(llvm::ConstantInt::get(builder.getInt32Ty(), 0));

        ctx.popScope();
    }
}

void LLVMCodeGenVisitor::visit(CallFuncNode& node) {
    FunctionInfo* info = ctx.lookupFunction(node.functionName);
    llvm::Function* func = module->getFunction(node.functionName);

    std::vector<llvm::Value*> llvmArgs;

    for (auto argExpr : node.arguments) {
        argExpr->accept(*this);
        llvmArgs.push_back(result);
    }

    result = builder.CreateCall(func, llvmArgs, "call" + info->node->name + "tmp");
}

void LLVMCodeGenVisitor::visit(WhileNode& node) {
    llvm::Function* currentFunc = builder.GetInsertBlock()->getParent();

    // Primero generamos el bloque condicional, cuerpo y salida
    llvm::BasicBlock* condBB = llvm::BasicBlock::Create(context, "while.cond", currentFunc);
    llvm::BasicBlock* bodyBB = llvm::BasicBlock::Create(context, "while.body");
    llvm::BasicBlock* endBB  = llvm::BasicBlock::Create(context, "while.end");

    // Evaluamos tipo para reservar almacenamiento
    llvm::Type* llvmTy = llvmType(node, node.returnType);

    // Variable temporal para guardar resultado del cuerpo
    llvm::AllocaInst* resultVar = builder.CreateAlloca(llvmTy, nullptr, "while.result");

    // Jump a cond
    builder.CreateBr(condBB);
    builder.SetInsertPoint(condBB);

    // Evaluar condición
    node.condition->accept(*this);
    llvm::Value* condVal = result;

    // Convertir a i1 si es float (para floats y bools representados como float)
    if (!condVal->getType()->isIntegerTy(1)) {
        condVal = builder.CreateFCmpONE(
            condVal,
            llvm::ConstantFP::get(context, llvm::APFloat(0.0f)),
            "while.cond.cmp"
        );
    }

    // Salto al cuerpo o al final
    builder.CreateCondBr(condVal, bodyBB, endBB);

    // Cuerpo del bucle
    currentFunc->getBasicBlockList().push_back(bodyBB);
    builder.SetInsertPoint(bodyBB);

    node.body->accept(*this);

    // Guardar resultado del cuerpo
    builder.CreateStore(result, resultVar);

    // Volver a cond
    builder.CreateBr(condBB);

    // Bloque de salida
    currentFunc->getBasicBlockList().push_back(endBB);
    builder.SetInsertPoint(endBB);

    // Leer resultado final
    result = builder.CreateLoad(llvmTy, resultVar, "while.final");
}


// void LLVMCodeGenVisitor::visit(IfNode& node) {
//     llvm::Function* func = builder.GetInsertBlock()->getParent();
//     llvm::Type* llvmTy = llvmType(node, node.returnType);

//     // Crear bloque de merge (salida del if)
//     llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(context, "if.merge", func);
//     llvm::PHINode* phi = builder.CreatePHI(llvmTy, node.getBranches().size() + 1, "iftmp");

//     llvm::BasicBlock* nextCondBB = nullptr;
//     std::vector<std::pair<llvm::Value*, llvm::BasicBlock*>> incoming;

//     // Recorrer if / elif
//     for (size_t i = 0; i < node.getBranches().size(); ++i) {
//         auto [condExpr, bodyExpr] = node.getBranches()[i];

//         if (i > 0) {
//             builder.SetInsertPoint(nextCondBB);
//         }

//         // Evaluar condición
//         condExpr->accept(*this);
//         llvm::Value* condVal = result;
//         if (!condVal->getType()->isIntegerTy(1)) {
//             condVal = builder.CreateFCmpONE(
//                 condVal,
//                 llvm::ConstantFP::get(context, llvm::APFloat(0.0f)),
//                 "ifcond"
//             );
//         }

//         // Crear bloques
//         llvm::BasicBlock* thenBB = llvm::BasicBlock::Create(context, "if.then", func);
//         nextCondBB = llvm::BasicBlock::Create(context, "if.next", func);
//         builder.CreateCondBr(condVal, thenBB, nextCondBB);

//         // THEN
//         builder.SetInsertPoint(thenBB);
//         bodyExpr->accept(*this);
//         llvm::Value* thenVal = result;
//         builder.CreateBr(mergeBB);

//         incoming.emplace_back(thenVal, thenBB);
//     }

//     if (ASTNode* elseBranch = node.getElseBranch()) {
//     llvm::BasicBlock* elseBB = builder.GetInsertBlock(); // el último nextcond creado
//     elseBranch->accept(*this);
//     llvm::Value* elseVal = result;
//     if (phi && elseVal)
//         phi->addIncoming(elseVal, elseBB);
//     builder.CreateBr(mergeBB);
//     } else {
//         // ELSE no existe, así que el último nextcond quedó como bloque actual.
//         llvm::BasicBlock* emptyBB = builder.GetInsertBlock();
//         builder.CreateBr(mergeBB); // hay que saltar al merge
//         if (phi)
//             phi->addIncoming(llvm::UndefValue::get(llvmTy), emptyBB); // valor por defecto
//     }

//     // MERGE block
//     func->getBasicBlockList().push_back(mergeBB);
//     builder.SetInsertPoint(mergeBB);

//     for (const auto& [val, bb] : incoming) {
//         phi->addIncoming(val, bb);
//     }

//     result = phi;
// }

void LLVMCodeGenVisitor::visit(IfNode& node) {
    llvm::Function* func = builder.GetInsertBlock()->getParent();
    llvm::Type* llvmTy = llvmType(node, node.returnType);
    
    std::vector<std::pair<llvm::BasicBlock*, llvm::BasicBlock*>> conditionBlocks; // (condBB, thenBB)
    
    // Crear bloques para cada condición
    for (size_t i = 0; i < node.getBranches().size(); ++i) {
        llvm::BasicBlock* condBB = llvm::BasicBlock::Create(context, 
            i == 0 ? "if.cond" : "elif.cond", func);
        llvm::BasicBlock* thenBB = llvm::BasicBlock::Create(context, 
            i == 0 ? "if.then" : "elif.then", func);
        conditionBlocks.emplace_back(condBB, thenBB);
    }
    
    // Bloque else (siempre existe en Hulk)
    llvm::BasicBlock* elseBB = llvm::BasicBlock::Create(context, "if.else", func);
    
    llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(context, "if.merge", func);

    // Empezar con el primer bloque condicional
    builder.CreateBr(conditionBlocks[0].first);
    
    // Generar código para cada condición
    for (size_t i = 0; i < node.getBranches().size(); ++i) {
        auto& [condBB, thenBB] = conditionBlocks[i];
        auto [condExpr, bodyExpr] = node.getBranches()[i];
        
        // Condición
        builder.SetInsertPoint(condBB);
        condExpr->accept(*this);
        llvm::Value* condVal = result;
        
        if (!condVal->getType()->isIntegerTy(1)) {
            condVal = builder.CreateFCmpONE(
                condVal,
                llvm::ConstantFP::get(context, llvm::APFloat(0.0f)),
                "ifcond"
            );
        }
        
        // Determinar siguiente bloque
        llvm::BasicBlock* nextBB = (i < node.getBranches().size() - 1) 
            ? conditionBlocks[i + 1].first 
            : elseBB;
        
        builder.CreateCondBr(condVal, thenBB, nextBB);
        
        // Cuerpo THEN
        builder.SetInsertPoint(thenBB);
        bodyExpr->accept(*this);
        builder.CreateBr(mergeBB);
    }
    
    // Generar ELSE
    builder.SetInsertPoint(elseBB);
    node.getElseBranch()->accept(*this);
    builder.CreateBr(mergeBB);
    
    // Generar MERGE con PHI node
    builder.SetInsertPoint(mergeBB);
    llvm::PHINode* phi = builder.CreatePHI(llvmTy, node.getBranches().size() + 1, "iftmp");
    
    // Agregar entradas desde los THEN
    for (size_t i = 0; i < node.getBranches().size(); ++i) {
        builder.SetInsertPoint(conditionBlocks[i].second);
        node.getBranches()[i].second->accept(*this);
        phi->addIncoming(result, conditionBlocks[i].second);
    }
    
    // Agregar entrada desde ELSE
    builder.SetInsertPoint(elseBB);
    node.getElseBranch()->accept(*this);
    phi->addIncoming(result, elseBB);
    
    builder.SetInsertPoint(mergeBB);
    result = phi;
}