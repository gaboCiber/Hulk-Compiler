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
    ctx.number_type->llvm_type = builder.getFloatTy();
    ctx.boolean_type->llvm_type = builder.getInt1Ty();
    ctx.string_type->llvm_type = builder.getInt8PtrTy();
}

llvm::Value* LLVMCodeGenVisitor::getTypeCode(llvm::Type* type) {
    if (type->isFloatTy()) return builder.getInt32(HULK_FLOAT);
    if (type == builder.getInt1Ty()) return builder.getInt32(HULK_BOOL);
    if (type->isPointerTy()) return builder.getInt32(HULK_STRING);
    
    std::cerr << "Error: tipo desconocido.\n";
    exit(1); 
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
    // Crear constante global con atributos correctos
    llvm::GlobalVariable* strVar = new llvm::GlobalVariable(
        *module,
        llvm::ArrayType::get(llvm::Type::getInt8Ty(context), node.value.size() + 1),
        true,  // isConstant
        llvm::GlobalValue::PrivateLinkage,
        llvm::ConstantDataArray::getString(context, node.value),
        ".str"
    );
    
    strVar->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
    strVar->setAlignment(llvm::Align(1));
    
    result = builder.CreateInBoundsGEP(
        strVar->getValueType(),
        strVar,
        {
            llvm::ConstantInt::get(context, llvm::APInt(32, 0)),
            llvm::ConstantInt::get(context, llvm::APInt(32, 0))
        },
        "str_ptr"
    );
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
    
    llvm::Value* lhs; 
    if(node.op != ":=")
    {
        node.left->accept(*this);
        lhs = result;
    }

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
    else if(node.op == "%")
    {
        // a % b = a - b * trunc(a / b)
        llvm::Value* div = builder.CreateFDiv(lhs, rhs, "divmodtemp");
        llvm::Value* trunc = builder.CreateUnaryIntrinsic(llvm::Intrinsic::trunc, div);
        llvm::Value* mul = builder.CreateFMul(rhs, trunc, "mulmodtemp");
        result = builder.CreateFSub(lhs, mul, "modtmp");
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
        llvm::Value* lhsType = getTypeCode(lhs->getType());
        llvm::Value* rhsType = getTypeCode(rhs->getType());
        
        llvm::Function* concatFunc = getBuiltinFunction(
            "hulk_string_concat",
            builder.getInt8PtrTy(),
            {builder.getInt8PtrTy(), builder.getInt32Ty(), builder.getInt8PtrTy(), builder.getInt32Ty()}
        );
        
        // Manejar LHS
        llvm::Value* lhsPtr;
        if (lhs->getType()->isPointerTy()) {
            lhsPtr = lhs;  // Ya es un puntero (string)
        } else {
            // Crear un alloca temporal para el valor no-string
            llvm::AllocaInst* alloca = builder.CreateAlloca(lhs->getType(), nullptr, "temp.lhs");
            builder.CreateStore(lhs, alloca);
            lhsPtr = builder.CreateBitCast(alloca, builder.getInt8PtrTy());
        }
        
        // Manejar RHS
        llvm::Value* rhsPtr;
        if (rhs->getType()->isPointerTy()) {
            rhsPtr = rhs;  // Ya es un puntero (string)
        } else {
            // Crear un alloca temporal para el valor no-string
            llvm::AllocaInst* alloca = builder.CreateAlloca(rhs->getType(), nullptr, "temp.rhs");
            builder.CreateStore(rhs, alloca);
            rhsPtr = builder.CreateBitCast(alloca, builder.getInt8PtrTy());
        }
        
        result = builder.CreateCall(concatFunc, {lhsPtr, lhsType, rhsPtr, rhsType}, "concat");
    }
    else if (node.op == ":=") {

        if (VariableNode* var = dynamic_cast<VariableNode*>(node.left)) {
            SymbolInfo* info = ctx.currentScope()->lookup(var->name);
            builder.CreateStore(rhs, info->llvmValue);
            result = rhs;
        } 
        else if (MemberAccessNode* member = dynamic_cast<MemberAccessNode*>(node.left)) {
            
            isAssignmentTarget = true;

            member->accept(*this); 
            builder.CreateStore(rhs, result); 
            result = rhs;

            isAssignmentTarget = false;
    
        }
    }



}

void LLVMCodeGenVisitor::visit(BlockNode& node) {
    for (auto& statement : node.statements) {
        statement->accept(*this);
    }
}

void LLVMCodeGenVisitor::visit(VariableNode& node) {
    
    SymbolInfo* info = ctx.currentScope()->lookup(node.name);

    if (info && info->isBuiltin && info->isConstant) {
        if (FloatNode* fnode = dynamic_cast<FloatNode*>(info->value)) {
            // Crear constante global para PI
            llvm::GlobalVariable* gvar = module->getGlobalVariable(node.name);
            if (!gvar) {
                gvar = new llvm::GlobalVariable( *module, builder.getFloatTy(), true, 
                    llvm::GlobalValue::ExternalLinkage,
                    llvm::ConstantFP::get(context, llvm::APFloat(fnode->value)),
                    node.name
                );
            }
            result = builder.CreateLoad( builder.getFloatTy(), gvar, "load_" + node.name);
        }
    }
    else {
        if (info->staticType->kind == Type::Kind::OBJECT) {
            // Para objetos, devolver el puntero directamente (no hacer load)
            result = info->llvmValue;
        } else {
            // Para primitivos, hacer load normal
            llvm::Type* varType = info->staticType->llvm_type;
            result = builder.CreateLoad(varType, info->llvmValue, "load_" + node.name);
        }
    }

}

void LLVMCodeGenVisitor::visit(LetInNode& node) {


    Scope* local = node.scope;
    ctx.pushScope(local);

    for (auto& binding : node.bindings) {
        VariableNode* var = binding.first;
        ASTNode* valueExpr = binding.second;

        valueExpr->accept(*this);
        llvm::Value* initValue = result;

        SymbolInfo* info = local->lookup(var->name);
        
        if (info->staticType->kind == Type::Kind::OBJECT) {
            // Para objetos: crear espacio para el puntero
            llvm::Value* alloca = builder.CreateAlloca(
                initValue->getType(), 
                nullptr, 
                var->name
            );
            builder.CreateStore(initValue, alloca);
            info->llvmValue = alloca; 
        } else {
            // Para primitivos
            llvm::Value* alloca = builder.CreateAlloca(
                info->staticType->llvm_type,
                nullptr,
                var->name
            );
            builder.CreateStore(initValue, alloca);
            info->llvmValue = alloca;
        }
    }

    node.block->accept(*this);
    ctx.popScope();

}

void LLVMCodeGenVisitor::visit(FunctionNode& node) {
    // 1. Obtener información de tipos desde el contexto
    FunctionInfo* info = ctx.lookupFunction(node.name);
    function_scope[node.name] = node.scope;

    // 2. Obtener tipos de los argumentos (ajustando para user types)
    std::vector<llvm::Type*> argTypes;
    for (VariableNode* arg : node.args) {
        SymbolInfo* argInfo = node.scope->lookup(arg->name);
        Type* argType = argInfo->staticType;

        llvm::Type* llvmArgType;
        if (argType->kind == Type::Kind::OBJECT) {
            // Convertir a puntero si es tipo definido por el usuario
            llvmArgType = llvm::PointerType::getUnqual(argType->llvm_type);
        } else {
            llvmArgType = argType->llvm_type;
        }
        argTypes.push_back(llvmArgType);
    }

    // 3. Crear tipo de retorno real
    //llvm::Type* retTy = info->staticReturnType->llvm_type;
    llvm::Type* retTy = info->staticReturnType->kind == Type::Kind::OBJECT
        ? llvm::PointerType::getUnqual(info->staticReturnType->llvm_type)
        : info->staticReturnType->llvm_type;

    llvm::FunctionType* funcType = llvm::FunctionType::get(retTy, argTypes, false);

    // 4. Crear la función en el módulo
    llvm::Function* func = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, node.name, module.get());

    // 5. Crear bloque de entrada
    llvm::BasicBlock* entry = llvm::BasicBlock::Create(context, "entry", func);
    builder.SetInsertPoint(entry);

    ctx.pushScope(node.scope);

    // 6. Mapear argumentos
    unsigned i = 0;
    for (auto& llvmArg : func->args()) {
        VariableNode* arg = node.args[i];
        llvmArg.setName(arg->name);

        SymbolInfo* argInfo = node.scope->lookup(arg->name);
        Type* argType = argInfo->staticType;

        // Si es tipo de usuario, entonces es puntero
        llvm::Type* varType = argType->kind == Type::Kind::OBJECT
                                ? llvm::PointerType::getUnqual(argType->llvm_type)
                                : argType->llvm_type;

        llvm::AllocaInst* alloca = builder.CreateAlloca(varType, nullptr, arg->name);
        builder.CreateStore(&llvmArg, alloca);
        argInfo->llvmValue = alloca;

        i++;
    }

    // 7. Visitar el bloque
    node.block->accept(*this);

    // 8. Retornar el resultado
    builder.CreateRet(result);

    // 9. Restaurar el scope
    ctx.popScope();
}

void LLVMCodeGenVisitor::visit(ProgramNode& node) {

    for (auto stmt : node.functions_and_types) {
        if(auto node = dynamic_cast<TypeNode*>(stmt)){
            type_node_map[node->name] = node;
        }
    }

    // Segundo declarar todas las funciones
    for (auto func : node.functions_and_types)
    {
        func->accept(*this);
    }

    // Generar main implícito si hay líneas/bloques fuera de funciones
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
    // 1. Verificar si es una función built-in
    if (ctx.isBuiltin(node.functionName)) {
        std::vector<llvm::Value*> argValues;
        for (auto arg : node.arguments) {
            arg->accept(*this);  // actualiza `result`
            argValues.push_back(result);
        }
        result = generateBuiltinCall(node.functionName, argValues);
        return;
    }

    // 2. Obtener información de la función
    FunctionInfo* info = ctx.lookupFunction(node.functionName);
    llvm::Function* func = module->getFunction(node.functionName);
    if (!func) {
        std::cerr << "Error: función no encontrada: " << node.functionName << "\n";
        exit(1);
    }

    std::vector<llvm::Value*> llvmArgs;

    // 3. Preparar argumentos
    for (size_t i = 0; i < node.arguments.size(); i++) {
        auto argExpr = node.arguments[i];
        argExpr->accept(*this);
        llvm::Value* actualValue = result;

        // Obtener tipo esperado del argumento i
        ctx.pushScope(function_scope[node.functionName]);
        SymbolInfo* simInfo = ctx.currentScope()->lookup(info->node->args[i]->name);
        Type* expectedType = simInfo->staticType;
        ctx.popScope();

        // // Si actualValue es un alloca (%p), lo cargamos (load)
        if (actualValue->getType()->isPointerTy() &&
            actualValue->getType()->getPointerElementType()->isPointerTy() &&
            expectedType->kind == Type::Kind::OBJECT) 
        {
            actualValue = builder.CreateLoad(actualValue->getType()->getPointerElementType(), actualValue, "loaded_arg");
        }

        // Si hay subtipado, hacer bitcast al tipo esperado
        if (expectedType->kind == Type::Kind::OBJECT) {
            llvm::Type* expectedPtrType = llvm::PointerType::getUnqual(expectedType->llvm_type);
            actualValue = builder.CreateBitCast(actualValue, expectedPtrType, "cast_arg");
        }

        llvmArgs.push_back(actualValue);
        
        // Si el argumento es un tipo objeto (user-defined), verificar si es necesario cargarlo
        
        // if (expectedType->kind == Type::Kind::OBJECT) {
        //     llvm::Type* expectedPtrTy = llvm::PointerType::getUnqual(expectedType->llvm_type);

        //     // Si actualValue es una alloca (i.e., puntero a puntero), entonces load
        //     if (actualValue->getType()->isPointerTy() &&
        //         actualValue->getType()->getPointerElementType()->isPointerTy()) {
        //         actualValue = builder.CreateLoad(actualValue->getType()->getPointerElementType(), actualValue, "loaded_arg");
        //     }

        //     // Si el tipo actual no es del tipo esperado, hacer bitcast
        //     if (actualValue->getType() != expectedPtrTy) {
        //         actualValue = builder.CreateBitCast(actualValue, expectedPtrTy, "cast_arg");
        //     }
        // }

        //llvmArgs.push_back(actualValue);

    }

    // 4. Generar la llamada
    result = builder.CreateCall(func, llvmArgs, "call" + info->node->name + "tmp");
}



llvm::Value* LLVMCodeGenVisitor::generateBuiltinCall(const std::string& name, const std::vector<llvm::Value*>& args) {
    
    // Obtener información del built-in
    BuiltinInfo* binfo = ctx.lookupBuiltin(name);

    // Manejar funciones matemáticas
    if (name == "sin" || name == "cos" || name == "sqrt" || name == "exp") {
        
        llvm::Intrinsic::ID id;
        if (name == "sin") id = llvm::Intrinsic::sin;
        else if (name == "cos") id = llvm::Intrinsic::cos;
        else if (name == "exp") id = llvm::Intrinsic::exp;
        else if (name == "sqrt") id = llvm::Intrinsic::sqrt;
        
        llvm::Function* fn = llvm::Intrinsic::getDeclaration( module.get(), id, {builder.getFloatTy()});
        return builder.CreateCall(fn, args, name + "tmp");
    }
    
    else if(name == "log")
    {
        llvm::Function* logFunc = getBuiltinFunction(
            "hulk_log_base",
            builder.getDoubleTy(),
            {builder.getDoubleTy(), builder.getDoubleTy()}
        );

        auto a1 = builder.CreateFPExt(args[0], builder.getDoubleTy());
        auto a2 = builder.CreateFPExt(args[1], builder.getDoubleTy());
        llvm::Value* result = builder.CreateCall(logFunc, {a1, a2}, "logtmp");
        return builder.CreateFPTrunc(result, builder.getFloatTy());

    }

    // Manejar print
    else if (name == "print") {

        llvm::Function* printFunc = getPrintFunctionForType(args[0]->getType());

        auto trueArgs = {args[0]};
        if (args[0]->getType()->isFloatTy())
            trueArgs = {builder.CreateFPExt(args[0], builder.getDoubleTy())};


        builder.CreateCall(printFunc, trueArgs, "printtmp");
        return args[0];
    }
    
    // Manejar rand
    else if (name == "rand") {
        llvm::Function* randFunc = getBuiltinFunction( "rand", builder.getFloatTy(), {});
        return builder.CreateCall(randFunc, {}, "randtmp");
    }
    
    std::cerr << "Erro: Built-in function ' " << name << " ' no soportada" << std::endl;
    exit(1);
}

llvm::Function* LLVMCodeGenVisitor::getPrintFunctionForType(llvm::Type* argType) {
    std::string funcName;
    llvm::Type* paramType = argType;
    llvm::Type* returnType = argType;
    
    // Conversión especial para floats
    if (argType->isFloatTy()) {
        paramType = builder.getDoubleTy();  // Usar double para la llamada
        returnType = builder.getFloatTy();  // print devuelve float (mismo tipo original)
    }
    
    if (argType->isFloatTy()) {
        funcName = "hulk_print_float";
    } 
    else if (argType == builder.getInt1Ty()) {
        funcName = "hulk_print_bool";
    } 
    else if (argType->isPointerTy() && 
             argType->getPointerElementType()->isIntegerTy(8)) {
        funcName = "hulk_print_string";
    }
    else {
        std::cerr << "Error: tipo no soportado para print" << std::endl;
        exit(1);
    }
    
    return getBuiltinFunction(funcName, returnType, {paramType});
}

llvm::Function* LLVMCodeGenVisitor::getBuiltinFunction( const std::string& name, llvm::Type* returnType, const std::vector<llvm::Type*>& argTypes) {
    
    // Verificar si ya está declarada
    llvm::Function* func = module->getFunction(name);
    if (!func) {
        llvm::FunctionType* funcType = 
            llvm::FunctionType::get(returnType, argTypes, false);
        func = llvm::Function::Create(
            funcType, llvm::Function::ExternalLinkage, name, module.get());
    }
    return func;
}

void LLVMCodeGenVisitor::visit(WhileNode& node) {
    llvm::Function* currentFunc = builder.GetInsertBlock()->getParent();

    // Primero generamos el bloque condicional, cuerpo y salida
    llvm::BasicBlock* condBB = llvm::BasicBlock::Create(context, "while.cond", currentFunc);
    llvm::BasicBlock* bodyBB = llvm::BasicBlock::Create(context, "while.body");
    llvm::BasicBlock* endBB  = llvm::BasicBlock::Create(context, "while.end");

    // Evaluamos tipo para reservar almacenamiento
    llvm::Type* llvmTy = node.returnType->llvm_type;

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

void LLVMCodeGenVisitor::visit(IfNode& node) {
    

    llvm::Function* func = builder.GetInsertBlock()->getParent();
    llvm::Type* llvmTy = node.returnType->kind == Type::Kind::OBJECT
                        ? llvm::PointerType::getUnqual(node.returnType->llvm_type)
                        : node.returnType->llvm_type;

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

    std::vector<llvm::Value*> brachResult;
    
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


        llvm::Value* casted = result;
        if (node.returnType->kind == Type::Kind::OBJECT) {
            llvm::Type* expectedPtrTy = llvm::PointerType::getUnqual(node.returnType->llvm_type);
            casted = builder.CreateBitCast(result, expectedPtrTy, "cast_phi");
        }
        brachResult.push_back(casted);

        builder.CreateBr(mergeBB);


    }
    
    // Generar ELSE
    builder.SetInsertPoint(elseBB);
    node.getElseBranch()->accept(*this);

    llvm::Value* elseResult = result;
    if (node.returnType->kind == Type::Kind::OBJECT) {
        llvm::Type* expectedPtrTy = llvm::PointerType::getUnqual(node.returnType->llvm_type);
        elseResult = builder.CreateBitCast(result, expectedPtrTy, "cast_phi");
    }
    brachResult.push_back(elseResult);

    builder.CreateBr(mergeBB);
    
    // Generar MERGE con PHI node
    builder.SetInsertPoint(mergeBB);
    llvm::PHINode* phi = builder.CreatePHI(llvmTy, node.getBranches().size() + 1, "iftmp");
    
    // Agregar entradas desde los THEN
    size_t i;
    for (i = 0; i < node.getBranches().size(); i++) {
        builder.SetInsertPoint(conditionBlocks[i].second);
        phi->addIncoming(brachResult[i], conditionBlocks[i].second);
    }
    
    // Agregar entrada desde ELSE
    builder.SetInsertPoint(elseBB);
    phi->addIncoming(brachResult.at(i), elseBB);
    
    builder.SetInsertPoint(mergeBB);
    result = phi;

}

void LLVMCodeGenVisitor::visit(IsNode& node) {
    SymbolInfo* info = ctx.currentScope()->lookup(node.variable_name);
    assert(info && "Variable no encontrada");

    // Recuperamos el tipo dinámico en tiempo de compilación
    Type* dynamicType = info->dynamicType;
    Type* checkType = info->staticType; //ctx.type_registry.get_type(node.type_name);

    // Verificamos subtipado estructural en tiempo de compilación
    bool isSubtype = dynamicType->is_subtype_of(checkType);

    result = llvm::ConstantInt::get(
        llvm::Type::getInt1Ty(context),
        isSubtype ? 1 : 0,
        false
    );
}


void LLVMCodeGenVisitor::visit(AsNode& node) {
    SymbolInfo* info = ctx.currentScope()->lookup(node.variable_name);
    assert(info && "Variable no definida en AsNode");
    
    Type* sourceType = info->dynamicType;
    Type* targetType = ctx.type_registry.get_type(node.type_name);

    llvm::Value* varPtr = info->llvmValue;

    llvm::Value* loaded = builder.CreateLoad(
        llvm::PointerType::getUnqual(sourceType->llvm_type),
        varPtr,
        node.variable_name + ".as.load"
    );

    llvm::PointerType* targetPtrTy = llvm::PointerType::getUnqual(targetType->llvm_type);
    llvm::Value* casted = builder.CreateBitCast(loaded, targetPtrTy, "as.cast");

    result = casted;
}

std::pair<Type*, FunctionType*> LLVMCodeGenVisitor::resolveMethod(Type* type, const std::string& methodName) {
    Type* current = type;
    while (current) {
        auto it = current->object_data.methods.find(methodName);
        if (it != current->object_data.methods.end()) {
            return {current, it->second};
        }
        current = current->object_data.parent;
    }
    return {nullptr, nullptr};
}

llvm::StructType* LLVMCodeGenVisitor::defineVTableType(Type* type) {
    std::vector<llvm::Type*> methodTypes;
    for (const auto& methodName : types_methods_ordered[type->name]) {
        methodTypes.push_back(builder.getInt8PtrTy()); // i8*
    }
    return llvm::StructType::create(context, methodTypes, type->name + "_vtable");

}

std::vector<llvm::Type*> LLVMCodeGenVisitor::getLLVMParamTypesForMethod(Type* type, FunctionType* fnType) {
    

    std::vector<llvm::Type*> argTypes;

    // El primer parámetro siempre es self
    argTypes.push_back(llvm::PointerType::getUnqual(type->llvm_type));

    for (Type* paramType : fnType->parameter_types) {

        argTypes.push_back(paramType->llvm_type);
    }

    return argTypes;
}

void LLVMCodeGenVisitor::constructTypeMethodStructs(Type* type) {
    std::set<std::string> visited;

    // Recorrer jerarquía de herencia desde la raíz
    std::vector<Type*> hierarchy;
    for (Type* t = type; t != nullptr; t = t->object_data.parent)
        hierarchy.push_back(t);

    std::reverse(hierarchy.begin(), hierarchy.end());

    int i = 0;
    for (Type* t : hierarchy) {
        for (auto& [methodName, _] : t->object_data.methods) {
            if (visited.insert(methodName).second) {
                types_methods_ordered[type->name].push_back(methodName);
                types_method_index_map[type->name][methodName] = i;
            }
            i++;

        }
    }
}

llvm::Constant* LLVMCodeGenVisitor::createVTableInstance(Type* type) {
    llvm::StructType* vtableTy = type->vtable_type;
    std::vector<llvm::Constant*> methodPtrs;

    int i = 0;
    for (const auto& methodName : types_methods_ordered[type->name]) {
        auto [ownerType, _] = resolveMethod(type, methodName);
        std::string fullMethodName = ownerType->name + "." + methodName;

        llvm::Function* func = module->getFunction(fullMethodName);
        if (!func) {
            llvm::errs() << "Error: método '" << fullMethodName << "' no encontrado al crear vtable\n";
            exit(1);
        }

        llvm::Type* expectedType = vtableTy->getElementType(i);  
        llvm::Constant* castedFunc = llvm::ConstantExpr::getBitCast(func, expectedType);
        methodPtrs.push_back(castedFunc);
        i++;
    }

    return llvm::ConstantStruct::get(vtableTy, methodPtrs);
}

llvm::GlobalVariable* LLVMCodeGenVisitor::defineVTableGlobal(Type* type, llvm::Constant* vtableInstance) {
    
    return new llvm::GlobalVariable(
        *module,
        type->vtable_type,
        true,  // isConstant
        llvm::GlobalValue::PrivateLinkage,
        vtableInstance,
        type->name + "_vtable_inst"
    );
}

void LLVMCodeGenVisitor::declareAllMethods(Type* type) {
    for (const auto& methodName : types_methods_ordered[type->name]) {
        auto [ownerType, methodType] = resolveMethod(type, methodName);
        if (!methodType || !ownerType) {
            llvm::errs() << "Error: método '" << methodName << "' no encontrado en jerarquía de tipo '" << type->name << "'\n";
            continue;
        }

        std::string fullMethodName = ownerType->name + "." + methodName;

        std::vector<llvm::Type*> argTypes = getLLVMParamTypesForMethod(ownerType, methodType);
        llvm::FunctionType* funcTy = llvm::FunctionType::get(
            methodType->return_type->llvm_type,
            argTypes,
            false
        );

        if (!module->getFunction(fullMethodName)) {
            llvm::Function::Create(funcTy, llvm::Function::ExternalLinkage, fullMethodName, module.get());
        }
    }
}

void LLVMCodeGenVisitor::visit(TypeNode& node){
    
    if(heritage_map[node.name] == 2)
        return;

    push_current_type(ctx.type_registry.get_type(node.name));

    types_scope[node.name] = node.scope;
    


    for(auto n: *node.type_args)
    {
        types_constructor_names[node.name].push_back(n->name);
    }

    if (node.inherits) {
        auto parent_node = type_node_map[node.inherits->parent_type];
        parent_node->accept(*this);
        heritage_map[node.inherits->parent_type] = 2;

        node.inherits->accept(*this);
    }
    
    //  Registrar estructura de atributos y métodos
    Type* type = get_current_type();

    // Crear StructType vacío antes de nada (forward declaration)
    type->llvm_type = llvm::StructType::create(context, type->name);

    // Construir métodos ordenados
    constructTypeMethodStructs(type);

    // Declarar prototipos de métodos antes de la vtable
    declareAllMethods(type);

    // Registrar vtable del tipo
    type->vtable_type = defineVTableType(type);

    types_vtable_globals[type->name] = defineVTableGlobal(type, createVTableInstance(type));

    // Registrar vtable del tipo
    type->llvm_type = defineTypeStruct(type);

    // Procesar todos los miembros del tipo
    for (auto member : node.members) {

        member->accept(*this);
    }

    pop_current_type();
    heritage_map[node.name] == 2;
}

llvm::Type* LLVMCodeGenVisitor::defineTypeStruct(Type* type) {
    llvm::StructType* structTy = llvm::cast<llvm::StructType>(type->llvm_type);

    if (structTy->isOpaque()) {
        std::vector<llvm::Type*> members;

        // Agregar la vtable como primer campo
        members.push_back(llvm::PointerType::getUnqual(type->vtable_type));

        Type* current = type;
        while (current != nullptr) {
            for (const auto& attr : current->object_data.attributes) {
                if (!attr.second->llvm_type) {
                    attr.second->llvm_type = defineTypeStruct(attr.second);
                }
                members.push_back(attr.second->llvm_type);
            }
            current = current->object_data.parent;
        }

        structTy->setBody(members, false);  // false = no packed
    }

    return structTy;
}

void LLVMCodeGenVisitor::visit(TypeMember& node){
    node.accept(*this);
}

void LLVMCodeGenVisitor::visit(InheritsNode& node){
    types_inherits_args[get_current_type()->name] = *node.parent_args;
}

void LLVMCodeGenVisitor::visit(AttributeNode& node){
    types_init_attr[TypeAttrKey(get_current_type()->name, node.getName())] = node.initializer;
}

void LLVMCodeGenVisitor::visit(MethodNode& node) {


    Type* currentType = get_current_type();
    std::string methodFullName = currentType->name + "." + node.getName();

    // Recuperar FunctionInfo y la función ya declarada
    FunctionInfo* info = ctx.lookupFunction(methodFullName);
    Scope* nodeScope = info->node->scope;

    llvm::Function* func = module->getFunction(methodFullName);
    if (!func) {
        std::cerr << "Error: método no encontrado: " << methodFullName << std::endl;
        exit(1);
    }

    // Crear bloque de entrada
    llvm::BasicBlock* entry = llvm::BasicBlock::Create(context, "entry", func);
    builder.SetInsertPoint(entry);

    ctx.pushScope(nodeScope);

    // Mapear parámetros
    unsigned i = 0;
    for (auto& llvmArg : func->args()) {
        std::string argName;

        if (i == 0) {
            // Primer argumento es self
            argName = "self";
            llvmArg.setName(argName);

            nodeScope->define("self", nullptr);
            SymbolInfo* selfInfo = nodeScope->lookup("self");
            selfInfo->staticType = selfInfo->dynamicType = currentType;
            selfInfo->llvmValue = &llvmArg;
        } else {
            // Argumentos normales
            VariableNode* param = node.parameters->at(i - 1);
            argName = param->name;
            llvmArg.setName(argName);

            SymbolInfo* argInfo = nodeScope->lookup(argName);
            llvm::AllocaInst* alloca = builder.CreateAlloca(
                argInfo->staticType->llvm_type, nullptr, argName
            );
            builder.CreateStore(&llvmArg, alloca);
            argInfo->llvmValue = alloca;
        }
        ++i;
    }

    // Generar cuerpo del método
    node.body->accept(*this);
    builder.CreateRet(result);

    ctx.popScope();

}

void LLVMCodeGenVisitor::defineTypeContructorVariables(Type* type, std::vector<ASTNode*> arguments){

    size_t i = 0;
    for(auto arg: arguments)
    {
        // Evaluar expresión del argumento
        arg->accept(*this);
        llvm::Value* initValue = result;

        // Obtener tipo desde contexto
        ctx.pushScope(types_scope[type->name]);

        std::string var_name = types_constructor_names[type->name].at(i);
        SymbolInfo* info = ctx.currentScope()->lookup(var_name);

        // Crear alocación en la pila
        llvm::Type* llvmTy = info->dynamicType->llvm_type;

        llvm::Value* alloca = builder.CreateAlloca(llvmTy, nullptr, var_name);
        builder.CreateStore(initValue, alloca);
        
        //info->value = valueExpr;
        info->llvmValue = alloca;

        ctx.popScope();

        i++;
    }

    
}

void LLVMCodeGenVisitor::visit(NewNode& node) {
    
    Type* type = ctx.type_registry.get_type(node.type_name);
    push_current_type(type);

    // 1. Allocate memory
    llvm::StructType* structTy = static_cast<llvm::StructType*>(type->llvm_type);
    llvm::Value* object = builder.CreateAlloca(structTy, nullptr, node.type_name + ".instance");

    // 2. Guardar vtable (posición 0)
    llvm::GlobalVariable* vtableGlobal = types_vtable_globals[type->name];
    llvm::Value* vtablePtr = builder.CreateBitCast(vtableGlobal, llvm::PointerType::getUnqual(type->vtable_type), "vtable_ptr");
    llvm::Value* vtableField = builder.CreateStructGEP(structTy, object, 0, "vtable_field");
    builder.CreateStore(vtablePtr, vtableField);

    // 3. Process constructor arguments
    defineTypeContructorVariables(type, *node.arguments);
    
    // 4. Initialize attributes
    unsigned fieldIndex = 1;
    Type* current = type;
    while (current != nullptr) {

        ctx.pushScope(types_scope[current->name]);
        for (const auto& attr : current->object_data.attributes) {
            
            const std::string& attrName = attr.first;
            ASTNode* initializer = types_init_attr[TypeAttrKey(current->name, attrName)];
            
            llvm::Value* initValue;
            if (initializer) {
                initializer->accept(*this);
                initValue = result;
            } 
            
            // Store en el campo correspondiente
            llvm::Value* fieldPtr = builder.CreateStructGEP(structTy, object, fieldIndex, attrName);
            builder.CreateStore(initValue, fieldPtr);
            fieldIndex++;
        }


        auto old = current;
        current = current->object_data.parent;


        if(current)
        {
    
            ctx.pushScope(types_scope[old->name]);
            defineTypeContructorVariables(current, types_inherits_args[old->name]);
            ctx.popScope();
        }

        ctx.popScope();

    }

    result = object;
    pop_current_type();


}

void LLVMCodeGenVisitor::visit(MemberAccessNode& node) {
//     if (dynamic_cast<SelfNode*>(node.object)) {
//         node.object->accept(*this);
//         llvm::Value* self = result;

//         // Obtener tipo del objeto
//         Type* objectType = get_current_type();
        
//         // Buscar el atributo en la jerarquía de tipos
//         int fieldIndex = 1;
//         bool found = false;
//         Type* current = objectType;
        
//         while (current && !found) {
//             for (const auto& attr : current->object_data.attributes) {
//                 if (attr.first == node.member_name) {
//                     found = true;
//                     break;
//                 }
//                 fieldIndex++;
//             }
//             if (!found) {
//                 current = current->object_data.parent;
//                 //fieldIndex = 0;
//             }
//         }

//         // Obtener puntero al campo directamente desde el puntero self
//         llvm::StructType* structTy = llvm::cast<llvm::StructType>(objectType->llvm_type);
//         llvm::Value* fieldPtr = builder.CreateStructGEP(structTy, self, fieldIndex, node.member_name);
        
//         if (isAssignmentTarget) {
//             // Para asignación: devolver solo el puntero
//             result = fieldPtr;
//         } else {
//             // Para acceso normal: devolver el valor cargado
//             llvm::Type* elemTy = fieldPtr->getType()->getPointerElementType();
//             result = builder.CreateLoad(elemTy, fieldPtr, "load_" + node.member_name);
//         }
//     }

    if (dynamic_cast<SelfNode*>(node.object)) {
        node.object->accept(*this);
        llvm::Value* self = result;

        Type* objectType = get_current_type(); // ya debes tener esto implementado
        int fieldIndex = getStructFieldIndex(objectType, node.member_name);

        llvm::StructType* structTy = llvm::cast<llvm::StructType>(objectType->llvm_type);
        llvm::Value* fieldPtr = builder.CreateStructGEP(structTy, self, fieldIndex, node.member_name);

        if (isAssignmentTarget) {
            result = fieldPtr;
        } else {
            llvm::Type* elemTy = fieldPtr->getType()->getPointerElementType();
            result = builder.CreateLoad(elemTy, fieldPtr, "load_" + node.member_name);
        }
    }
}


int LLVMCodeGenVisitor::getStructFieldIndex(Type* type, const std::string& name) {
    int index = 1; // comenzamos en 1 porque el campo 0 es el puntero a vtable

    std::vector<Type*> hierarchy;
    for (Type* current = type; current; current = current->object_data.parent)
        hierarchy.push_back(current);

    // Recorrer desde el ancestro más lejano hacia el hijo
    for (auto it = hierarchy.rbegin(); it != hierarchy.rend(); ++it) {
        for (const auto& attr : (*it)->object_data.attributes) {
            if (attr.first == name)
                return index;
            index++;
        }
    }

}



void LLVMCodeGenVisitor::visit(SelfNode& node) {   

    // Obtener el valor de 'self' del scope actual
    SymbolInfo* selfInfo = ctx.currentScope()->lookup("self");
    if (!selfInfo || !selfInfo->llvmValue) {
        llvm::errs() << "Error: 'self' no definido en el scope actual\n";
        return;
    }
    
    // Devolver directamente el puntero a self
    result = selfInfo->llvmValue;
}

void LLVMCodeGenVisitor::visit(BaseNode& node) {

    llvm::Function* currentFunc = builder.GetInsertBlock()->getParent();
    std::string currentMethodName = currentFunc->getName().split('.').second.str();

    Type* parentType = get_current_type()->object_data.parent;

    while (parentType) {
        auto it = parentType->object_data.methods.find(currentMethodName);
        if (it != parentType->object_data.methods.end()) {
            break;
        }

        parentType = parentType->object_data.parent;
    }

    llvm::Value* selfValue = currentFunc->getArg(0);  // %self
    llvm::Type* parentStructTy = parentType->llvm_type;
    llvm::PointerType* parentPtrTy = llvm::PointerType::getUnqual(parentStructTy);
    llvm::Value* castedSelf = builder.CreateBitCast(selfValue, parentPtrTy, "base_self");

    std::string parentMethodName = parentType->name + "." + currentMethodName;
    llvm::Function* parentFunc = module->getFunction(parentMethodName);
    if (!parentFunc) {
        std::cerr << "Error: no se encontró la función " << parentMethodName << " en el módulo.\n";
        exit(1);
    }

    std::vector<llvm::Value*> args = { castedSelf };
    if (node.arguments) {
        for (auto arg : *node.arguments) {
            arg->accept(*this);
            args.push_back(result);
        }
    }

    result = builder.CreateCall(parentFunc, args, "base_call");
}

void LLVMCodeGenVisitor::visit(MethodCallNode& node) {
    // 1. Evaluar el objeto receptor
    node.object->accept(*this);
    llvm::Value* objectPtr = result;

    // 2. Determinar si hay que hacer load
    llvm::Value* loadedPtr = objectPtr;
    llvm::Type* expectedPtrTy = llvm::PointerType::getUnqual(node.object_returnType->llvm_type);

    // Solo hacemos load si objectPtr es una variable (por ejemplo, alloca)
    if (!dynamic_cast<SelfNode*>(node.object) && llvm::isa<llvm::AllocaInst>(objectPtr)) {
        loadedPtr = builder.CreateLoad(expectedPtrTy, objectPtr, "obj_ptr");
    }

    // 3. Obtener vtable
    llvm::StructType* vtableTy = node.object_returnType->vtable_type;
    llvm::PointerType* vtablePtrTy = llvm::PointerType::getUnqual(vtableTy);
    llvm::Value* vtableField = builder.CreateStructGEP(
        node.object_returnType->llvm_type,
        loadedPtr,
        0,
        "vtable_ptr"
    );
    llvm::Value* vtablePtr = builder.CreateLoad(vtablePtrTy, vtableField, "vtable");

    // 4. Acceder al método desde la vtable
    std::string typeName = node.object_returnType->name;
    std::string methodName = node.getMethodName();
    int methodIndex = types_method_index_map[typeName][methodName];

    llvm::Value* methodSlotPtr = builder.CreateStructGEP(
        vtableTy,
        vtablePtr,
        methodIndex,
        "method_ptr"
    );
    llvm::Value* rawMethodPtr = builder.CreateLoad(
        methodSlotPtr->getType()->getPointerElementType(),
        methodSlotPtr,
        "method_loaded"
    );

    // 5. Resolver tipo original del método
    auto [ownerType, _] = resolveMethod(node.object_returnType, methodName);
    if (!ownerType) {
        llvm::errs() << "Error: método '" << methodName << "' no encontrado en jerarquía de tipo '" << typeName << "'\n";
        exit(1);
    }

    std::string fullMethodName = ownerType->name + "." + methodName;
    llvm::Function* realFn = module->getFunction(fullMethodName);
    llvm::FunctionType* originalFnType = realFn->getFunctionType();

    // 6. Crear nuevo FunctionType con tipo dinámico como primer parámetro
    std::vector<llvm::Type*> paramTypes;
    paramTypes.push_back(loadedPtr->getType()); // puntero a objeto dinámico
    for (unsigned i = 1; i < originalFnType->getNumParams(); ++i) {
        paramTypes.push_back(originalFnType->getParamType(i));
    }

    llvm::FunctionType* adaptedFnType = llvm::FunctionType::get(
        originalFnType->getReturnType(),
        paramTypes,
        false
    );

    // 7. Bitcast
    llvm::Value* methodFn = builder.CreateBitCast(
        rawMethodPtr,
        llvm::PointerType::getUnqual(adaptedFnType),
        "method_fn_casted"
    );

    // 8. Argumentos
    std::vector<llvm::Value*> args;
    args.push_back(loadedPtr);
    for (auto& arg : node.arguments) {
        arg->accept(*this);
        args.push_back(result);
    }

    // 9. Llamar al método
    result = builder.CreateCall(adaptedFnType, methodFn, args, "dyn_call");


}