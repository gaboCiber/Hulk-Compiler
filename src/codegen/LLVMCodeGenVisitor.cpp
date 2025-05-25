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
        llvm::Function* concatFunc = getBuiltinFunction(
            "hulk_string_concat",
            builder.getInt8PtrTy(),
            {builder.getInt8PtrTy(), builder.getInt8PtrTy()}
        );
        
        result = builder.CreateCall(concatFunc, {lhs, rhs}, "concat");
    }
    if (node.op == ":=") {

        if (VariableNode* var = dynamic_cast<VariableNode*>(node.left)) {
            SymbolInfo* info = ctx.currentScope()->lookup(var->name);
            builder.CreateStore(rhs, info->llvmValue);
            result = rhs;
        } 
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
        llvm::Type* varType = llvmType(node, info->type);
        result = builder.CreateLoad(varType, info->llvmValue, "load_" + node.name);
    }

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
    
    // Primero verificar si es built-in
    if (ctx.isBuiltin(node.functionName)) {
        std::vector<llvm::Value*> argValues;
        for (auto arg : node.arguments) {
            arg->accept(*this);
            argValues.push_back(result);
        }
        result = generateBuiltinCall(node.functionName, argValues);
        return;
    }
    
    FunctionInfo* info = ctx.lookupFunction(node.functionName);
    llvm::Function* func = module->getFunction(node.functionName);

    std::vector<llvm::Value*> llvmArgs;

    for (auto argExpr : node.arguments) {
        argExpr->accept(*this);
        llvmArgs.push_back(result);
    }

    result = builder.CreateCall(func, llvmArgs, "call" + info->node->name + "tmp");
}

llvm::Value* LLVMCodeGenVisitor::generateBuiltinCall(const std::string& name, const std::vector<llvm::Value*>& args) {
    
    // Obtener información del built-in
    BuiltinInfo* binfo = ctx.lookupBuiltin(name);

    // Manejar funciones matemáticas
    if (name == "sin" || name == "cos" || name == "sqrt" || name == "log" || name == "exp") {
        
        llvm::Intrinsic::ID id;
        if (name == "sin") id = llvm::Intrinsic::sin;
        else if (name == "cos") id = llvm::Intrinsic::cos;
        else if (name == "exp") id = llvm::Intrinsic::exp;
        else if (name == "log") id = llvm::Intrinsic::log;
        else if (name == "sqrt") id = llvm::Intrinsic::sqrt;
        
        llvm::Function* fn = llvm::Intrinsic::getDeclaration( module.get(), id, {builder.getFloatTy()});
        return builder.CreateCall(fn, args, name + "tmp");
    }
    
    // Manejar print
    else if (name == "print") {

        llvm::Function* printFunc = getPrintFunctionForType(args[0]->getType());
        llvm::Value* ret = builder.CreateCall(printFunc, {builder.CreateFPExt(args[0], builder.getDoubleTy())}, "printtmp");
        return ret; //args[0];
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

void LLVMCodeGenVisitor::visit(TypeMember& node){

}

void LLVMCodeGenVisitor::visit(TypeNode& node){

}

void LLVMCodeGenVisitor::visit(InheritsNode& node){

}

void LLVMCodeGenVisitor::visit(AttributeNode& node){

}

void LLVMCodeGenVisitor::visit(MethodNode& node){

}

void LLVMCodeGenVisitor::visit(NewNode& node){

}

void LLVMCodeGenVisitor::visit(MemberAccessNode& node){

}

void LLVMCodeGenVisitor::visit(SelfNode& node){

}

void LLVMCodeGenVisitor::visit(BaseNode& node){

}

void LLVMCodeGenVisitor::visit(MethodCallNode& node){

}
    
