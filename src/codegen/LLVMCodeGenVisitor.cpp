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
            
            member->accept(*this); // Esto nos da el puntero al campo
            builder.CreateStore(rhs, result); // Almacenar el valor
            result = rhs;
            
            // // Asignación a un miembro de un objeto
            // member->object->accept(*this);
            // llvm::Value* objectPtr = result;

            // // Obtener el tipo del objeto
            // llvm::Type* objectType = objectPtr->getType()->getPointerElementType();
            // llvm::StructType* structType = llvm::cast<llvm::StructType>(objectType);

            // // Obtener el índice del miembro
            // int memberIndex = -1;
            // for (unsigned i = 0; i < structType->getNumElements(); ++i) {
            //     if (structType->getElementName(i) == member->member_name) {
            //         memberIndex = i;
            //         break;
            //     }
            // }

            // if (memberIndex == -1) {
            //     std::cerr << "[Line " << member->line << "] Error: miembro '" << member->member_name << "' no encontrado en tipo.\n";
            //     return;
            // }

            // // Crear la dirección del miembro
            // llvm::Value* memberPtr = builder.CreateStructGEP(objectPtr, memberIndex, "member_ptr");
            // builder.CreateStore(rhs, memberPtr);
            // result = rhs;
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
        llvm::Type* varType = info->type->llvm_type;
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
        llvm::Type* llvmTy = info->type->llvm_type;

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
        argTypes.push_back(argInfo->type->llvm_type);
    }

    // Crear tipo de retorno real
    llvm::Type* retTy = info->returnType->llvm_type;
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
        llvm::Type* llvmTy = argInfo->type->llvm_type;

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

    // Segundo declarar todas las funciones
    for (auto func : node.functions_and_types)
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
    llvm::Type* llvmTy = node.returnType->llvm_type;
    
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
        builder.CreateBr(mergeBB);

        brachResult.push_back(result);
    }
    
    // Generar ELSE
    builder.SetInsertPoint(elseBB);
    node.getElseBranch()->accept(*this);
    builder.CreateBr(mergeBB);

    brachResult.push_back(result);
    
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

void LLVMCodeGenVisitor::visit(TypeMember& node){
    node.accept(*this);
}

void LLVMCodeGenVisitor::visit(TypeNode& node){

    std::cout<<"Iniciando Tipo "<< node.name<<std::endl;

    push_current_type(ctx.type_registry.get_type(node.name));

    types_scope[node.name] = node.scope;
    
    std::cout<<"Procesando argumentos"<<std::endl;
    for(auto n: *node.type_args)
    {
        types_constructor_names[node.name].push_back(n->name);
    }

    std::cout<<"Procesando herencia"<<std::endl;
    if (node.inherits) {
        node.inherits->accept(*this);
    }
    
    // Registrar la estructura del tipo
    get_current_type()->llvm_type = defineTypeStruct(get_current_type());
    

    // Procesar todos los miembros del tipo
    for (auto member : node.members) {
        std::cout<<"Visitando miembro " << member->getName() <<std::endl;
        member->accept(*this);
    }

    pop_current_type();

    std::cout<<"Finito"<<std::endl;
}

llvm::Type* LLVMCodeGenVisitor::defineTypeStruct(Type* type) {
    
    if(type->llvm_type)
        return type->llvm_type;


    // Recorrer la jerarquía de herencia para recolectar todos los atributos
    std::vector<llvm::Type*> members;
    Type* current = type;
    while (current != nullptr) {
        for (const auto& attr : current->object_data.attributes) {
            if(!attr.second->llvm_type)
                attr.second->llvm_type = defineTypeStruct(attr.second);
            
            members.push_back(attr.second->llvm_type);
        }
        current = current->object_data.parent;
    }
    
    // Crear la estructura LLVM
    llvm::StructType* structTy = llvm::StructType::create(context, members, type->name);
    return structTy;
}

void LLVMCodeGenVisitor::visit(InheritsNode& node){
    types_inherits_args[get_current_type()->name] = *node.parent_args;
}

void LLVMCodeGenVisitor::visit(AttributeNode& node){
    types_init_attr[TypeAttrKey(get_current_type()->name, node.getName())] = node.initializer;
}

void LLVMCodeGenVisitor::visit(MethodNode& node) {
    int j = 0;

    std::cout<<std::to_string(++j)<<std::endl;
    
    // Nombre completo del método: Tipo.metodo
    std::string methodFullName = get_current_type()->name + "." + node.getName();
    
    // Usar la misma lógica que FunctionNode
    FunctionInfo* info = ctx.lookupFunction(methodFullName);
    Scope* nodeScope = types_scope[get_current_type()->name];
    
    // Preparar tipos de parámetros (args)
    std::vector<llvm::Type*> paramTypes;

    
    for (VariableNode* arg : *node.parameters) {
        SymbolInfo* argInfo = nodeScope->lookup(arg->name);
        paramTypes.push_back(argInfo->type->llvm_type);
    }

    // Crear función
    if(!info->returnType->llvm_type)
        info->returnType->llvm_type = defineTypeStruct(info->returnType);

    llvm::FunctionType* funcType = llvm::FunctionType::get(
        info->returnType->llvm_type,
        paramTypes,
        false
    );

    llvm::Function* func = llvm::Function::Create(
        funcType,
        llvm::Function::ExternalLinkage,
        methodFullName,
        module.get()
    );

    // Cuerpo de la función
    llvm::BasicBlock* entry = llvm::BasicBlock::Create(context, "entry", func);
    builder.SetInsertPoint(entry);

    ctx.pushScope(nodeScope);  

    std::cout<<std::to_string(++j)<<std::endl;

    // Mapear argumentos (parámetros)
    unsigned i = 0;
    for (auto& llvmArg : func->args()) {
        std::cout<<std::to_string(j) << " " << std::to_string(i)<<std::endl;

        std::string argName = node.parameters->at(i)->name;        
        llvmArg.setName(argName);

        SymbolInfo* argInfo = nodeScope->lookup(argName);
        llvm::Type* llvmTy = argInfo->type->llvm_type;

        llvm::AllocaInst* alloca = builder.CreateAlloca(llvmTy, nullptr, argName);
        builder.CreateStore(&llvmArg, alloca);
        argInfo->llvmValue = alloca;

        i++;
    }

    std::cout<<std::to_string(++j)<<std::endl;

    // Generar cuerpo - el último valor será el retorno
    node.body->accept(*this);
    builder.CreateRet(result);

    ctx.popScope();

    std::cout<<std::to_string(++j)<<std::endl;

}

void LLVMCodeGenVisitor::defineTypeContructorVariables(Type* type, std::vector<ASTNode*> arguments){

    ctx.pushScope(types_scope[type->name]);

    size_t i = 0;
    for(auto arg: arguments)
    {
        // Evaluar expresión del argumento
        arg->accept(*this);
        llvm::Value* initValue = result;

        if (!initValue) {
            std::cerr << "[Line " << arg->line << "] Error: no se pudo generar código '.\n";
            ctx.popScope();
            return;
        }

        // Obtener tipo desde contexto (debería estar definido en análisis de tipos)
        std::string var_name = types_constructor_names[type->name].at(i);
        SymbolInfo* info = ctx.currentScope()->lookup(var_name);

        // Crear alocación en la pila
        llvm::Type* llvmTy = info->type->llvm_type;

        llvm::Value* alloca = builder.CreateAlloca(llvmTy, nullptr, var_name);
        builder.CreateStore(initValue, alloca);
        
        //info->value = valueExpr;
        info->llvmValue = alloca;

        i++;
    }

    ctx.popScope();
}

void LLVMCodeGenVisitor::visit(NewNode& node) {
    Type* type = ctx.type_registry.get_type(node.type_name);

    push_current_type(type);

    // 1. Allocate memory
    llvm::StructType* structTy = static_cast<llvm::StructType*>(type->llvm_type);
    llvm::Value* object = builder.CreateAlloca(structTy, nullptr, node.type_name + ".instance");
    
    // 2. Process constructor arguments
    defineTypeContructorVariables(type, *node.arguments);

    // 3. Initialize attributes
    unsigned fieldIndex = 0;
    Type* current = type;
    while (current != nullptr) {
        for (const auto& attr : current->object_data.attributes) {
            const std::string& attrName = attr.first;
            ASTNode* initializer = types_init_attr[TypeAttrKey(current->name, attrName)];
            
            llvm::Value* initValue;
            if (initializer) {
                initializer->accept(*this);  // Evaluar la expresión de inicialización
                initValue = result;
            } 
            
            // Store en el campo correspondiente
            llvm::Value* fieldPtr = builder.CreateStructGEP(structTy, object, fieldIndex, attrName);
            builder.CreateStore(initValue, fieldPtr);

            types_attr_values[TypeAttrKey(type->name, attrName)] = fieldPtr;

            fieldIndex++;
        }
        auto old = current;
        current = current->object_data.parent;

        if(current)
            defineTypeContructorVariables(current, types_inherits_args[old->name]);
    }
    
    result = object;

    pop_current_type();
}

void LLVMCodeGenVisitor::visit(MemberAccessNode& node) {
    
    if (dynamic_cast<SelfNode*>(node.object)) {

        int k = 10;

        std::cout<<std::to_string(++k)<<std::endl;
        // Obtener tipo del objeto (self)
        Type* objectType = get_current_type();
        
        //Buscar el atributo en la jerarquía de tipos
        int fieldIndex = 0;
        bool found = false;
        Type* current = objectType;
        
        std::cout<<std::to_string(++k)<<std::endl;
        while (current && !found) {
            for (const auto& attr : current->object_data.attributes) {
                if (attr.first == node.member_name) {
                    found = true;
                    break;
                }
                fieldIndex++;
            }
            if (!found) {
                current = current->object_data.parent;
            }
        }
        
        std::cout<<std::to_string(++k)<<std::endl;

        // Obtener puntero al campo
        llvm::StructType* structTy = llvm::cast<llvm::StructType>(objectType->llvm_type);
        llvm::Value* fieldPtr = builder.CreateStructGEP(structTy, types_attr_values[TypeAttrKey(objectType->name, node.member_name)], fieldIndex, node.member_name);
        
        // Cargar el valor
        result = builder.CreateLoad(fieldPtr->getType()->getPointerElementType(), fieldPtr);
        std::cout<<std::to_string(++k)<<std::endl;
    }
}

void LLVMCodeGenVisitor::visit(SelfNode& node) {   
    
}

void LLVMCodeGenVisitor::visit(BaseNode& node) {
    // 1. Obtener el método actual
    llvm::Function* currentFunc = builder.GetInsertBlock()->getParent();
    std::string currentMethodName = currentFunc->getName().split('.').second.str();
    
    // 2. Buscar en la jerarquía de padres
    Type* parentType = get_current_type()->object_data.parent;
    while (parentType) {
        // Buscar el método en el padre
        auto it = parentType->object_data.methods.find(currentMethodName);
        if (it != parentType->object_data.methods.end()) {
            // 3. Preparar argumentos (args)
            std::vector<llvm::Value*> args;
            
            // Argumentos del nodo base (si los hay)
            if (node.arguments) {
                for (auto arg : *node.arguments) {
                    arg->accept(*this);
                    args.push_back(result);
                }
            }
            
            // 4. Llamar al método del padre
            std::string parentMethodName = parentType->name + "." + currentMethodName;
            llvm::Function* parentFunc = module->getFunction(parentMethodName);
            result = builder.CreateCall(parentFunc, args);
            return;
        }
        parentType = parentType->object_data.parent;
    }
    
}

void LLVMCodeGenVisitor::visit(MethodCallNode& node) {
    // Evaluar el objeto
    node.object->accept(*this);
    llvm::Value* object = result;
    Type* objectType = node.object_returnType;
    
    // Buscar el método en la jerarquía
    FunctionType* methodType = nullptr;
    Type* current = objectType;
    while (current && !methodType) {
        auto it = current->object_data.methods.find(node.getMethodName());
        if (it != current->object_data.methods.end()) {
            methodType = it->second;
        }
        current = current->object_data.parent;
    }
    
    
    // Preparar argumentos
    std::vector<llvm::Value*> args;
    args.push_back(object); // this/self
    
    for (auto arg : node.arguments) {
        arg->accept(*this);
        args.push_back(result);
    }
    
    // Buscar la función LLVM
    std::string methodFullName = current->name + "." + node.getMethodName();
    llvm::Function* func = module->getFunction(methodFullName);
    
    // Crear la llamada
    result = builder.CreateCall(func, args, "methodcall");
}
    
