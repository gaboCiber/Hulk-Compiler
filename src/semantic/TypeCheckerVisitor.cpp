#include "semantic/TypeCheckerVisitor.hpp"
#include "ast/ASTNode.hpp"

TypeCheckerVisitor::TypeCheckerVisitor(Context& context) : ctx(context) {}

void TypeCheckerVisitor::visit(FloatNode& node) {
    lastType = ctx.number_type;
}

void TypeCheckerVisitor::visit(BoolNode& node) {
    lastType = ctx.boolean_type;
}

void TypeCheckerVisitor::visit(StringNode& node) {
    lastType = ctx.string_type;
}

void TypeCheckerVisitor::visit(UnaryOpNode& node) {
    // Primero chequea el hijo
    node.node->accept(*this);
    Type* childT = lastType;
    if (errorFlag) return;  // corto si ya hubo error

    // Ahora, según el operador:
    if (node.op == "-") {
        // Negación: solo Number
        if (childT != ctx.number_type) {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: operador '" + node.op + "' requiere un operando de tipo Number.";
            return;
        }
        lastType = ctx.number_type;
    }
    else if (node.op == "!") {
        // Negación lógica: solo bool
        if (childT != ctx.boolean_type) {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: operador '" + node.op + "' requiere un operando booleano.";
            return;
        }
        lastType = ctx.boolean_type;
    }
    else {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: operador desconocido '" + node.op + "'.";
    }
}

void TypeCheckerVisitor::visit(BinOpNode& node) {
    // Primero chequea el hijo izquierdo
    node.left->accept(*this);
    Type* leftT = lastType;
    if (errorFlag) return;  // corto si ya hubo error

    // Luego el derecho
    node.right->accept(*this);
    Type* rightT = lastType;
    if (errorFlag) return;

    // Ahora, según el operador:
    if (node.op == "+" || node.op == "-" || node.op == "*" ||
        node.op == "/" || node.op == "^" || node.op == "%") {
        // Aritméticos: ambos deben ser Number
        if (leftT != ctx.number_type || rightT != ctx.number_type) {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: operador '" + node.op + "' requiere operandos de tipo Number.";
            return;
        }
        lastType = ctx.number_type;
    }
    else if (node.op == ">" || node.op == "<" || node.op == ">=" || node.op == "<=") {
        // Relacionales: ambos deben ser Number, resultado bool
        if (leftT != ctx.number_type || rightT != ctx.number_type) {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: operador '" + node.op + "' requiere operandos de tipo Number.";
            return;
        }
        lastType = ctx.boolean_type;
    }
    else if (node.op == "==" || node.op == "!=") {
        if (leftT != rightT) {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: operador '" + node.op + "' requiere operandos del mismo tipo.";
            return;
        }
        lastType = ctx.boolean_type;
    }
    else if( node.op == ":=")
    {

        if (leftT != rightT) {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: operador '" + node.op + "' requiere operandos del mismo tipo, no (" + leftT->name + " y " + rightT->name + "). \n";
            return;
        }

        lastType = rightT;
    }
    else if (node.op == "&" || node.op == "|") {
        if (leftT != ctx.boolean_type || rightT != ctx.boolean_type) {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: operador '" + node.op + "' requiere operandos booleanos.";
            return;
        }
        lastType = ctx.boolean_type;
    }
    else if (node.op == "@") {
        // Concatenación: solo string
        if (!leftT->is_primitive() ||!rightT->is_primitive()) {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: los operadorandos '" + node.op + "' deben ser de tipos primitivos.";
            return;
        }
        lastType = ctx.string_type;
    }
    else {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: operador desconocido '" + node.op + "'.";
    }
}

void TypeCheckerVisitor::visit(BlockNode& node) {
    for (auto& child : node.statements) {
        child->accept(*this);
        if (errorFlag) return;  // corto si ya hubo error
    }
}

void TypeCheckerVisitor::visit(VariableNode& node) {

    SymbolInfo* info = ctx.currentScope()->lookup(node.name);

    if(!info->dynamicType)
    {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: la variable " + node.name + " no posee tipo.\n" ;
        return;
    }

    if(!node.declared_type.empty())
    {
        Type* expected = ctx.type_registry.get_type(node.declared_type);
        if(!info->dynamicType->is_subtype_of(expected))
        {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: el tipo inferido (" + info->dynamicType->name + ") de la variable ' " + node.name + " ' es diferente a su tipo esperado (" + expected->name + ").\n" ;
            return;
        }    

         info->staticType = expected;

    }
    else
        info->staticType = info->dynamicType;
    
    
    lastType = info->staticType;
    
}

void TypeCheckerVisitor::visit(LetInNode& node) {
    ctx.pushScope(node.scope);
    for (auto& pair: node.bindings)
    {
        pair.second->accept(*this);

        if(errorFlag)
            return;

        SymbolInfo* info = ctx.currentScope()->lookup(pair.first->name);

        if(lastType != info->dynamicType)
        {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: el tipo inferido (" + info->dynamicType->name + ") de la variable ' " + pair.first->name + " ' es diferente a su lastype (" + lastType->name + ").\n" ;
            return;
        }

        if(!pair.first->declared_type.empty())
        {
            Type* expected = ctx.type_registry.get_type(pair.first->declared_type);
            if(!info->dynamicType->is_subtype_of(expected))
            {
                errorFlag = true;
                errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: el tipo inferido (" + info->dynamicType->name + ") de la variable ' " + pair.first->name + " ' es diferente a su tipo esperado (" + expected->name + ").\n" ;
                return;
            }    

            info->staticType = expected;
        }
        else{
            info->staticType = info->dynamicType;
        }
        
    }
        
    node.block->accept(*this);
    ctx.popScope();
}

void TypeCheckerVisitor::visit(FunctionNode& node) {

    ctx.pushScope(node.scope);
    
    node.block->accept(*this);
    if(errorFlag)
        return;

    Type* infered = lastType;
    FunctionInfo* info = ctx.lookupFunction(node.name);

    if(! info->dinamicReturnType)
        info->dinamicReturnType = infered;
    else if(infered != info->dinamicReturnType)
    {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: el tipo inferido (" + infered->name + ") de la funcion ' " + node.name + " ' es diferente a su lastype (" + lastType->name + ").\n" ;
        return;
    }

    for (auto& arg: node.args)
    {
        arg->accept(*this);
        if(errorFlag)
            return;
    }
    
    if(!node.declared_type.empty())
    {
        Type* expected = ctx.type_registry.get_type(node.declared_type);
        if(!info->dinamicReturnType->is_subtype_of(expected))
        {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: el tipo inferido (" + info->dinamicReturnType->name + ") de la función ' " + node.name + " ' es diferente a su tipo esperado (" + expected->name + ").\n" ;
            return;
        }    

        info->staticReturnType = expected;
    }
    else{
        info->staticReturnType = info->dinamicReturnType;
    }
    
    ctx.popScope();

}

void TypeCheckerVisitor::visit(ProgramNode& node) {
    for (auto stmt : node.functions_and_types) {
        stmt->accept(*this);
        if(errorFlag)
        {
            errorList.push_back(errorMsg);
            errorFlag = false;
        }
    }

    for (auto stmt : node.statements) {
        stmt->accept(*this);
        if(errorFlag)
        {
            errorList.push_back(errorMsg);
            errorFlag = false;
        }
    }
} 

void TypeCheckerVisitor::visit(CallFuncNode& node){
    FunctionInfo* info = ctx.lookupFunction(node.functionName);

    // Primero verificar si es built-in
    if (ctx.isBuiltin(node.functionName)) {
        checkBuiltinCall(node);
        return;
    }

    // Es una funcion normal
    if ( node.arguments.size() != info->node->args.size() ) {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: la función '" + node.functionName + "' requiere " + std::to_string(info->node->args.size()) + " argumentos. Se le pasaron " + std::to_string(node.arguments.size()) + ".\n";
        return;
    }


    for (size_t i = 0; i < node.arguments.size(); ++i) {
        
        node.arguments[i]->accept(*this);
        if(errorFlag)
            return;

        Type* actualType = lastType;

        VariableNode* expectedArg = info->node->args[i];
        SymbolInfo* expectedInfo = info->node->scope->lookup(expectedArg->name);

        if (!expectedInfo) {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: el argumento '" + expectedArg->name + "' no está definido en el scope de la función '" + info->node->name + "'.\n";
            return;
        }

        if (!actualType->is_subtype_of(expectedInfo->staticType)) {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(node.line) + "] Error: el argumento '" + std::to_string(i+1) + "' de la función ' " + node.functionName + " ' debe ser de tipo ' " + expectedInfo->staticType->name + " ' o de su subtipo, no de tipo ' " + actualType->name + " ' .\n";
            return;
        }
    }

    lastType = info->staticReturnType ? info->staticReturnType : info->dinamicReturnType;
}

void TypeCheckerVisitor::checkBuiltinCall(CallFuncNode& node) {
    BuiltinInfo* binfo = ctx.lookupBuiltin(node.functionName);
    if (!binfo) {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: Built-in function '" + node.functionName + "' no encontrada'.\n";
        return;
    }
    
    // Verificar número de argumentos
    if (node.arguments.size() != binfo->argTypes.size()) {
        errorFlag = true;
        errorMsg = "Incorrect number of arguments for " + node.functionName;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: la función '" + node.functionName + "' requiere " + std::to_string(binfo->argTypes.size()) + " argumentos. Se le pasaron " + std::to_string(node.arguments.size()) + ".\n";

        return;
    }
    
    // Verificar tipos de argumentos

    if (binfo->returnsArgumentType) {
        node.arguments[0]->accept(*this);
    
        if(errorFlag)
            return;

        if(!lastType->is_primitive())
        {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: el argumento de la función ' " + node.functionName + " ' debe ser de tipo primitivo, no de tipo ' " + lastType->name + " ' .\n";
            return;    
        }

    }
    else {
        for (size_t i = 0; i < node.arguments.size(); ++i) {
            node.arguments[i]->accept(*this);

            if (!lastType->is_subtype_of(binfo->argTypes[i])) {
                errorFlag = true;
                errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: Argumento " + std::to_string(i+1) + " debe ser '" + binfo->argTypes[i]->name + "' o subtipo";
                return;
            }
        }

        lastType = binfo->returnType;
    }
    
}

void TypeCheckerVisitor::visit(WhileNode& node) {
    node.condition->accept(*this);
    if(errorFlag)
            return;

    if(lastType != ctx.boolean_type)
    {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: la condición del while debe ser de tipo booleano.\n";
        return;
    }

    node.body->accept(*this);
    if(errorFlag)
            return;

    if(lastType != node.returnType)
    {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: el tipo inferido del bloque while es diferente a su tipo chequeado.\n";
        return;
    }
    //lastType = *node.returnType;
}

void TypeCheckerVisitor::visit(IfNode& node) {

    Type* expectedType = node.returnType;
    
    for(auto& pair: node.getBranches()){
        // Verificar condición

        pair.first->accept(*this);
        if (errorFlag) return;
        
        if (lastType != ctx.boolean_type) {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(pair.first->line) + "] La condición debe ser booleana";
            return;
        }

        // Verificar cuerpo

        pair.second->accept(*this);
        if (errorFlag) return;
        
        if (!lastType->is_subtype_of(expectedType)) {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(pair.second->line) + "] El bloque debe retornar '" + expectedType->name + "' o subtipo";
            return;
        }


    }

    if (ASTNode* br = node.getElseBranch()) {

        br->accept(*this);
        if (errorFlag) 
            return;
        

        if (!lastType->is_subtype_of(expectedType)) {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(br->line) + "] El bloque else debe retornar '" + expectedType->name + "' o subtipo";
            return;
        }


    }

    lastType = expectedType;
}

void TypeCheckerVisitor::visit(IsNode& node){
    lastType = ctx.boolean_type;
}

void TypeCheckerVisitor::visit(AsNode& node){
    SymbolInfo* info = ctx.currentScope()->lookup(node.variable_name);
    
    if(!info->staticType)
    {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: tipo todavia no definido para la variable " + node.variable_name + " . \n";
        return;
    }

    Type* type = ctx.type_registry.get_type(node.type_name);
    
    if (!type->is_subtype_of(info->staticType)) {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: el tipo '" + node.type_name +  "' no es subtipo de la varialbe '" + node.variable_name + "' que es de tipo '" + info->staticType->name + "' \n.";
        return;
    }

    lastType = type;
}

void TypeCheckerVisitor::visit(TypeMember& node){
    node.accept(*this);
}

void TypeCheckerVisitor::visit(TypeNode& node){
    
    Type* current_type = ctx.type_registry.get_type(node.name);

    push_current_type(current_type);
    ctx.pushScope(node.scope);

    // Verificar parámetros de tipo
    for (auto arg : *node.type_args) {
        arg->accept(*this);
        if (errorFlag) 
            return;

        current_type->object_data.constructor[arg->name] = lastType;
    }

    // Verificar herencia
    if (node.inherits) {
        
        if (node.inherits->parent_args->size() < node.type_args->size()) {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(node.line) + "] Erro semántico: Número incorrecto de argumentos para tipo padre";
            return;
        }

        node.inherits->accept(*this);
        if (errorFlag) return;
    }

    // Verificar miembros
    for (auto member : node.members) {

        member->accept(*this);
        if (errorFlag) return;
    }
    
    ctx.popScope();
    pop_current_type();

}


void TypeCheckerVisitor::visit(InheritsNode& node) {
    Type* parent_type = ctx.type_registry.get_type(node.parent_type);
    
    if (!parent_type) {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Tipo padre no existe: " + node.parent_type;
        return;
    }
        
    // Verificar argumentos del padre
    if (node.parent_args) {
        // Obtener tipo base para comparar parámetros

        auto arg_info = parent_type->object_data.constructor.begin();
        for (size_t i = 0; i < node.parent_args->size(); i++) {
            
            (*node.parent_args)[i]->accept(*this);
            if (errorFlag) return;
            
            //Verificar compatibilidad con parámetros del tipo padre
            auto& [arg_name, arg_type] = *arg_info;
            if (!lastType->is_subtype_of(arg_type)) {
                errorFlag = true;
                errorMsg = "[Line " + std::to_string(node.line) + "] Argumento de tipo incompatible";
                return;
            }

            std::advance(arg_info, 1);
        }
    }
}

void TypeCheckerVisitor::visit(AttributeNode& node) {
    Type* current_type = get_current_type();
    Type* declared_type = node.declared_type.empty() ? nullptr : 
                        ctx.type_registry.get_type(node.declared_type);

    // Verificar inicializador
    node.initializer->accept(*this);
    if (errorFlag) 
        return;

    // Si tiene tipo declarado, verificar compatibilidad
    if (declared_type && !lastType->is_subtype_of(declared_type)) {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: El tipo inicializador ( " + lastType->name + " ) del atributo ' " + node.getName() + " ' incompatible con tipo declarado ( " + declared_type->name +  " ). \n";
        return;
    }

    // Registrar tipo final del atributo
    Type* attr_type = declared_type ? declared_type : lastType;
    current_type->object_data.attributes[node.getName()] = attr_type;
    lastType = attr_type;
    
}

void TypeCheckerVisitor::visit(MethodNode& node) {
    

    Type* current_type = get_current_type();
    std::string method_name = current_type->name + "." + node.getName();
    FunctionInfo* func_info = ctx.lookupFunction(method_name);

    if (!func_info) {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Método no registrado: " + method_name;
        return;
    }

    func_info->node->accept(*this);
    if(errorFlag)
            return;
    
    // Verificar tipo de retorno declarado
    Type* declared_return = node.declared_type.empty() ? nullptr :
                          ctx.type_registry.get_type(node.declared_type);


    auto method_info = get_current_type()->object_data.methods[node.getName()];

    // Procesar cuerpo del método
    ctx.pushScope(func_info->node->scope);
        
    size_t i = 0;
    for (auto& arg: func_info->node->args)
    {

        SymbolInfo* info = ctx.currentScope()->lookup(arg->name);


        if(!info->staticType)
        {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(node.line) + "] Error: no se pudo inferir el tipo del argumento '" + arg->name + "' \n"; 
        }


        if(!arg->declared_type.empty())
        {
            Type* declared_arg = ctx.type_registry.get_type(arg->declared_type);
    
            if(!info->dynamicType->is_subtype_of(declared_arg))
            {
                errorFlag = true;
                errorMsg = "[Line " + std::to_string(node.line) + "] Error: el argumento '" + arg->name + "' debe ser de tipo '" + declared_arg->name + "' o subtipo";
                return;
            }        
            
            info->staticType = declared_arg;
        }
        else
            info->staticType = info->dynamicType;


        method_info->parameter_types.at(i) = info->staticType;  

        i++;
    }

    ctx.popScope();

    // Verificar compatibilidad de retorno
    if (declared_return && !lastType->is_subtype_of(declared_return)) {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: Retorno incompatible con tipo declarado";
        return;
    }
    else if(declared_return)
        func_info->staticReturnType = method_info->return_type = declared_return;
    else
        func_info->staticReturnType = method_info->return_type = lastType;


    // Registrar tipo de retorno final
    func_info->dinamicReturnType = lastType;
    lastType = func_info->dinamicReturnType;

}

void TypeCheckerVisitor::visit(NewNode& node) {
    Type* type = ctx.type_registry.get_type(node.type_name);
    
    // Verificar argumentos del constructor
    if (node.arguments->size() != type->object_data.constructor.size()) {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: Número incorrecto de argumentos para el constructor";
        return;
    }

    auto arg_info = type->object_data.constructor.begin();
    for (size_t i = 0; i < node.arguments->size(); i++) {
        (*node.arguments)[i]->accept(*this);
        if (errorFlag) return;
        
        auto& [arg_name, arg_type] = *arg_info;
        if (!lastType->is_subtype_of(arg_type)) {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(node.line) + "] Error: el argumento '" + std::to_string(i) + "' del constructor de  '" + node.type_name + "'  debe ser de tipo '" + arg_type->name + "' o subtipo. No de tipo '" + lastType->name + "' .\n";
            return;
        }

        std::advance(arg_info, 1);
    }

    lastType = type;
}

void TypeCheckerVisitor::visit(MemberAccessNode& node){
    node.object->accept(*this);
    if (errorFlag) return;

    Type* objectType = lastType;
    
    // Verifica que el atributo exista
    auto current = objectType;
    while (current)
    {
        auto attrs_info = current->object_data.attributes;
        if(attrs_info.find(node.member_name) != attrs_info.end())
        {
            break;
        }

        current = current->object_data.parent;
    }

    lastType = current->object_data.attributes[node.member_name];

}

void TypeCheckerVisitor::visit(SelfNode& node){
    Type* current = get_current_type();
    if (!current) {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error: 'self' usado fuera de tipo";
        return;
    }
    lastType = current;
}

void TypeCheckerVisitor::visit(BaseNode& node) {
    Type* current = get_current_type();
    std::string realFuncName = ctx.currentScope()->functionName.substr(get_current_type()->name.size() + 1);

    // Verificar argumentos si existen
    if (node.arguments) {
        // Verificar compatibilidad con constructor del padre
        Type* parent_type = current->object_data.parent;

        if (node.arguments->size() != parent_type->object_data.methods[realFuncName]->parameter_types.size()) {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: Número incorrecto de argumentos para el base";
            return;
        }

        auto arg_info = parent_type->object_data.constructor.begin();
        for (size_t i = 0; i < node.arguments->size(); i++) {
            (*node.arguments)[i]->accept(*this);
            if (errorFlag) return;
            
            auto& [arg_name, arg_type] = *arg_info;
            if (!lastType->is_subtype_of(arg_type)) {
                errorFlag = true;
                errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: Argumento de constructor incompatible";
                return;
            }

            std::advance(arg_info, 1);
        }

    }

    lastType = current->object_data.parent->object_data.methods[realFuncName]->return_type;
}

void TypeCheckerVisitor::visit(MethodCallNode& node) {
    // Verificar objeto receptor
    node.object->accept(*this);
    if (errorFlag) return;
    
    Type* object_type = lastType;
    node.object_returnType = object_type;

    // Buscar método en la jerarquía
    FunctionType* method_type = nullptr;
    Type* temp = object_type;
    while (temp != nullptr) {
        auto it = temp->object_data.methods.find(node.getMethodName());
        if (it != temp->object_data.methods.end()) {
            method_type = it->second;
            break;
        }
        temp = temp->object_data.parent;
    }

    if (!method_type) {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico:  El tipo '" + 
                  object_type->name + "' no tiene método '" + node.getMethodName() + "'";
        return;
    }

    // Verificar argumentos
    if (node.arguments.size() != method_type->parameter_types.size()) {
        errorFlag = true;
        errorMsg = "[Line " + std::to_string(node.line) + "] Número incorrecto de argumentos para '" + 
                  node.getMethodName() + "'";
        return;
    }

    for (size_t i = 0; i < node.arguments.size(); i++) {
        node.arguments[i]->accept(*this);
        if (errorFlag) return;
        
        if (!lastType->is_subtype_of(method_type->parameter_types[i])) {
            errorFlag = true;
            errorMsg = "[Line " + std::to_string(node.line) + "] Error semántico: Argumento " + 
                      std::to_string(i+1) + " incompatible para '" + node.getMethodName() + "'";
            return;
        }
    }

    lastType = method_type->return_type;
    //node.object_returnType = lastType;
}
    
