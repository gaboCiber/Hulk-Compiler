#include "semantic/Context.hpp"

Context::Context() {
    globalScope = new Scope(nullptr); // raíz
    scopeStack.push_back(globalScope);

    number_type = type_registry.get_type("Number");
    string_type = type_registry.get_type("String");
    boolean_type = type_registry.get_type("Boolean");
    object_type = type_registry.get_type("Object");

    initializeBuiltins();
}

Context::~Context() {
    for (Scope* s : scopeStack) {
        delete s;
    }
}

void Context::initializeBuiltins() {
    // Constantes matemáticas
    defineBuiltinConstant("PI", number_type, new FloatNode(3.141592653589793, -1));
    defineBuiltinConstant("E", number_type, new FloatNode(2.718281828459045, -1));

    // Funciones matemáticas (todas toman y retornan Float)
    defineBuiltinFunction("sin", number_type, {number_type});
    defineBuiltinFunction("cos", number_type, {number_type});
    defineBuiltinFunction("sqrt", number_type, {number_type});
    defineBuiltinFunction("log", number_type, {number_type, number_type});
    defineBuiltinFunction("exp", number_type, {number_type});
    
    // Funciones con efectos secundarios
    defineBuiltinFunction("print", object_type, {object_type}, true);
    defineBuiltinFunction("rand", number_type, {});
    
}

bool Context::defineBuiltinFunction(const std::string& name, Type* returnType, const std::vector<Type*>& argTypes, bool returnsArgumentType) {
    if (builtinTable.count(name)) 
        return false;
    
    // Crear nodo de función dummy (no se usa para built-ins reales)
    auto* funcNode = new FunctionNode(name, {}, nullptr, -1);
    
    builtinTable[name] = {
        BuiltinType::FUNCTION,
        returnType,
        argTypes,
        nullptr,
        returnsArgumentType
    };
    
    // Registrar también en la tabla de funciones
    functionTable[name] = {
        funcNode,
        returnType,
        true,  // isBuiltin
        &builtinTable[name]
    };
    
    return true;
}

bool Context::defineBuiltinConstant(const std::string& name, Type* type, ASTNode* value) {
    if (builtinTable.count(name)) return false;
    
    builtinTable[name] = {
        BuiltinType::CONSTANT,
        type,
        {},  // No hay argumentos
        value,
        true
    };
    
    // Registrar en el scope global
    globalScope->define(name, value);
    auto* info = globalScope->localLookup(name);
    info->type = type;
    info->isBuiltin = true;
    info->isConstant = true;
    
    return true;
}

bool Context::isBuiltin(const std::string& name) const {
    return builtinTable.count(name) > 0;
}

BuiltinInfo* Context::lookupBuiltin(const std::string& name) {
    auto it = builtinTable.find(name);
    return (it != builtinTable.end()) ? &it->second : nullptr;
}


void Context::pushScope(Scope* scope) {
    scopeStack.push_back(scope);
}

void Context::popScope() {
    scopeStack.pop_back();
}

Scope* Context::currentScope() const {
    return scopeStack.back();
}

bool Context::defineFunction(const std::string& name, FunctionNode* node) {
    if (functionTable.count(name)) {
        return false;  // ya definida
    }
    functionTable[name] = { node, nullptr};
    return true;
}

FunctionInfo* Context::lookupFunction(const std::string& name) {
    auto it = functionTable.find(name);
    return (it != functionTable.end()) ? &it->second : nullptr;
}
