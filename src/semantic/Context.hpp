#pragma once
#include "semantic/Scope.hpp"
#pragma once
#include "semantic/Scope.hpp"
#include "ast/ASTNode.hpp"
#include "ast/TypeRegistry.hpp"
#include <unordered_map>

enum class BuiltinType {
    FUNCTION,
    CONSTANT
};

struct BuiltinInfo {
    BuiltinType type;
    Type* returnType; 
    std::vector<Type*> argTypes; // Para funciones
    ASTNode* value = nullptr;   // Para constantes
    bool returnsArgumentType = false;  // Para print
};

struct FunctionInfo {
    FunctionNode* node;
    Type* returnType;
    bool isBuiltin = false;
    BuiltinInfo* builtinInfo = nullptr; // Solo para built-ins
};

class Context {
public:
    Scope* globalScope;
    TypeRegistry type_registry; 
    std::unordered_map<std::string, FunctionInfo> functionTable;
    std::unordered_map<std::string, BuiltinInfo> builtinTable;

    Type* number_type;
    Type* string_type;
    Type* boolean_type;
    Type* object_type;

    Context();
    ~Context();

    void pushScope(Scope* scope);
    void popScope();
    Scope* currentScope() const;

    bool defineFunction(const std::string& name, FunctionNode* node);
    FunctionInfo* lookupFunction(const std::string& name);

    // Built-ins
    void initializeBuiltins();
    bool isBuiltin(const std::string& name) const;
    BuiltinInfo* lookupBuiltin(const std::string& name);
    bool defineBuiltinFunction(const std::string& name, Type* returnType, const std::vector<Type*>& argTypes, bool returnsArgumentType = false);
    bool defineBuiltinConstant(const std::string& name, Type* type, ASTNode* value);

private:
    std::vector<Scope*> scopeStack;
};
