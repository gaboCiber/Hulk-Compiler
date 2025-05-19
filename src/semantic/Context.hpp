#pragma once
#include "semantic/Scope.hpp"

struct FunctionInfo {
    FunctionNode* node;     // El nodo AST de la función
    Type returnType;        // Tipo de retorno
};


class Context {
public:
    Scope* globalScope;
    std::unordered_map<std::string, FunctionInfo> functionTable;

    Context();
    ~Context();

    void pushScope(Scope* scope);
    void popScope();
    Scope* currentScope() const;

    bool defineFunction(const std::string& name, FunctionNode* node);
    FunctionInfo* lookupFunction(const std::string& name);

private:
    std::vector<Scope*> scopeStack;
};
