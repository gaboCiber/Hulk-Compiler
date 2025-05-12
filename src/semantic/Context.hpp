#pragma once
#include "semantic/Scope.hpp"

class Context {
public:
    Scope* globalScope;

    Context();
    ~Context();

    void pushScope(Scope* scope);
    void popScope();
    Scope* currentScope() const;

private:
    std::vector<Scope*> scopeStack;
};
