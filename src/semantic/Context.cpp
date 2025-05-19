#include "semantic/Context.hpp"

Context::Context() {
    globalScope = new Scope(nullptr); // raíz
    scopeStack.push_back(globalScope);
}

Context::~Context() {
    for (Scope* s : scopeStack) {
        delete s;
    }
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
    functionTable[name] = { node, Type::Unknown };
    return true;
}

FunctionInfo* Context::lookupFunction(const std::string& name) {
    auto it = functionTable.find(name);
    return (it != functionTable.end()) ? &it->second : nullptr;
}
