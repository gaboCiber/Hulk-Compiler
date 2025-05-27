#include "semantic/Scope.hpp"
#include <iostream>

// Constructor
Scope::Scope(Scope* parentScope) : parent(parentScope) {}

// Destructor
Scope::~Scope() = default;


SymbolInfo* Scope::lookup(const std::string& name) {
    auto it = symbols.find(name);
    if (it != symbols.end()) {
        return &it->second;
    } else if (parent) {
        return parent->lookup(name);
    } else {
        return nullptr;
    }
}

SymbolInfo* Scope::localLookup(const std::string& name) {
    auto it = symbols.find(name);
    return (it != symbols.end()) ? &it->second : nullptr;
}

void Scope::define(const std::string& name, ASTNode* value) {
    
    SymbolInfo* info = localLookup(name);
    if (info != nullptr) {
        info->value = value;
    }
    else{
        info = new SymbolInfo();
        info->type = nullptr;
        info->value = value;
        symbols[name] = *info;
    }
}

void Scope::print(int indent) const {
    std::string pad(indent, ' ');
    for (const auto& pair : symbols) {
        std::cout << pad << pair.first << " -> ";
        if (pair.second.value)
            pair.second.value->print(indent + 2);
        else
            std::cout << "null\n";
    }
}
