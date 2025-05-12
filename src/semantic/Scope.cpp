#pragma once
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

bool Scope::define(const std::string& name, ASTNode* value) {
    if (localLookup(name) != nullptr) {
        return false;
    }
    
    SymbolInfo info;
    info.type = Type::Unknown;
    info.value = value;
    symbols[name] = info;
    return true;
}

bool Scope::redefine(const std::string& name, ASTNode* value) {
    if (localLookup(name) == nullptr) {
        return false;
    }
    
    SymbolInfo& info = symbols[name];
    info.value = value;
    //symbols[name] = info;
    return true;
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
