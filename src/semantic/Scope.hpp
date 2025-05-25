#pragma once
#include <unordered_map>
#include <string>
#include "ast/ASTNode.hpp"
#include <llvm/IR/Instructions.h>

struct SymbolInfo {
    Type type = Type::Unknown;
    ASTNode* value = nullptr;
    llvm::Value* llvmValue = nullptr;
    bool isBuiltin = false;  // Nueva bandera
    bool isConstant = false; // Para constantes built-in
};

class Scope {
public:
    Scope* parent;

    Scope(Scope* parentScope = nullptr);
    ~Scope();

    SymbolInfo* lookup(const std::string& name);     // búsqueda recursiva
    SymbolInfo* localLookup(const std::string& name); // búsqueda solo en el scope actual
    void define(const std::string& name, ASTNode* value); // define variable
    void print(int indent = 0) const;

private:
    std::unordered_map<std::string, SymbolInfo> symbols;
};
