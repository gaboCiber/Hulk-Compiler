#pragma once
#include <unordered_map>
#include <string>
#include "ast/ASTNode.hpp"
#include <llvm/IR/Instructions.h>
#include <string>

struct SymbolInfo {
    Type* type = nullptr;
    ASTNode* value = nullptr;
    llvm::Value* llvmValue = nullptr;
    bool isBuiltin = false;  // Nueva bandera
    bool isConstant = false; // Para constantes built-in
    bool isTypeParameter = false; // Para marcar parámetros de tipo
};

class Scope {
public:
    Scope* parent;
    std::string functionName = "";

    Scope(Scope* parentScope = nullptr);
    ~Scope();

    SymbolInfo* lookup(const std::string& name);     // búsqueda recursiva
    SymbolInfo* localLookup(const std::string& name); // búsqueda solo en el scope actual
    void define(const std::string& name, ASTNode* value); // define variable
    void print(int indent = 0) const;
    int getNumberOfSymbols(){return symbols.size();}

private:
    std::unordered_map<std::string, SymbolInfo> symbols;
};
