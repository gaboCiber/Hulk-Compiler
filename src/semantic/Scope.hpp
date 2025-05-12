#pragma once
#include <unordered_map>
#include <string>
#include "ast/ASTNode.hpp"

enum class Type {
    Float,
    Bool,
    String,
    Unknown  // útil como valor por defecto
};

struct SymbolInfo {
    Type type = Type::Unknown;
    //bool isDefined = false;
    ASTNode* value = nullptr;
    // Puedes añadir: llvm::Value* llvmPtr = nullptr;
};

class Scope {
public:
    Scope* parent;

    Scope(Scope* parentScope = nullptr);
    ~Scope();

    SymbolInfo* lookup(const std::string& name);     // búsqueda recursiva
    SymbolInfo* localLookup(const std::string& name); // búsqueda solo en el scope actual
    bool define(const std::string& name, ASTNode* value); // define variable
    bool redefine(const std::string& name, ASTNode* value); // redefine variable

    void print(int indent = 0) const;

private:
    std::unordered_map<std::string, SymbolInfo> symbols;
};
