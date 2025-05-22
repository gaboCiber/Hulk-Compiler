#pragma once
#include <unordered_map>
#include <string>
#include "ast/ASTNode.hpp"
#include <llvm/IR/Instructions.h>

enum class Type {
    Float,
    Bool,
    String,
    Unknown  // útil como valor por defecto
};

constexpr const char* TypeToString(Type t) {
    switch (t) {
        case Type::Float:  return "Float";
        case Type::Bool:   return "Bool";  
        case Type::String: return "String";
        default: return "";
    }
}


struct SymbolInfo {
    Type type = Type::Unknown;
    //bool isDefined = false;
    ASTNode* value = nullptr;
    llvm::Value* llvmValue = nullptr;
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
