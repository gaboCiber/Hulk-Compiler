#pragma once
#include "Symbol.hpp"
#include <vector>
#include <string>

template <typename TokenType>
struct Rule {
    Symbol<TokenType> lhs;                     // No terminal
    std::vector<Symbol<TokenType>> rhs;        // Cuerpo de la producción
    size_t id;                                 // ID único de la regla (opcional)

    Rule(const Symbol<TokenType>& lhs_, const std::vector<Symbol<TokenType>>& rhs_, size_t id_ = 0)
        : lhs(lhs_), rhs(rhs_), id(id_) {}

    Rule() : lhs(Symbol<TokenType>("", SymbolType::UNKNOWN)), rhs({}), id(0) {} // Default constructor


    std::string to_string() const {
        std::string result = lhs.toString() + " → ";
        if (rhs.empty()) return result + "ε";
        for (const auto& sym : rhs) {
            result += sym.toString() + " ";
        }
        return result;
    }
};


