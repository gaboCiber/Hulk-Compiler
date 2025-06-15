// Symbol.hpp
#pragma once
#include <optional>

enum class SymbolType { TERMINAL, NON_TERMINAL, EPSILON, END_MARKER, UNKNOWN };

template <typename TokenType>
class Symbol {
private:
    SymbolType type;
    std::string name;
    std::optional<TokenType> token_type; // Solo para terminales

public:

    Symbol() = default;

    // Constructor para no terminales
    Symbol(const std::string& name, SymbolType type) 
        : type(type), name(name) {}

    // Constructor para terminales (con TokenType)
    Symbol(const std::string& name, SymbolType type, TokenType token_type) 
        : type(type), name(name), token_type(token_type) {}

    SymbolType getType() const {
        return type;
    }

    const std::string& getName() const {
        return name;
    }

    std::optional<TokenType> getTokenType() const {
        return token_type;
    }

    bool isTerminal() const { 
        return type == SymbolType::TERMINAL; 
    }
    
    bool isNonTerminal() const {
        return type == SymbolType::NON_TERMINAL;
    }

    bool isEpsilon() const{
        return type == SymbolType::EPSILON;
    }

    bool isEndMarker() const{
        return type == SymbolType::END_MARKER;
    }

    bool matches(TokenType input_type) const {
        return isTerminal() && token_type.has_value() && token_type.value() == input_type;
    }

    bool operator==(const Symbol& other) const {
        return type == other.type && name == other.name;
    }

    bool operator!=(const Symbol& other) const {
        return !(*this == other);
    }

    bool operator<(const Symbol& other) const {
        return name < other.name; // Comparación basada en el nombre
    }

    std::string toString() const {
        // if (token_type.has_value()) {
        //     return name + "('" + token_type.value_or(TokenType{}).lexeme + "')";
        // }
        
        return name;
    }

};

