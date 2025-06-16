#ifndef LEXER_TOKEN_HPP
#define LEXER_TOKEN_HPP

#include <string>
#include <variant>
#include <optional>
#include <cstddef>
#include <variant>

namespace lexer {

// Tipo de valor que un token puede contener
using TokenValue = std::variant<std::monostate, float, std::string, bool>;

// Token genérico (no depende de una enumeración concreta)
template <typename TokenType>
struct Token {
    TokenType type;
    TokenValue value;
    std::string lexeme;
    std::size_t line;
    std::size_t column;

    Token() = default;

    Token(TokenType t, TokenValue v, const std::string& lxm, std::size_t ln, std::size_t col)
        : type(t), value(std::move(v)), lexeme(lxm), line(ln), column(col) {}

    std::string toSring() const {
        return lexeme;
    }
};

} // namespace lexer

#endif // LEXER_TOKEN_HPP
