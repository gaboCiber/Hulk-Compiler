#ifndef LEXER_TOKEN_H
#define LEXER_TOKEN_H

#include <string>
#include <cstdlib>
#include <cstring>

// Definición de tipos de tokens
enum class TokenType {
    LET, IN, FUNCTION, WHILE, IF, ELSE, ELIF,
    TRUE, FALSE, TYPE, NEW, INHERITS, SELF, BASE, IS, AS,
    ID, FLOAT, STRING, BOOL,
    POW, PLUS, MINUS, TIMES, DIV, MOD, 
    LPAREN, RPAREN, LKEY, RKEY, SEMICOLON, COMA,
    GREATER_THAN, LESS_THAN, GREATER, LESS,
    EQUAL, NOEQUAL, LAMBDA, DESTRUCTIVE_ASSIGNM, ASSIGNM,
    AND, OR, NOT, BCONCAT, CONCAT, DOT, DOUBLE_DOT,
    END_OF_FILE
};

// Estructura para manejar valores de tokens sin std::variant
struct TokenValue {
    enum { FLOAT, STRING, BOOL, NONE } type;
    union {
        float fval;
        char* sval;
        bool bval;
    };
    
    TokenValue() : type(NONE) {}
    TokenValue(float f) : type(FLOAT), fval(f) {}
    TokenValue(const char* s) : type(STRING) { 
        sval = strdup(s); 
    }
    TokenValue(bool b) : type(BOOL), bval(b) {}
    
    ~TokenValue() {
        if (type == STRING && sval) {
            free(sval);
        }
    }
};

struct Token {
    TokenType type;
    TokenValue value;
    std::string lexeme;
    size_t line;
    size_t column;
    
    Token(TokenType t, TokenValue v, const std::string& l, size_t ln, size_t col)
        : type(t), value(v), lexeme(l), line(ln), column(col) {}
};

#endif // LEXER_TOKEN_H