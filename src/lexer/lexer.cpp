#include "include/lexer.h"
#include "include/regex_engine.h"
#include "parser.tab.h"  // Incluir después de nuestras definiciones
#include <cstring>

// Declarar yylval como extern (definido en el parser de Bison)
extern YYSTYPE yylval;

// Definir yytext como variable global
char* yytext = nullptr;

namespace lexer {

Lexer::Lexer() : current_pos_(0), current_line_(1), current_column_(1), token_pos_(0) {
    engine_ = std::make_unique<RegexEngine>();
}

Lexer::Lexer(const std::string& source) : Lexer() {
    set_source(source);
}

void Lexer::set_source(const std::string& source) {
    source_ = source;
    current_pos_ = 0;
    current_line_ = 1;
    current_column_ = 1;
    token_pos_ = 0;
    tokens_.clear();
    tokenize_source();
}

Token Lexer::next_token() {
    if (token_pos_ >= tokens_.size()) {
        return {TokenType::END_OF_FILE, TokenValue(), "", current_line_, current_column_};
    }
    return tokens_[token_pos_++];
}

bool Lexer::has_more_tokens() const {
    return token_pos_ < tokens_.size();
}

int Lexer::get_next_token() {
    if (!has_more_tokens()) {
        return 0; // EOF
    }
    
    Token token = next_token();
    yylineno = token.line;
    
    // Actualizar yytext con el lexema actual
    if (yytext) {
        free(yytext);  // Liberar memoria previa
    }
    yytext = strdup(token.lexeme.c_str());
    
    // Asignar a yylval según el tipo de token
    switch (token.type) {
        case TokenType::FLOAT:
            yylval.fval = token.value.fval;
            break;
        case TokenType::STRING:
            yylval.sval = strdup(token.value.sval);
            break;
        case TokenType::BOOL:
            yylval.bval = token.value.bval;
            break;
        case TokenType::ID:
            yylval.sval = strdup(token.lexeme.c_str());
            break;
        default:
            // Para otros tokens, no necesitamos asignar valor
            break;
    }
    
    return static_cast<int>(token.type);
}

void Lexer::tokenize_source() {
    tokens_ = engine_->tokenize(source_);
    token_pos_ = 0;
}

void Lexer::update_position(const std::string& lexeme) {
    for (char c : lexeme) {
        if (c == '\n') {
            current_line_++;
            current_column_ = 1;
        } else {
            current_column_++;
        }
    }
}

} // namespace lexer

int yylex() {
    static lexer::Lexer lexer;
    return lexer.get_next_token();
}