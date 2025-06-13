#ifndef LEXER_LEXER_H
#define LEXER_LEXER_H

#include "token.h"
#include "regex_engine.h"
#include <string>
#include <memory>

// Declaración forward compatible con Bison
#ifndef YYSTYPE
extern "C" union YYSTYPE;
#endif

// Variables globales para Bison
extern int yylineno;
extern YYSTYPE yylval;

// Declaración de yytext
extern char* yytext;

namespace lexer {

class Lexer {
public:
    Lexer();
    explicit Lexer(const std::string& source);
    
    void set_source(const std::string& source);
    Token next_token();
    bool has_more_tokens() const;
    
    int get_next_token();
    
private:
    std::string source_;
    size_t current_pos_;
    size_t current_line_;
    size_t current_column_;
    std::vector<Token> tokens_;
    size_t token_pos_;
    std::unique_ptr<RegexEngine> engine_;
    
    void tokenize_source();
    void update_position(const std::string& lexeme);
};

} // namespace lexer

int yylex();

#endif // LEXER_LEXER_H