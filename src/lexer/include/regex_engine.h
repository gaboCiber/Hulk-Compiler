#ifndef LEXER_REGEX_ENGINE_H
#define LEXER_REGEX_ENGINE_H

#include "token.h"
#include <vector>
#include <string>
#include <functional>
#include <regex>
#include <stdexcept>

namespace lexer {

class RegexEngine {
public:
    struct Rule {
        std::string pattern;
        TokenType type;
        std::function<TokenValue(const std::string&)> converter;
        bool ignore;
    };

    explicit RegexEngine();
    
    void add_rule(const std::string& pattern, TokenType type, 
                 std::function<TokenValue(const std::string&)> converter = nullptr,
                 bool ignore = false);
    
    void add_error_pattern(const std::string& pattern);
    
    std::vector<Token> tokenize(const std::string& input);

private:
    std::vector<Rule> rules_;
    std::vector<std::string> error_patterns_;
    
    void setup_default_rules();
};

} // namespace lexer

#endif // LEXER_REGEX_ENGINE_H