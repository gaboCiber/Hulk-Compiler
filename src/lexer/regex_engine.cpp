#include "include/regex_engine.h"
#include <utility>
#include <cctype>
#include <iostream>
#include <cstdlib>

namespace lexer {

RegexEngine::RegexEngine() {
    setup_default_rules();
}

void RegexEngine::add_rule(const std::string& pattern, TokenType type,
                         std::function<TokenValue(const std::string&)> converter,
                         bool ignore) {
    rules_.push_back({pattern, type, std::move(converter), ignore});
}

void RegexEngine::add_error_pattern(const std::string& pattern) {
    error_patterns_.push_back(pattern);
}

void RegexEngine::setup_default_rules() {
    // [Todas las reglas existentes permanecen igual...]
}

std::vector<Token> RegexEngine::tokenize(const std::string& input) {
    std::vector<Token> tokens;
    size_t pos = 0;
    size_t line = 1;
    size_t column = 1;
    
    auto update_position = [&](const std::string& lexeme) {
        for (char c : lexeme) {
            if (c == '\n') {
                line++;
                column = 1;
            } else {
                column++;
            }
        }
    };
    
    while (pos < input.length()) {
        bool matched = false;
        
        for (const auto& rule : rules_) {
            std::regex re(rule.pattern);
            std::smatch match;
            std::string remaining = input.substr(pos);
            
            if (std::regex_search(remaining, match, re, 
                                 std::regex_constants::match_continuous)) {
                std::string lexeme = match.str();
                size_t match_line = line;
                size_t match_column = column;
                
                update_position(lexeme);
                
                if (!rule.ignore) {
                    TokenValue value = rule.converter ? rule.converter(lexeme) : TokenValue();
                    tokens.emplace_back(rule.type, value, lexeme, match_line, match_column);
                }
                
                pos += lexeme.length();
                matched = true;
                break;
            }
        }
        
        if (!matched) {
            for (const auto& error_pat : error_patterns_) {
                std::regex re(error_pat);
                std::smatch match;
                std::string remaining = input.substr(pos);
                
                if (std::regex_search(remaining, match, re, 
                                     std::regex_constants::match_continuous)) {
                    std::cerr << "Lexer error at line " << line
                              << ", column " << column
                              << ": Invalid token '" << match.str() << "'" << std::endl;
                    std::exit(EXIT_FAILURE);
                }
            }
            
            std::cerr << "Lexer error at line " << line
                      << ", column " << column
                      << ": Unexpected character '" << input.substr(pos, 1) << "'" << std::endl;
            std::exit(EXIT_FAILURE);
        }
    }
    
    tokens.emplace_back(TokenType::END_OF_FILE, TokenValue(), "", line, column);
    return tokens;
}

} // namespace lexer