#ifndef LEXER_REGEX_ENGINE_HPP
#define LEXER_REGEX_ENGINE_HPP

#include "token.hpp"
#include <vector>
#include <string>
#include <functional>
#include <regex>
#include <stdexcept>

namespace lexer {

template <typename TokenType>
class RegexEngine {
public:
    struct Rule {
        std::string pattern;
        TokenType type;
        std::function<TokenValue(const std::string&)> converter;
        bool ignore;
    };

    RegexEngine() = default;

    void add_rule(const std::string& pattern, TokenType type,
                  std::function<TokenValue(const std::string&)> converter = nullptr,
                  bool ignore = false) {
        rules_.emplace_back(Rule{pattern, type, std::move(converter), ignore});
    }

    void add_error_pattern(const std::string& pattern) {
        error_patterns_.push_back(pattern);
    }

    std::vector<Token<TokenType>> tokenize(const std::string& input) {
        std::vector<Token<TokenType>> tokens;
        std::size_t pos = 0;
        std::size_t line = 1;
        std::size_t column = 1;

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

        while (pos < input.size()) {
            bool matched = false;

            for (const auto& rule : rules_) {
                std::regex re(rule.pattern);
                std::smatch match;
                std::string remaining = input.substr(pos);

                if (std::regex_search(remaining, match, re,
                                      std::regex_constants::match_continuous)) {
                    std::string lexeme = match.str();
                    std::size_t match_line = line;
                    std::size_t match_column = column;

                    update_position(lexeme);

                    if (!rule.ignore) {
                        TokenValue value = rule.converter ? rule.converter(lexeme) : std::monostate{};
                        tokens.emplace_back(rule.type, value, lexeme, match_line, match_column);
                    }

                    pos += lexeme.size();
                    matched = true;
                    break;
                }
            }

            if (!matched) {
                for (const auto& err_pattern : error_patterns_) {
                    std::regex re(err_pattern);
                    std::smatch match;
                    std::string remaining = input.substr(pos);

                    if (std::regex_search(remaining, match, re,
                                          std::regex_constants::match_continuous)) {
                        throw std::runtime_error("Lexer error at line " + std::to_string(line) +
                            ", column " + std::to_string(column) + ": Invalid token '" + match.str() + "'");
                    }
                }

                throw std::runtime_error("Lexer error at line " + std::to_string(line) +
                    ", column " + std::to_string(column) + ": Unexpected character '" + input[pos] + "'");
            }
        }

        return tokens;
    }

private:
    std::vector<Rule> rules_;
    std::vector<std::string> error_patterns_;
};

} // namespace lexer

#endif // LEXER_REGEX_ENGINE_HPP
