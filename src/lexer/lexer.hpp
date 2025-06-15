#ifndef LEXER_LEXER_HPP
#define LEXER_LEXER_HPP

#include "token.hpp"
#include "regex_engine.hpp"
#include <vector>
#include <memory>
#include <optional>
#include <string>

namespace lexer {

template <typename TokenType>
class Lexer {
public:
    
    Lexer() : token_pos_(0) {
        engine_ = std::make_unique<RegexEngine<TokenType>>();
    }

    explicit Lexer(std::string source)
        : source_(std::move(source)), token_pos_(0) {
        engine_ = std::make_unique<RegexEngine<TokenType>>();
    }

    RegexEngine<TokenType>& engine() { return *engine_; }

    void put_source(std::string source){
        source_ = std::move(source);
    }

    void tokenize() {
        tokens_ = engine_->tokenize(source_);
        token_pos_ = 0;
    }

    std::optional<Token<TokenType>> next_token() {
        if (token_pos_ < tokens_.size()) {
            return tokens_[token_pos_++];
        }
        return std::nullopt;
    }

    const std::vector<Token<TokenType>>& all_tokens() const {
        return tokens_;
    }

private:
    std::string source_;
    std::unique_ptr<RegexEngine<TokenType>> engine_;
    std::vector<Token<TokenType>> tokens_;
    std::size_t token_pos_;
};

} // namespace lexer

#endif // LEXER_LEXER_HPP
