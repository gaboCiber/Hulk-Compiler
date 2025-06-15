#pragma once

#include "Grammar.hpp"
#include <map>
#include <set>
#include <stack>
#include <memory>
#include <iostream>

template <typename TokenType, typename ASTNode>
class LL1Parser {
public:
    using SymbolT = Symbol<TokenType>;
    using RuleT = Rule<TokenType>;
    using GrammarT = Grammar<TokenType, ASTNode>;
    using ASTNodePtr = std::shared_ptr<ASTNode>;
    using MyLexer = lexer::Lexer<TokenType>;


    LL1Parser(const GrammarT& grammar) : grammar(grammar) {
        compute_first();
        compute_follow();
        build_parse_table();
    }

    void compute_first() {
        for (const auto& term : grammar.terminals) {
            first_sets[term].insert(term);
        }
        first_sets[SymbolT("ε", SymbolType::EPSILON)].insert(SymbolT("ε", SymbolType::EPSILON));
        first_sets[SymbolT("$", SymbolType::END_MARKER)].insert(SymbolT("$", SymbolType::END_MARKER));

        bool changed;
        do {
            changed = false;
            for (const auto& rule : grammar.rules) {
                const SymbolT& A = rule.lhs;
                const auto& production = rule.rhs;

                if (production.empty() || is_epsilon(production[0])) {
                    changed |= add_to_first(A, {SymbolT("ε", SymbolType::EPSILON)}, changed);
                    continue;
                }

                std::set<SymbolT> new_first;
                bool all_derive_epsilon = true;

                for (const auto& X : production) {
                    if (X.isTerminal()) {
                        new_first.insert(X);
                        all_derive_epsilon = false;
                        break;
                    }

                    const auto& first_X = first_sets[X];
                    bool has_epsilon = false;
                    for (const auto& sym : first_X) {
                        if (sym.isEpsilon()) {
                            has_epsilon = true;
                        } else {
                            new_first.insert(sym);
                        }
                    }

                    if (!has_epsilon) {
                        all_derive_epsilon = false;
                        break;
                    }
                }

                if (all_derive_epsilon) {
                    new_first.insert(SymbolT("ε", SymbolType::EPSILON));
                }

                changed |= add_to_first(A, new_first, changed);
            }
        } while (changed);
    }

    void compute_follow() {
        follow_sets[grammar.start_symbol].insert(SymbolT("$", SymbolType::END_MARKER));

        bool changed;
        do {
            changed = false;
            for (const auto& rule : grammar.rules) {
                const SymbolT& A = rule.lhs;
                const auto& production = rule.rhs;

                for (size_t i = 0; i < production.size(); ++i) {
                    const SymbolT& B = production[i];
                    if (!B.isNonTerminal()) continue;

                    if (i + 1 < production.size()) {
                        const SymbolT& beta_first = production[i + 1];
                        std::set<SymbolT> first_beta;

                        if (beta_first.isTerminal()) {
                            first_beta.insert(beta_first);
                        } else {
                            first_beta = first_sets[beta_first];
                        }

                        bool has_epsilon = false;
                        std::set<SymbolT> to_add;
                        for (const auto& sym : first_beta) {
                            if (is_epsilon(sym)) {
                                has_epsilon = true;
                            } else {
                                to_add.insert(sym);
                            }
                        }
                        changed |= add_to_follow(B, to_add, changed);

                        if (has_epsilon || (beta_first.isNonTerminal() && first_sets[beta_first].count(SymbolT("ε", SymbolType::EPSILON)))) {
                            changed |= add_to_follow(B, follow_sets[A], changed);
                        }
                    } else {
                        changed |= add_to_follow(B, follow_sets[A], changed);
                    }
                }
            }
        } while (changed);
    }

    void build_parse_table() {
        for (const auto& rule : grammar.rules) {
            const SymbolT& A = rule.lhs;
            const std::vector<SymbolT>& alpha = rule.rhs;

            std::set<SymbolT> first_alpha;
            bool derives_epsilon = true;

            for (const auto& X : alpha) {
                const auto& first_X = first_sets[X];
                for (const auto& sym : first_X) {
                    if (!is_epsilon(sym))
                        first_alpha.insert(sym);
                }

                if (first_X.find(SymbolT("ε", SymbolType::EPSILON)) == first_X.end()) {
                    derives_epsilon = false;
                    break;
                }
            }

            for (const auto& a : first_alpha) {
                parse_table[A][a.getTokenType().value()] = rule;
            }

            if (derives_epsilon || alpha.empty()) {
                for (const auto& b : follow_sets[A]) {
                    parse_table[A][b.getTokenType().value()] = rule;
                }
            }
        }
    }

    const std::set<SymbolT>& get_first(const SymbolT& sym) { 
        return first_sets[sym]; 
    }
    
    const std::set<SymbolT>& get_follow(const SymbolT& sym) { 
        return follow_sets[sym]; 
    }

    const RuleT& get_rule(const SymbolT& non_terminal, const TokenType& terminal) const { 
        auto it = parse_table.find(non_terminal);
        if (it != parse_table.end()) {
            auto rule_it = it->second.find(terminal);
            if (rule_it != it->second.end()) {
                return rule_it->second;
            }
        }
        
        throw std::runtime_error("No rule found for " + non_terminal.toString() + " with terminal lexema ");
    }

    void print_first() const {
        std::cout << "FIRST sets:\n";
        for (const auto& [sym, first] : first_sets) {
            std::cout << sym.toString() << ": { ";
            for (const auto& f : first) {
                std::cout << f.toString() << " ";
            }
            std::cout << "}\n";
        }
    }

    void print_follow() const {
        std::cout << "FOLLOW sets:\n";
        for (const auto& [sym, follow] : follow_sets) {
            if (sym.getType() != SymbolType::NON_TERMINAL) continue;
            std::cout << sym.toString() << ": { ";
            for (const auto& f : follow) {
                std::cout << f.toString() << " ";
            }
            std::cout << "}\n";
        }
    }

    void print_parse_table() const  {
        std::cout << "Parse table:\n";
        for (const auto& [nt, row] : parse_table) {
            for (const auto& [t, rule] : row) {
                std::cout << "M[" << nt.getName() << ", " << t << "] = "
                        << rule.lhs.getName() << " → ";
                for (const auto& sym : rule.rhs)
                    std::cout << sym.getTokenType().value_or(TokenType{}) << " ";
                std::cout << std::endl;
            }
        }
    }

    ASTNodePtr parse(MyLexer& lexer) {
        
        std::cout<<"Empezando a parsear"<<std::endl;

        std::cout<<"Start symbol: "<<grammar.start_symbol.getName();
        
        std::stack<Symbol<TokenType>> parse_stack;
        parse_stack.push(Symbol<TokenType>("$", SymbolType::END_MARKER));
        parse_stack.push(grammar.start_symbol);

        std::stack<ASTNodePtr> value_stack;  // Para construir el AST
        std::optional<lexer::Token<TokenType>> current_token = lexer.next_token();
        std::optional<lexer::Token<TokenType>> last_terminal_token;  // Guarda el último terminal consumido

        while (!parse_stack.empty()) {
            Symbol<TokenType> top = parse_stack.top();

            std::cout<<"Analizando token: "<< current_token.value().lexeme;

            // Caso 1: Símbolo terminal o fin de entrada
            if (top.isTerminal() || top.getType() == SymbolType::END_MARKER) {
                if (top.getType() == SymbolType::END_MARKER && 
                    current_token == std::nullopt) {
                    parse_stack.pop();  // Consumimos el $
                    break;
                }

                if(current_token == std::nullopt)
                    throw std::runtime_error(
                        "Error de sintaxis: Se esperaba '" + top.getName() + 
                        "' pero se encontró el fin de la cadena'"
                    );

                if (top.matches(current_token->type)) {
                    // Guardamos tokens relevantes para acciones semánticas
                    if (top.getName() == "number" || top.getName() == "id") {
                        last_terminal_token = current_token;
                    }
                    
                    parse_stack.pop();
                    current_token = lexer.next_token();
                } else {
                    throw std::runtime_error(
                        "Error de sintaxis: Se esperaba '" + top.getName() + 
                        "' pero se encontró '" + current_token->lexeme + "'"
                    );
                }
            }
            // Caso 2: Símbolo no terminal
            else {
                const auto& rule = get_rule(top, current_token->type);
                parse_stack.pop();

                // Aplicar regla en orden inverso (para pila)
                for (auto it = rule.rhs.rbegin(); it != rule.rhs.rend(); ++it) {
                    if (!it->isEpsilon()) {
                        parse_stack.push(*it);
                    }
                }

                // Ejecutar acción semántica si existe
                auto action = grammar.get_action_for_rule(rule.id);
                if ( action ) {
                    std::vector<ASTNodePtr> children;
                    for (size_t i = 0; i < rule.rhs.size(); ++i) {
                        if (!value_stack.empty()) {
                            children.push_back(value_stack.top());
                            value_stack.pop();
                        }
                    }
                    std::reverse(children.begin(), children.end());

                    ASTNodePtr result = action(
                        children,
                        last_terminal_token.value_or(lexer::Token<TokenType>{})
                    );
                    value_stack.push(result);
                }
            }
        }

        if (!value_stack.empty()) {
            return value_stack.top();  // Retorna el AST completo
        }
        return nullptr;  // En caso de error
    }

private:
    const GrammarT& grammar;
    std::map<SymbolT, std::set<SymbolT>> first_sets;
    std::map<SymbolT, std::set<SymbolT>> follow_sets;
    std::map<SymbolT, std::map<TokenType, RuleT>> parse_table;

    bool is_epsilon(const SymbolT& sym) const {
        return sym.getName() == "ε"; // o usa un campo tipo EPSILON si lo defines
    }

    bool add_to_first(const SymbolT& A, const std::set<SymbolT>& symbols, bool& changed) {
        bool modified = false;
        for (const auto& sym : symbols) {
            if (first_sets[A].insert(sym).second) {
                modified = true;
            }
        }
        if (modified) changed = true;
        return modified;
    }

    bool add_to_follow(const SymbolT& A, const std::set<SymbolT>& symbols, bool& changed) {
        bool modified = false;
        for (const auto& sym : symbols) {
            if (follow_sets[A].insert(sym).second) {
                modified = true;
            }
        }
        if (modified) changed = true;
        return modified;
    }
};
