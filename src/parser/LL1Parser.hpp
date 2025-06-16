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

        first_sets[grammar.end_symbol].insert(grammar.end_symbol);
        first_sets[grammar.epsilon_symbol].insert(grammar.epsilon_symbol);
        
        bool changed;
        do {
            changed = false;
            for (const auto& rule : grammar.rules) {
                const SymbolT& A = rule.lhs;
                const auto& production = rule.rhs;

                if (production.empty() || production[0].isEpsilon()) {
                    //changed |= add_to_first(A, grammar.epsilon_symbol, changed);
                    changed |= add_to_first(A, std::set<Symbol<TokenType>>{grammar.epsilon_symbol}, changed);
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
                    new_first.insert(grammar.epsilon_symbol);
                    
                }

                changed |= add_to_first(A, new_first, changed);
            }
        } while (changed);
    }

    void compute_follow() {
        
        follow_sets[grammar.start_symbol].insert(grammar.end_symbol);

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
                            if (sym.isEpsilon()) {
                                has_epsilon = true;
                            } else {
                                to_add.insert(sym);
                            }
                        }
                        changed |= add_to_follow(B, to_add, changed);

                        if (has_epsilon || (beta_first.isNonTerminal() && first_sets[beta_first].count(grammar.epsilon_symbol))) {
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
        // Limpiar tabla existente
        parse_table.clear();

        for (const auto& rule : grammar.rules) {
            const SymbolT& A = rule.lhs;
            const auto& production = rule.rhs;

            if( production.empty() || production[0].isEpsilon()) {
                // Regla para producción epsilon
                for (const auto& b : follow_sets[A]) {
                    if (b.isTerminal() || b.isEndMarker()) {
                        parse_table[A][*b.getTokenType()] = rule;
                    }
                }
                continue;
            }

            // Regla para producción no epsilon
            const auto first_alpha = first_sets[production[0]];
            for (const auto& a : first_alpha) {
                if(a.isEpsilon())
                {
                    for(const auto& b : follow_sets[A])
                    {
                        if(b.isTerminal() || b.isEndMarker())
                        {
                            parse_table[A][*b.getTokenType()] = rule;
                        }
                    }
                }
                else if (a.isTerminal()) {
                    parse_table[A][*a.getTokenType()] = rule;
                }
                
            }

            // // Calcular FIRST(production)
            // std::set<SymbolT> first_alpha;
            // bool derives_epsilon = true;

            // for (const auto& X : production) {
            //     const auto& first_X = first_sets[X];
                
            //     // Agregar todos los símbolos excepto épsilon
            //     for (const auto& sym : first_X) {
            //         if (!is_epsilon(sym)) {
            //             first_alpha.insert(sym);
            //         }
            //     }
                
            //     // Verificar si X puede derivar épsilon
            //     if (first_X.find(SymbolT("ε", SymbolType::EPSILON)) == first_X.end()) {
            //         derives_epsilon = false;
            //         break;
            //     }
            // }

            // // Si toda la producción puede derivar épsilon
            // if (derives_epsilon) {
            //     first_alpha.insert(SymbolT("ε", SymbolType::EPSILON));
            // }

            // // Regla 1: Para cada terminal 'a' en FIRST(alpha)
            // for (const auto& a : first_alpha) {
            //     if ( a.isTerminal() && a.getTokenType() ) {
            //         parse_table[A][*a.getTokenType()] = rule;
            //     }
            //     else if (a.isEndMarker())
            //     {
            //         parse_table[A][*a.getTokenType()] = rule;
            //     }
                
            // }

            // // Regla 2: Si épsilon está en FIRST(alpha)
            // if (derives_epsilon) {
            //     for (const auto& b : follow_sets[A]) {
            //         if (b.isTerminal() && b.getTokenType()) {
            //             parse_table[A][*b.getTokenType()] = rule;
            //         }
            //         else if (b.isEndMarker())
            //         {
            //             parse_table[A][*b.getTokenType()] = rule;
            //         }
            //     }
            // }
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

    void print_parse_table() const {
        std::cout << "Parse table:\n";
        for (const auto& [nt, row] : parse_table) {
            for (const auto& [token_type, rule] : row) {
                std::cout << "M[" << nt.getName() << ", " << token_type << "] = "
                        << rule.lhs.getName() << " → ";
                for (const auto& sym : rule.rhs) {
                    if (sym.isEpsilon()) {
                        std::cout << "ε ";
                    } else {
                        std::cout << sym.getName() << " ";
                    }
                }
                std::cout << "\n";
            }
        }
    }

    // ASTNodePtr parse(MyLexer& lexer) {
    //     lexer.tokenize();
        
    //     // Definir tipos para elementos de la pila
    //     using Action = std::function<void()>;
    //     using StackElement = std::variant<Symbol<TokenType>, Action>;
        
    //     // Pila de análisis (símbolos y acciones)
    //     std::stack<StackElement> parse_stack;
    //     parse_stack.push(grammar.end_symbol);
    //     parse_stack.push(grammar.start_symbol);

    //     // Pila de valores para construcción del AST
    //     std::stack<ASTNodePtr> value_stack;
        
    //     auto current_token = lexer.next_token();

    //     while (!parse_stack.empty()) {
    //         if(current_token)
    //             std::cout<<"Token: "<< current_token.value().lexeme << " Type: "<<current_token.value().type<<std::endl;
    //         else
    //             std::cout<<"<END>"<<std::endl;

    //         StackElement top_element = parse_stack.top();
    //         parse_stack.pop();

    //         // Caso 1: Es una acción semántica
    //         if (std::holds_alternative<Action>(top_element)) {
                
    //             std::cout<<"Ejecutando accion semántica..."<<std::endl;

    //             auto action = std::get<Action>(top_element);
    //             action();  // Ejecutar la acción
    //             continue;
    //         }

    //         // Caso 2: Es un símbolo
    //         Symbol<TokenType> top = std::get<Symbol<TokenType>>(top_element);
    //         std::cout<<"Top: "<< top.getName()<<std::endl;

    //         std::cout<<"1: is endMarker?"<<std::endl;
    //         // 1. Manejar símbolo de fin
    //         if (top.isEndMarker()) {
    //             if (!current_token) break;
    //             throw std::runtime_error("Fin de entrada esperado");
    //         }

    //         std::cout<<"2: is empty?"<<std::endl;
    //         // 2. Manejar épsilon
    //         if (top.isEpsilon()) {
    //             continue;
    //         }

    //         std::cout<<"3: is terminal?"<<std::endl;
    //         // 3. Terminales - CONSUMIR TOKEN Y EJECUTAR ACCIÓN SI EXISTE
    //         if (top.isTerminal()) {
    //             if (!current_token) {
    //                 throw std::runtime_error("Token esperado pero se encontró fin de entrada");
    //             }

    //             std::cout<<"3.1: matchea con el token actual?"<<std::endl;
    //             if (top.matches(current_token->type)) {
    //                 std::cout<<"3.2: Matchea!"<<std::endl;
    //                 // Obtener acción asociada al símbolo terminal
    //                 if (auto action = grammar.get_action_for_sym_rule(top)) {
    //                     std::cout<<"3.3: Ejecutando acción semántica para terminal: "<< top.getName() <<std::endl;
    //                     // Ejecutar acción y apilar resultado
    //                     if (auto node = action(*current_token)) {
    //                         value_stack.push(node);
    //                     }
    //                 }
                    
    //                 current_token = lexer.next_token();
    //             } else {
    //                 throw std::runtime_error(
    //                     "Token inesperado: " + current_token->lexeme + 
    //                     ", se esperaba: " + top.getName()
    //                 );
    //             }
    //             continue;
    //         }

    //         std::cout<<"4: No terminales"<<std::endl;
    //         // 4. No terminales - APLICAR REGLA Y PROGRAMAR ACCIÓN
    //         TokenType token_type = current_token ? current_token->type : TokenType::END;
            
    //         std::cout<<"4.1: Obteniendo regla"<<std::endl;
    //         const auto& rule = get_rule(top, token_type);
            
    //         std::cout<<"4.2: Regla obtenida: " << rule.to_string() << std::endl;
    //         // Crear acción semántica si existe
    //         std::optional<Action> rule_action;
    //         if (auto action_func = grammar.get_action_for_rule(rule.id)) {
    //             std::cout<<"4.3: Acción semántica encontrada para la regla: " << rule.id << std::endl;
    //             // Calcular cuántos hijos necesita esta regla
    //             size_t num_children = 0;
    //             for (const auto& sym : rule.rhs) {
    //                 if (!sym.isEpsilon()) num_children++;
    //             }
                
    //             std::cout<<"4.4: Número de hijos necesarios: " << num_children << std::endl;
    //             // Crear acción que se ejecutará cuando la regla esté completa
    //             // rule_action = [=, &value_stack, action_func]() {
    //             //     // Recolectar hijos de la pila de valores
    //             //     std::vector<ASTNodePtr> children;
    //             //     for (size_t i = 0; i < num_children; i++) {
    //             //         if (value_stack.empty()) {
    //             //             throw std::runtime_error("Pila de valores insuficiente para la regla " + rule.to_string());
    //             //         }
    //             //         children.insert(children.begin(), value_stack.top());
    //             //         value_stack.pop();
    //             //     }

    //             //     // Ejecutar acción con los hijos recolectados
    //             //     ASTNodePtr result = action_func(children, lexer::Token<TokenType>{});
    //             //     if (result) {
    //             //         value_stack.push(result);
    //             //     }
    //             // };

    //             // Dentro de las acciones semánticas de reglas:
    //             rule_action = [=, &value_stack, action_func]() {
    //                 std::vector<ASTNodePtr> children;
    //                 for (size_t i = 0; i < num_children; i++) {
    //                     if (value_stack.empty()) {
    //                         throw std::runtime_error("Pila de valores insuficiente para la regla " + rule.to_string());
    //                     }
                        
    //                     // Solo recolectar valores no nulos
    //                     if (value_stack.top()) {
    //                         children.insert(children.begin(), value_stack.top());
    //                     }
    //                     value_stack.pop();
    //                 }

    //                 ASTNodePtr result = action_func(children, lexer::Token<TokenType>{});
    //                 if (result) {
    //                     value_stack.push(result);
    //                 }
    //             };

    //             std::cout<<"4.5: Acción semántica programada para la regla: " << rule.id << std::endl;
    //         }

    //         std::cout<<"5: Apilando símbolos de la regla"<< rule.id <<std::endl;
    //         // Empilar acción semántica si existe (será ejecutada después de los símbolos)
    //         if (rule_action) {
    //             parse_stack.push(*rule_action);
    //         }

    //         std::cout<<"6: Apilando simbolos de la produccion"<< rule.id<<std::endl;
    //         // Empilar símbolos de la producción en orden inverso
    //         // CON ACCIÓN SEMÁNTICA INSERTADA AL FINAL
    //         for (auto it = rule.rhs.rbegin(); it != rule.rhs.rend(); ++it) {
    //             if (!it->isEpsilon()) {
    //                 parse_stack.push(*it);
    //             }
    //         }
            
    //     }

    //     // Verificar que no queden tokens sin consumir
    //     if (current_token) {
    //         throw std::runtime_error("Entrada no completamente consumida");
    //     }

    //     // El resultado final está en el tope de la pila de valores
    //     return value_stack.empty() ? nullptr : value_stack.top();
    // }

    ASTNodePtr parse(MyLexer& lexer) {
        lexer.tokenize();
        
        using Action = std::function<void()>;
        using StackElement = std::variant<Symbol<TokenType>, Action>;
        
        std::stack<StackElement> parse_stack;
        parse_stack.push(grammar.end_symbol);
        parse_stack.push(grammar.start_symbol);

        std::stack<ASTNodePtr> value_stack;
        auto current_token = lexer.next_token();

        while (!parse_stack.empty()) {
            StackElement top_element = parse_stack.top();
            parse_stack.pop();

            if (std::holds_alternative<Action>(top_element)) {
                auto action = std::get<Action>(top_element);
                action();
                continue;
            }

            Symbol<TokenType> top = std::get<Symbol<TokenType>>(top_element);

            if (top.isEndMarker()) {
                if (!current_token) break;
                throw std::runtime_error("Fin de entrada esperado");
            }

            if (top.isEpsilon()) {
                continue;
            }

            if (top.isTerminal()) {
                if (!current_token) {
                    throw std::runtime_error("Token esperado pero se encontró fin de entrada");
                }

                if (top.matches(current_token->type)) {
                    if (auto action = grammar.get_action_for_sym_rule(top)) {
                        if (auto node = action(*current_token)) {
                            value_stack.push(node);
                        }
                    }
                    current_token = lexer.next_token();
                } else {
                    throw std::runtime_error("Token inesperado: " + current_token->lexeme);
                }
                continue;
            }

            TokenType token_type = current_token ? current_token->type : TokenType::END;
            const auto& rule = get_rule(top, token_type);
            
            std::optional<Action> rule_action;
            if (auto action_func = grammar.get_action_for_rule(rule.id)) {
                size_t expected_values = 0;
                for (const auto& sym : rule.rhs) {
                    if (!sym.isEpsilon() && grammar.has_action_for_sym_rule(sym)) {
                        expected_values++;
                    }
                }
                
                rule_action = [=, &value_stack, action_func]() {
                    std::vector<ASTNodePtr> children;
                    for (size_t i = 0; i < expected_values; i++) {
                        if (value_stack.empty()) {
                            throw std::runtime_error("Pila de valores insuficiente");
                        }
                        children.insert(children.begin(), value_stack.top());
                        value_stack.pop();
                    }

                    if (auto result = action_func(children, lexer::Token<TokenType>{})) {
                        value_stack.push(result);
                    }
                };
            }

            if (rule_action) {
                parse_stack.push(*rule_action);
            }

            for (auto it = rule.rhs.rbegin(); it != rule.rhs.rend(); ++it) {
                if (!it->isEpsilon()) {
                    parse_stack.push(*it);
                }
            }
        }

        if (current_token) {
            throw std::runtime_error("Entrada no completamente consumida");
        }

        return value_stack.empty() ? nullptr : value_stack.top();
    }

private:
    const GrammarT& grammar;
    std::map<SymbolT, std::set<SymbolT>> first_sets;
    std::map<SymbolT, std::set<SymbolT>> follow_sets;
    std::map<SymbolT, std::map<TokenType, RuleT>> parse_table;

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
