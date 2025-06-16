#pragma once
#include "Rule.hpp"
#include <vector>
#include <set>
#include <map>
#include <functional>
#include <memory>

template <typename TokenType, typename ASTNode>
class Grammar {
public:
    using SymbolT = Symbol<TokenType>;
    using RuleT = Rule<TokenType>;
    using ASTNodePtr = std::shared_ptr<ASTNode>;
    using RuleActionFunction = std::function<ASTNodePtr(const std::vector<ASTNodePtr>&, const lexer::Token<TokenType>&)>;
    using SymbolActionFuntion = std::function<ASTNodePtr(const lexer::Token<TokenType>&)>;


    std::vector<RuleT> rules;
    std::set<SymbolT> terminals;
    std::set<SymbolT> non_terminals;
    std::map<size_t, RuleActionFunction> actions; // ID de regla -> acción
    std::map<std::string, SymbolActionFuntion> sym_action;

    SymbolT start_symbol;
    SymbolT epsilon_symbol;
    SymbolT end_symbol;

    Grammar(const SymbolT& start, const SymbolT& epsilon, const SymbolT& end) 
        : start_symbol(start), end_symbol(end), epsilon_symbol(epsilon) {}

    void add_rule(const RuleT& rule, const RuleActionFunction& action) {
        rules.push_back(rule);
        actions[rule.id] = action;
        non_terminals.insert(rule.lhs);
        for (const auto& sym : rule.rhs) {
            if (sym.isTerminal())
                terminals.insert(sym);
            else if (sym.isNonTerminal())
                non_terminals.insert(sym);
        }
    }

    void add_sym_rule(const SymbolT& sym, const SymbolActionFuntion& action) {
        sym_action[sym.getName()] = action;
    }

    const std::vector<RuleT> get_rules_for(const SymbolT& non_terminal) const {
        std::vector<RuleT> res;
        for (const auto& r : rules) {
            if (r.lhs == non_terminal)
                res.push_back(r);
        }
        return res;
    }

    const RuleActionFunction& get_action_for_rule(size_t rule_id) const {
        auto it = actions.find(rule_id);
        if (it != actions.end()) {
            return it->second;
        }
        
        static RuleActionFunction empty_function = [](const auto&, const auto&) -> ASTNodePtr {
            return nullptr;
        };

        return empty_function;
    }

    const SymbolActionFuntion& get_action_for_sym_rule(const SymbolT& sym) const {
        auto it = sym_action.find(sym.getName());
        if (it != sym_action.end()) {
            return it->second;
        }
        
        static SymbolActionFuntion empty_function = [](const auto&) -> ASTNodePtr {
            return nullptr;
        };

        return empty_function;
    }

    bool has_action_for_sym_rule(const SymbolT& sym) const {
        return sym_action.find(sym.getName()) != sym_action.end();
    }
};

