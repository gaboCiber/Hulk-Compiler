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
    using ActionFunction = std::function<ASTNodePtr(
        const std::vector<ASTNodePtr>&,
        const lexer::Token<TokenType>&
    )>;

    std::vector<RuleT> rules;
    std::set<SymbolT> terminals;
    std::set<SymbolT> non_terminals;
    std::map<size_t, ActionFunction> actions; // ID de regla -> acción

    SymbolT start_symbol;

    Grammar(const SymbolT& start) : start_symbol(start) {}

    void add_rule(const RuleT& rule, const ActionFunction& action) {
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

    const std::vector<RuleT> get_rules_for(const SymbolT& non_terminal) const {
        std::vector<RuleT> res;
        for (const auto& r : rules) {
            if (r.lhs == non_terminal)
                res.push_back(r);
        }
        return res;
    }

    const ActionFunction& get_action_for_rule(size_t rule_id) const {
        if( actions.find(rule_id) == actions.end()) {
            return nullptr;
        }

        return actions.at(rule_id);
    }
};

