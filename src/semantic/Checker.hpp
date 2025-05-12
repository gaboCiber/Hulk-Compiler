#pragma once

#include "ast/ASTNode.hpp"
#include "semantic/Context.hpp"
#include "semantic/DefinitionVisitor.hpp"
#include "semantic/UsageCheckerVisitor.hpp"
#include "semantic/TypeCheckerVisitor.hpp"
#include <iostream>

class Checker {
public:
    static bool runSemanticAnalysis(ASTNode* root, Context& ctx);
};

