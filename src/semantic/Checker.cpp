#include "semantic/Checker.hpp"

bool Checker::runSemanticAnalysis(ASTNode* root, Context& ctx) {
    
    // Primera pasada: definición de variables y scopes
    DefinitionVisitor def(ctx);
    root->accept(def);
    if (def.hasError()) {
        std::cerr << "❌ " << def.getError() << std::endl;
        return false;
    }
    std::cout << "✅ Scopes definidos correctamente.\n";

    // Segunda pasada: uso de variables
    UsageCheckerVisitor usage(ctx);
    root->accept(usage);
    if (usage.hasError()) {
        std::cerr << "❌ " << usage.getError() << std::endl;
        return false;
    }
    std::cout << "✅ Uso de variables válido.\n";

    // Cuarta pasada: inferencia de tipos
    TypeInferenceVisitor inference(ctx);
    root->accept(inference);
    if (inference.hasError()) {
        std::cerr << "❌ " << inference.getError() << std::endl;
        return false;
    }
    std::cout << "✅ Tipos inferidos correctamente.\n";

    // Cuarta pasada: chequeo de tipos
    TypeCheckerVisitor types(ctx);
    root->accept(types);
    if (types.hasError()) {
        std::cerr << "❌ " << types.getError() << std::endl;
        return false;
    }
    std::cout << "✅ Tipos verificados correctamente.\n";

    return true;
}