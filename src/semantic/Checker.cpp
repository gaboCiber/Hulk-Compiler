#include "semantic/Checker.hpp"

bool Checker::runSemanticAnalysis(ASTNode* root, Context& ctx) {
    
    // Primera pasada: definición de variables y scopes
    DefinitionVisitor def(ctx);
    root->accept(def);

    UsageCheckerVisitor usage(ctx);
    root->accept(usage);

    bool ret = false;
    
    if (def.hasError()) {
        def.printError();
        ret = true;
    }
    else
        std::cout << "✅ Scopes definidos correctamente.\n";
    
        
    if (usage.hasError()) {
        usage.printError();
        ret = true;
    }
    else
        std::cout << "✅ Uso de variables válido.\n";

    if(ret)
        return false;

    // Cuarta pasada: inferencia de tipos
    TypeInferenceVisitor inference(ctx);
    root->accept(inference);
    if (inference.hasError()) {
        inference.printError();
        return false;
    }
    std::cout << "✅ Tipos inferidos correctamente.\n";

    // Cuarta pasada: chequeo de tipos
    TypeCheckerVisitor types(ctx);
    root->accept(types);
    if (types.hasError()) {
        types.printError();
        return false;
    }
    std::cout << "✅ Tipos verificados correctamente.\n";

    return true;
}