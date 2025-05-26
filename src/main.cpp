#include <iostream>
#include "ast/ASTNode.hpp"
#include <fstream>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/FileSystem.h>
#include "codegen/LLVMCodeGenVisitor.hpp"
#include "semantic/Checker.hpp"

// Declaraciones necesarias del parser
extern int yyparse();
extern ASTNode* root;

int main() {
    std::cout << "🔍 Analizando entrada...\n";

    if (yyparse() == 0 && root) {
        std::cout << "✅ Análisis sintáctico exitoso. AST:\n";
        root->print();

        // Context ctx;

        // // Análisis semántico completo
        // if (!Checker::runSemanticAnalysis(root, ctx)) {
        //     delete root;
        //     return 1;
        // }
        
        // LLVMCodeGenVisitor codegen("HulkModule", ctx);
        // root->accept(codegen);

        // // Añadir retorno
        // //codegen.builder.CreateRet(llvm::ConstantInt::get(codegen.builder.getInt32Ty(), 0));
        // //codegen.builder.CreateRet(codegen.result);

        // // Imprimir en consola
        // std::cout << "\n🔧 Código LLVM IR generado:\n";
        // codegen.getModule()->print(llvm::outs(), nullptr);

        // // Guardar en archivo .ll
        // std::error_code EC;
        // llvm::raw_fd_ostream outFile("hulk/output.ll", EC, llvm::sys::fs::OF_Text);
        // if (!EC) {
        //     codegen.getModule()->print(outFile, nullptr);
        //     std::cout << "\n💾 IR guardado en hulk/output.ll\n";
        // } else {
        //     std::cerr << "❌ No se pudo guardar IR: " << EC.message() << "\n";
        // }
    }

    delete root;
    return 0;
}
