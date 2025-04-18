#include <iostream>
#include "ast/ASTNode.hpp"
#include "semantic/SemanticChecker.hpp"
#include <fstream>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/FileSystem.h>
#include "codegen/LLVMCodeGenVisitor.hpp"


// Declaraciones necesarias del parser
extern int yyparse();
extern ASTNode* root;

int main() {
    std::cout << "🔍 Analizando entrada...\n";

    if (yyparse() == 0 && root) {
        std::cout << "✅ Análisis sintáctico exitoso. AST:\n";
        root->print();

        std::cout << "\n🔎 Iniciando análisis semántico...\n";
        SemanticChecker checker;
        root->accept(checker);
        
        LLVMCodeGenVisitor codegen("HulkModule");
        root->accept(codegen);

        // Añadir retorno
        codegen.builder.CreateRet(codegen.result);
        
        // Imprimir en consola
        std::cout << "\n🔧 Código LLVM IR generado:\n";
        codegen.getModule()->print(llvm::outs(), nullptr);

        
        
        // // Guardar en archivo .ll
        // std::error_code EC;
        // llvm::raw_fd_ostream outFile("build/output.ll", EC, llvm::sys::fs::OF_Text);
        // if (!EC) {
        //     codegen.getModule()->print(outFile, nullptr);
        //     std::cout << "\n💾 IR guardado en build/output.ll\n";
        // } else {
        //     std::cerr << "❌ No se pudo guardar IR: " << EC.message() << "\n";
        // }
    } else {
        std::cerr << "❌ Error de sintaxis.\n";
    }

    return 0;
}
