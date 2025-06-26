#pragma once

#include "ast/Visitor.hpp"
#include "semantic/Context.hpp"

#include <llvm/IR/LLVMContext.h>  // ✅ define llvm::LLVMContext
#include <llvm/IR/IRBuilder.h>    // ✅ define llvm::IRBuilder
#include <llvm/IR/Module.h>       // ✅ define llvm::Module
#include <llvm/IR/Value.h>        // ✅ define llvm::Value
#include <llvm/IR/Type.h>
#include <llvm/IR/DerivedTypes.h>

#include <string>
#include <unordered_map>
#include <stack>
#include <map>
#include <vector>
#include <memory>  // para std::unique_ptr

// Definir constantes para los tipos
enum HulkType {
    HULK_FLOAT = 0,
    HULK_BOOL = 1,
    HULK_STRING = 2
};

// En lugar de concatenar strings para los maps
struct TypeAttrKey {
    std::string typeName;
    std::string attrName;
    
    TypeAttrKey(const std::string& type, const std::string& attr)
        : typeName(type), attrName(attr) {}

    bool operator==(const TypeAttrKey& other) const {
        return typeName == other.typeName && attrName == other.attrName;
    }
};

namespace std {
    template<> struct hash<TypeAttrKey> {
        size_t operator()(const TypeAttrKey& k) const {
            return hash<string>()(k.typeName) ^ hash<string>()(k.attrName);
        }
    };
}


class LLVMCodeGenVisitor : public Visitor {
public:
    llvm::LLVMContext context;
    llvm::IRBuilder<> builder;
    std::unique_ptr<llvm::Module> module;

    llvm::Value* result;

    Context& ctx;

    LLVMCodeGenVisitor(const std::string& moduleName, Context& c);
    //llvm::Type* getllvmType(ASTNode& node,Type* t);

    void visit(FloatNode& node) override;
    void visit(StringNode& node) override;
    void visit(BoolNode& node) override;
    void visit(BinOpNode& node) override;
    void visit(UnaryOpNode& node) override;
    void visit(BlockNode& node) override;
    void visit(VariableNode& node) override;
    void visit(LetInNode& node) override;
    void visit(FunctionNode& node) override;
    void visit(ProgramNode& node) override;
    void visit(CallFuncNode& node) override;
    void visit(WhileNode& node) override;
    void visit(IfNode& node) override;
    void visit(TypeMember& node) override;
    void visit(TypeNode& node) override;
    void visit(InheritsNode& node) override;
    void visit(AttributeNode& node) override;
    void visit(MethodNode& node) override;
    void visit(NewNode& node) override;
    void visit(MemberAccessNode& node) override;
    void visit(SelfNode &node) override;
    void visit(BaseNode& node) override;
    void visit(MethodCallNode& node) override;
    void visit(IsNode& node) override;
    void visit(AsNode& node) override;

    llvm::Module* getModule() const;

private:
    bool isAssignmentTarget = false;

    llvm::Value* generateBuiltinCall(const std::string& name, const std::vector<llvm::Value*>& args);
    llvm::Function* getPrintFunctionForType(llvm::Type *type);
    llvm::Function* getBuiltinFunction(const std::string& name, llvm::Type* returnType, const std::vector<llvm::Type*>& argTypes);
    llvm::Value* getTypeCode(llvm::Type* type);
    llvm::StructType* defineVTableType(Type* type);
    std::vector<llvm::Type*> getLLVMParamTypesForMethod(Type* type, FunctionType* fnType);
    llvm::Constant* createVTableInstance(Type* type);
    llvm::GlobalVariable* defineVTableGlobal(Type* type, llvm::Constant* vtableInstance);
    void constructTypeMethodStructs(Type* type);
    void declareAllMethods(Type* type);
    std::pair<Type*, FunctionType*> resolveMethod(Type* type, const std::string& methodName);


    llvm::Type* defineTypeStruct(Type* type);
    
    std::unordered_map<TypeAttrKey, ASTNode*> types_init_attr;
    std::unordered_map<std::string, std::vector<ASTNode*>> types_inherits_args;
    std::unordered_map<std::string, Scope*> types_scope;
    std::unordered_map<std::string, Scope*> function_scope;
    std::unordered_map<std::string, std::vector<std::string>> types_constructor_names;
    std::unordered_map<std::string, llvm::GlobalVariable*> types_vtable_globals;
    std::unordered_map<std::string, std::map<std::string, int>> types_method_index_map;
    std::unordered_map<std::string, std::vector<std::string>> types_methods_ordered;

    std::unordered_map<std::string, int> heritage_map;
    std::unordered_map<std::string, ASTNode*> type_node_map;

    
    void defineTypeContructorVariables(Type* type, std::vector<ASTNode*> arguments);
    int getStructFieldIndex(Type *type, const std::string &name);

    std::stack<Type*> current_type_stack;
    
    void push_current_type(Type* type) {
        current_type_stack.push(type);
    }
    
    void pop_current_type() {
        if (!current_type_stack.empty()) {
            current_type_stack.pop();
        }
    }
    
    Type* get_current_type() const {
        return current_type_stack.empty() ? nullptr : current_type_stack.top();
    }

};
