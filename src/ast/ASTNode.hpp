#pragma once
#include "Visitor.hpp"
#include <string>
#include <memory>
#include <iostream>
#include <vector>
#include "Type.hpp"

class Visitor;
class Scope;

class ASTNode {
public:
    int line;
    ASTNode(int l = -1) : line(l) {}
    virtual ~ASTNode() {}
    virtual void print(int indent = 0) const = 0;
    virtual void accept(Visitor& visitor) = 0; 
};

class FloatNode : public ASTNode {
public:
    float value;
    FloatNode(float v, int l);
    void print(int indent = 0) const override;
    void accept(Visitor& visitor) override;
};

class BoolNode : public ASTNode {
public:
    bool value;
    BoolNode(bool v, int l);
    void print(int indent = 0) const override;
    void accept(Visitor& visitor) override;
};

class StringNode : public ASTNode {
public:
    std::string value;
    StringNode(const std::string& v, int l);
    void print(int indent = 0) const override;
    void accept(Visitor& visitor) override;
};

class UnaryOpNode : public ASTNode {
public:
    std::string op;
    ASTNode* node;

    UnaryOpNode(const std::string& o, ASTNode* n, int l);
    void print(int indent = 0) const override;
    void accept(Visitor& visitor) override;
    ~UnaryOpNode();
}; 

class BinOpNode : public ASTNode {
public:
    std::string op;
    ASTNode* left;
    ASTNode* right;

    BinOpNode(const std::string& o, ASTNode* l, ASTNode* r, int ln);
    void print(int indent = 0) const override;
    void accept(Visitor& visitor) override;
    ~BinOpNode();

};

class BlockNode : public ASTNode {
public:
    std::vector<ASTNode*> statements;
    void push_back(ASTNode* node); 
    void print(int indent = 0) const override;
    void accept(Visitor& visitor) override;
    ~BlockNode();
};


class VariableNode: public ASTNode {
public:
    std::string name;
    VariableNode(const std::string& n, int l);
    void print(int indent = 0) const override;
    void accept(Visitor& visitor) override;
};

class LetInNode : public ASTNode {
public:
    std::vector<std::pair<VariableNode*, ASTNode*>> bindings;
    BlockNode* block;
    Scope* scope = nullptr;

    LetInNode(const std::vector<std::pair<VariableNode*, ASTNode*>>& b, BlockNode* blk, int l);
    void print(int indent = 0) const override;
    void accept(Visitor& visitor) override;
    ~LetInNode();
};

class FunctionNode: public ASTNode {
public:
    std::string name;
    std::vector<VariableNode*> args;
    BlockNode* block;
    Scope* scope = nullptr;
    
    FunctionNode(const std::string& n, const std::vector<VariableNode*>& a, BlockNode* blk, int l);
    void print(int indent = 0) const override;
    void accept(Visitor& visitor) override;
    ~FunctionNode();
};

class ProgramNode : public ASTNode {
public:
    std::vector<FunctionNode*> functions;
    std::vector<TypeNode*> types;
    std::vector<ASTNode*> statements;  // Unificamos blocks y lines aquí
    
    void push_func(FunctionNode* func) { functions.push_back(func); }
    void push_statement(ASTNode* stmt) { statements.push_back(stmt); }
    void push_types(TypeNode* ty)      { types.push_back(ty); }
    
    void print(int indent = 0) const override;
    void accept(Visitor& visitor) override;
    ~ProgramNode() override;
};

class CallFuncNode : public ASTNode {
public:
    std::string functionName;
    std::vector<ASTNode*> arguments;
    
    CallFuncNode(const std::string& name, const std::vector<ASTNode*>& args, int line)
        : ASTNode(line), functionName(name), arguments(args) {}
    
    void print(int indent = 0) const override;
    void accept(Visitor& visitor) override;
    ~CallFuncNode();
};

class WhileNode : public ASTNode {
public:
    ASTNode* condition;
    BlockNode* body;
    Type returnType;
    
    WhileNode(ASTNode* cond, BlockNode* b, int line);
    void print(int indent = 0) const override;
    void accept(Visitor& visitor) override;
    ~WhileNode();
};

class IfNode : public ASTNode {
    std::vector<std::pair<ASTNode*, ASTNode*>> branches; // (condición, cuerpo)
    ASTNode* elseBranch;
    
public:
    Type returnType;

    IfNode(ASTNode* cond, ASTNode* then, ASTNode* elseBr, int line);
    void addElifBranch(ASTNode* cond, ASTNode* branch);
    void print(int indent = 0) const override;
    void accept(Visitor& visitor) override;
    ~IfNode();
    
    const auto& getBranches() const { return branches; }
    ASTNode* getElseBranch() const { return elseBranch; }
};




class TypeNode : public ASTNode {
public:
    std::string name;
    std::vector<VariableNode*>* type_args;
    InheritsNode* inherits;
    std::vector<TypeMember*> members;

    TypeNode(const std::string& name, 
            std::vector<VariableNode*>* args,
            InheritsNode* inherits,
            const std::vector<TypeMember*>& members,
            int line);
    ~TypeNode();  // Solo declaración
    void print(int indent = 0) const override;
    void accept(Visitor& visitor) override;  // Solo declaración
};

class InheritsNode : public ASTNode {
public:
    std::string parent_type;
    std::vector<ASTNode*>* parent_args;

    InheritsNode(const std::string& parent, 
                std::vector<ASTNode*>* args,
                int line);
    ~InheritsNode();  // Solo declaración
    void print(int indent = 0) const override;
    void accept(Visitor& visitor) override;  // Solo declaración
};

class TypeMember : public ASTNode {
public:
    enum class Kind { Attribute, Method };
    
    TypeMember(Kind kind, int line) : ASTNode(line), kind(kind) {}
    virtual ~TypeMember() = default;
    
    Kind getKind() const { return kind; }
    virtual std::string getName() const = 0;
    virtual void print(int indent = 0) const override = 0;
    virtual void accept(Visitor& visitor) override = 0;

private:
    Kind kind;
};

class AttributeNode : public TypeMember {
public:
    AttributeNode(const std::string& name, ASTNode* init, int line);
    ~AttributeNode();
    
    std::string getName() const override { return name; }
    void print(int indent = 0) const override;
    void accept(Visitor& visitor) override;

private:
    std::string name;
    ASTNode* initializer;
};

class MethodNode : public TypeMember {
public:
    MethodNode(const std::string& name,
              std::vector<VariableNode*>* params,
              ASTNode* body,
              int line);
    ~MethodNode();
    
    std::string getName() const override { return name; }
    void print(int indent = 0) const override;
    void accept(Visitor& visitor) override;

private:
    std::string name;
    std::vector<VariableNode*>* parameters;
    ASTNode* body;
};

class NewNode : public ASTNode {
public:
    std::string type_name;
    std::vector<ASTNode*>* arguments;

    NewNode(const std::string& type, std::vector<ASTNode*>* args, int line);
    ~NewNode();  // Solo declaración
    void print(int indent = 0) const override;
    void accept(Visitor& visitor) override;  // Solo declaración
};

class MemberAccessNode : public ASTNode {
public:
    ASTNode* object;
    std::string member_name;

    MemberAccessNode(ASTNode* obj, const std::string& member, int line);
    ~MemberAccessNode();  // Solo declaración
    void print(int indent = 0) const override;
    void accept(Visitor& visitor) override;  // Solo declaración
};

class SelfNode : public ASTNode {
public:
    SelfNode(int line);
    void print(int indent = 0) const override;
    void accept(Visitor& visitor) override;  // Solo declaración
};

class BaseNode : public ASTNode {
public:
    std::vector<ASTNode*>* arguments;

    BaseNode(std::vector<ASTNode*>* arg, int line);
    void print(int indent = 0) const override;
    void accept(Visitor& visitor) override;  // Solo declaración
};


class MethodCallNode : public ASTNode {
public:
    MethodCallNode(ASTNode* obj, 
                  const std::string& name, 
                  const std::vector<ASTNode*>& args, 
                  int line);
    ~MethodCallNode();
    
    void print(int indent = 0) const override;
    void accept(Visitor& visitor) override;
    
    ASTNode* getObject() const { return object; }
    const std::string& getMethodName() const { return method_name; }
    const std::vector<ASTNode*>& getArguments() const { return arguments; }

private:
    ASTNode* object;
    std::string method_name;
    std::vector<ASTNode*> arguments;
};

