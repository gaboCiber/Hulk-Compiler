#pragma once
#include <string>
#include <memory>
#include <iostream>
#include <vector>

class Visitor;

class ASTNode {
public:
    virtual ~ASTNode() {}
    virtual void print(int indent = 0) const = 0;
    virtual void accept(Visitor& visitor) = 0; 
};

class FloatNode : public ASTNode {
public:
    float value;
    FloatNode(float v);
    void print(int indent = 0) const override;
    void accept(Visitor& visitor) override;
};

class BoolNode : public ASTNode {
public:
    bool value;
    BoolNode(bool v);
    void print(int indent = 0) const override;
    void accept(Visitor& visitor) override;
};

class StringNode : public ASTNode {
public:
    std::string value;
    StringNode(const std::string& v);
    void print(int indent = 0) const override;
    void accept(Visitor& visitor) override;
};


class UnaryOpNode : public ASTNode {
public:
    std::string op;
    ASTNode* node;

    UnaryOpNode(const std::string& o, ASTNode* n);
    void print(int indent = 0) const override;
    void accept(Visitor& visitor) override;

}; 

class BinOpNode : public ASTNode {
public:
    std::string op;
    ASTNode* left;
    ASTNode* right;

    BinOpNode(const std::string& o, ASTNode* l, ASTNode* r);
    void print(int indent = 0) const override;
    void accept(Visitor& visitor) override;

};

class BlockNode : public ASTNode {
public:
    std::vector<ASTNode*> statements;
    void push_back(ASTNode* node); 
    void print(int indent = 0) const override;
    void accept(Visitor& visitor) override;
};
