#include "ASTNode.hpp"
#include "Visitor.hpp"

// FloatNode implementation
FloatNode::FloatNode(float v, int l) : ASTNode(l), value(v) {}

void FloatNode::print(int indent) const {
    std::cout << std::string(indent, ' ') << "Float(" << value << ")\n";
}

void FloatNode::accept(Visitor& visitor) {
    visitor.visit(*this);
}

// BoolNode implementation
BoolNode::BoolNode(bool v, int l) : ASTNode(l), value(v) {}

void BoolNode::print(int indent) const {
    std::cout << std::string(indent, ' ') << "Bool(" << value << ")\n";
}

void BoolNode::accept(Visitor& visitor) {
    visitor.visit(*this);
}

// StringNode implementation
StringNode::StringNode(const std::string& v, int l) : ASTNode(l), value(v) {}

void StringNode::print(int indent) const {
    std::cout << std::string(indent, ' ') << "String(\"" << value << "\")\n";
}

void StringNode::accept(Visitor& visitor) {
    visitor.visit(*this);
}

// UnaryOpNode implementation
UnaryOpNode::UnaryOpNode(const std::string& o, ASTNode* n, int l) 
: ASTNode(l), op(o), node(n) {}

void UnaryOpNode::print(int indent) const {
    std::cout << std::string(indent, ' ') << "UnaryOp(" << op << ")\n";
    node->print(indent + 2);
}

void UnaryOpNode::accept(Visitor& visitor) {
    visitor.visit(*this);
}

UnaryOpNode::~UnaryOpNode() {
    delete node;
}


// BinOpNode implementation
BinOpNode::BinOpNode(const std::string& o, ASTNode* l, ASTNode* r, int ln) 
    : ASTNode(ln), op(o), left(l), right(r) {}

void BinOpNode::print(int indent) const {
    std::cout << std::string(indent, ' ') << "BinOp(" << op << ")\n";
    left->print(indent + 2);
    right->print(indent + 2);
}

void BinOpNode::accept(Visitor& visitor) {
    visitor.visit(*this);
}

BinOpNode::~BinOpNode() {
    delete left;
    delete right;
}

// BlockNode implementation
void BlockNode::push_back(ASTNode* node) {
    statements.push_back(node);
}

void BlockNode::print(int indent) const {
    std::cout << std::string(indent, ' ') << "Block:\n";
    for (const auto& statement : statements) {
        statement->print(indent + 2);
    }
}

void BlockNode::accept(Visitor& visitor) {
    visitor.visit(*this);
}

BlockNode::~BlockNode() {
    for (auto stmt : statements) {
        delete stmt;
    }
}

// VariableNode Implementation
VariableNode::VariableNode(const std::string& n, int l) : ASTNode(l), name(n) {}

void VariableNode::print(int indent) const {
    std::cout << std::string(indent, ' ') << "Variable(" << name << ")\n";
}

void VariableNode::accept(Visitor& visitor) {
    visitor.visit(*this);
}

// LetInNode Implementation
LetInNode::LetInNode(const std::vector<std::pair<VariableNode*, ASTNode*>>& b, BlockNode* blk, int l)
    : ASTNode(l), bindings(b), block(blk) {}

void LetInNode::print(int indent) const {
    std::cout << std::string(indent, ' ') << "Let\n";
    for (const auto& pair : bindings) {
        std::cout << std::string(indent + 2, ' ') << pair.first->name << " =\n";
        pair.second->print(indent + 4);
    }
    std::cout << std::string(indent, ' ') << "In\n";
    block->print(indent + 2);
}

void LetInNode::accept(Visitor& visitor) {
    visitor.visit(*this);
}

LetInNode::~LetInNode() {
    for (auto& pair : bindings) {
        delete pair.first;
        delete pair.second;
    }
    delete block;
}

// FunctionNode Implementation
FunctionNode::FunctionNode(const std::string& n, const std::vector<VariableNode*>& a, BlockNode* blk, int l)
    : ASTNode(l), name(n), args(a), block(blk) {} 

void FunctionNode::print(int indent) const {
    std::cout << std::string(indent, ' ') << "Function " << name << " ( ";
    for (const auto& a : args) {
        std::cout << a->name << " ";
    }
    std::cout << std::string(indent, ' ') << ")\n";
    block->print(indent + 2);
}

void FunctionNode::accept(Visitor& visitor) {
    visitor.visit(*this);
}

FunctionNode::~FunctionNode() {
    for (auto& a : args) {
        delete a;
    }

    delete block;
} 

// En ASTNode.cpp - Implementación de ProgramNode

void ProgramNode::print(int indent) const {
    std::cout << std::string(indent, ' ') << "Program:\n";
    
    // Imprimir funciones primero
    for (const auto& func : functions) {
        func->print(indent + 2);
    }
    
    // Imprimir statements (bloques y líneas)
    for (const auto& stmt : statements) {
        stmt->print(indent + 2);
    }
}

void ProgramNode::accept(Visitor& visitor) {
    visitor.visit(*this);
}

ProgramNode::~ProgramNode() {
    // Liberar memoria de funciones
    for (auto func : functions) {
        delete func;
    }
    
    // Liberar memoria de statements
    for (auto stmt : statements) {
        delete stmt;
    }
}

void CallFuncNode::print(int indent) const {
    std::cout << std::string(indent, ' ') << "CallFunc: " << functionName << "\n";
    for (const auto& arg : arguments) {
        arg->print(indent + 2);
    }
}

void CallFuncNode::accept(Visitor& visitor) {
    visitor.visit(*this);
}

CallFuncNode::~CallFuncNode() {
    for (auto arg : arguments) {
        delete arg;
    }
}

WhileNode::WhileNode(ASTNode* cond, BlockNode* body, int line)
    : ASTNode(line), condition(cond), body(body) {}

void WhileNode::print(int indent) const {
    std::cout << std::string(indent, ' ') << "While:\n";
    std::cout << std::string(indent + 2, ' ') << "Condition:\n";
    condition->print(indent + 4);
    std::cout << std::string(indent + 2, ' ') << "Body:\n";
    body->print(indent + 4);
}

void WhileNode::accept(Visitor& visitor) {
    visitor.visit(*this);
}

WhileNode::~WhileNode() {
    delete condition;
    delete body;
}



IfNode::IfNode(ASTNode* cond, ASTNode* then, ASTNode* elseBr, int line)
    : ASTNode(line), elseBranch(elseBr) {
    branches.emplace_back(cond, then);
}

void IfNode::addElifBranch(ASTNode* cond, ASTNode* branch) {
    branches.emplace_back(cond, branch);
}

void IfNode::print(int indent) const {
    std::cout << std::string(indent, ' ') << "If:\n";
    
    for (size_t i = 0; i < branches.size(); ++i) {
        const auto& [cond, branch] = branches[i];
        std::string label = (i == 0) ? "If" : "Elif";
        
        std::cout << std::string(indent + 2, ' ') << label << " Condition:\n";
        cond->print(indent + 4);
        std::cout << std::string(indent + 2, ' ') << label << " Body:\n";
        branch->print(indent + 4);
    }
    
    if (elseBranch) {
        std::cout << std::string(indent + 2, ' ') << "Else:\n";
        elseBranch->print(indent + 4);
    }
}

void IfNode::accept(Visitor& visitor) {
    visitor.visit(*this);
}

IfNode::~IfNode() {
    for (auto& [cond, branch] : branches) {
        delete cond;
        delete branch;
    }
    delete elseBranch;
}







TypeNode::TypeNode(const std::string& name, 
                 std::vector<VariableNode*>* args,
                 InheritsNode* inherits,
                 const std::vector<TypeMember*>& members,
                 int line) :
    ASTNode(line), name(name), type_args(args), 
    inherits(inherits), members(members) {}

TypeNode::~TypeNode() {
    for (auto arg : *type_args) delete arg;
    delete type_args;
    delete inherits;
    for (auto member : members) delete member;
}

void TypeNode::print(int indent) const {
    std::cout << std::string(indent, ' ') << "TypeDeclaration: " << name << "\n";
    std::cout << std::string(indent + 2, ' ') << "Type Parameters:\n";
    for (const auto& arg : *type_args) {
        arg->print(indent + 4);
    }
    
    if (inherits) {
        std::cout << std::string(indent + 2, ' ') << "Inherits:\n";
        inherits->print(indent + 4);
    }
    
    std::cout << std::string(indent + 2, ' ') << "Members:\n";
    for (const auto& member : members) {
        member->print(indent + 4);
    }
}

void TypeNode::accept(Visitor& visitor) {
    visitor.visit(*this);
}

// =================== InheritsNode ===================
InheritsNode::InheritsNode(const std::string& parent, 
                         std::vector<ASTNode*>* args,
                         int line) :
    ASTNode(line), parent_type(parent), parent_args(args) {}

InheritsNode::~InheritsNode() {
    for (auto arg : *parent_args) delete arg;
    delete parent_args;
}

void InheritsNode::print(int indent) const {
    std::cout << std::string(indent, ' ') << "InheritsFrom: " << parent_type << "\n";
    if (!parent_args->empty()) {
        std::cout << std::string(indent + 2, ' ') << "Parent Arguments:\n";
        for (const auto& arg : *parent_args) {
            arg->print(indent + 4);
        }
    }
}

void InheritsNode::accept(Visitor& visitor) {
    visitor.visit(*this);
}

// ================== AttributeNode ==================
AttributeNode::AttributeNode(const std::string& name, 
                           ASTNode* init, 
                           int line) :
    TypeMember(TypeMember::Kind::Attribute, line), 
    name(name), initializer(init) {}

AttributeNode::~AttributeNode() {
    delete initializer;
}

void AttributeNode::print(int indent) const {
    std::cout << std::string(indent, ' ') << "Attribute: " << name << "\n";
    std::cout << std::string(indent + 2, ' ') << "Initializer:\n";
    initializer->print(indent + 4);
}

void AttributeNode::accept(Visitor& visitor) {
    visitor.visit(*this);
}

// =================== MethodNode ====================
MethodNode::MethodNode(const std::string& name,
                     std::vector<VariableNode*>* params,
                     ASTNode* body,
                     int line) :
    TypeMember(TypeMember::Kind::Method, line), 
    name(name), parameters(params), body(body) {}

MethodNode::~MethodNode() {
    for (auto param : *parameters) delete param;
    delete parameters;
    delete body;
}

void MethodNode::print(int indent) const {
    std::cout << std::string(indent, ' ') << "Method: " << name << "\n";
    std::cout << std::string(indent + 2, ' ') << "Parameters:\n";
    for (const auto& param : *parameters) {
        param->print(indent + 4);
    }
    std::cout << std::string(indent + 2, ' ') << "Body:\n";
    body->print(indent + 4);
}

void MethodNode::accept(Visitor& visitor) {
    visitor.visit(*this);
}

// ===================== NewNode =====================
NewNode::NewNode(const std::string& type, 
               std::vector<ASTNode*>* args, 
               int line) :
    ASTNode(line), type_name(type), arguments(args) {}

NewNode::~NewNode() {
    for (auto arg : *arguments) delete arg;
    delete arguments;
}

void NewNode::print(int indent) const {
    std::cout << std::string(indent, ' ') << "NewInstance: " << type_name << "\n";
    if (!arguments->empty()) {
        std::cout << std::string(indent + 2, ' ') << "Arguments:\n";
        for (const auto& arg : *arguments) {
            arg->print(indent + 4);
        }
    }
}

void NewNode::accept(Visitor& visitor) {
    visitor.visit(*this);
}

// ================ MemberAccessNode =================
MemberAccessNode::MemberAccessNode(ASTNode* obj, 
                                  const std::string& member, 
                                  int line) :
    ASTNode(line), object(obj), member_name(member) {}

MemberAccessNode::~MemberAccessNode() {
    delete object;
}

void MemberAccessNode::print(int indent) const {
    std::cout << std::string(indent, ' ') << "MemberAccess: ." << member_name << "\n";
    std::cout << std::string(indent + 2, ' ') << "Object:\n";
    object->print(indent + 4);
}

void MemberAccessNode::accept(Visitor& visitor) {
    visitor.visit(*this);
}

// ==================== SelfNode =====================
SelfNode::SelfNode(int line) : ASTNode(line) {}

void SelfNode::print(int indent) const {
    std::cout << std::string(indent, ' ') << "SelfReference\n";
}

void SelfNode::accept(Visitor& visitor) {
    visitor.visit(*this);
}

// ==================== BaseNode =====================
BaseNode::BaseNode(int line) : ASTNode(line) {}

void BaseNode::print(int indent) const {
    std::cout << std::string(indent, ' ') << "BaseReference\n";
}

void BaseNode::accept(Visitor& visitor) {
    visitor.visit(*this);
}



MethodCallNode::MethodCallNode(ASTNode* obj, 
                             const std::string& name,
                             const std::vector<ASTNode*>& args,
                             int line) :
    ASTNode(line), 
    object(obj),
    method_name(name),
    arguments(args) {}

MethodCallNode::~MethodCallNode() {
    delete object;
    for (auto arg : arguments) {
        delete arg;
    }
}

void MethodCallNode::print(int indent) const {
    std::cout << std::string(indent, ' ') << "MethodCall: " << method_name << "\n";
    std::cout << std::string(indent + 2, ' ') << "Object:\n";
    object->print(indent + 4);
    if (!arguments.empty()) {
        std::cout << std::string(indent + 2, ' ') << "Arguments:\n";
        for (auto arg : arguments) {
            arg->print(indent + 4);
        }
    }
}

void MethodCallNode::accept(Visitor& visitor) {
    visitor.visit(*this);
}