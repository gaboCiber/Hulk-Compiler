#pragma once
#include <string>
#include <vector>
#include <unordered_map>
#include <map>
#include <memory>
#include <llvm/IR/Type.h>


class Type;

// Solo para funciones (first-class citizens)
struct FunctionType {
    std::vector<Type*> parameter_types;
    Type* return_type;
};

class Type {
public:
    enum class Kind {
        PRIMITIVE,  // Number, String, Boolean (no heredables)
        OBJECT      // Tipos definidos por el usuario
    };

    Kind kind;
    std::string name;
    llvm::Type* llvm_type;

    // Solo para tipos objeto
    struct {
        Type* parent = nullptr; // Herencia (Object si es nullptr)
        std::unordered_map<std::string, Type*> constructor;
        std::map<std::string, Type*> attributes;
        std::unordered_map<std::string, FunctionType*> methods;
    } object_data;


    // Constructores
    static std::shared_ptr<Type> create_primitive(const std::string& name);
    static std::shared_ptr<Type> create_object(const std::string& name, const std::vector<std::string> arguments, Type* parent = nullptr) ;
    
    // Helpers
    bool is_primitive() const;
    bool is_subtype_of(const Type* other) const;
    static std::string TypeToString(Type* t) { return t->name;}
};