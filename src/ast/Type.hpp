#pragma once
#include <string>
#include <vector>
#include <unordered_map>
#include <memory>

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

    // Solo para tipos objeto
    struct {
        Type* parent = nullptr; // Herencia (Object si es nullptr)
        std::unordered_map<std::string, Type*> attributes;
        std::unordered_map<std::string, FunctionType*> methods;
    } object_data;


    // Constructores
    static std::shared_ptr<Type> create_primitive(const std::string& name);
    static std::shared_ptr<Type> create_object(const std::string& name, Type* parent = nullptr) ;
    
    // Helpers
    bool is_primitive() const;
    bool is_subtype_of(const Type* other) const;
};