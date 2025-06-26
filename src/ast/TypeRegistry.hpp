#pragma once
#include "Type.hpp"
#include <stdexcept>

class TypeRegistry {
    std::unordered_map<std::string, std::shared_ptr<Type>> types_;

public:
    TypeRegistry();

    // Registra un tipo definido por el usuario
    std::string register_user_type(const std::string& name, const std::vector<std::string> arguments);

    // Registra un atributo en un tipo
    void register_attribute(const std::string& type_name, const std::string& attr_name, Type* attr_type);

    // Registra un método en un tipo
    void register_method(const std::string& type_name, const std::string& method_name, FunctionType* method_type);

    void register_parent(Type* type, Type* parent);

    // Obtiene un tipo por nombre
    Type* get_type(const std::string& name) const;

    // Verifica si existe un tipo por nombre
    bool has_type(const std::string& name) const;

    // Encuentra el ancestro común más bajo entre dos tipos
    Type* findLowestCommonAncestor(Type* type1, Type* type2) const;
 
private:
    std::vector<Type*> getAncestors(Type* type) const;

};