#include "TypeRegistry.hpp"

#pragma once
#include "Type.hpp"
#include <stdexcept>

TypeRegistry::TypeRegistry() {
    // Registra tipos primitivos (sellados)
    types_["Object"] = Type::create_object("Object");
    types_["Number"] = Type::create_primitive("Number");
    types_["String"] = Type::create_primitive("String");
    types_["Boolean"] = Type::create_primitive("Boolean");
}

// Registra un tipo definido por el usuario
std::string TypeRegistry::register_user_type(const std::string& name, const std::string& parent_name = "Object") {
    
    if (is_sealed(parent_name)) {
        return "No se puede heredar del tipo sellado ' " + parent_name + " '.";
    }

    if(types_.find(name) != types_.end())
    {
        return "El tipo ' " + name + " ' ya fue declarado.";
    }

    Type* parent = get_type(parent_name);
    types_[name] = Type::create_object(name, parent);

    return "";
}

// Registra un atributo en un tipo
void TypeRegistry::register_attribute(const std::string& type_name, 
                        const std::string& attr_name, 
                        Type* attr_type) {
    Type* type = get_type(type_name);
    type->object_data.attributes[attr_name] = attr_type;
}

// Registra un método en un tipo
void TypeRegistry::register_method(const std::string& type_name,
                    const std::string& method_name,
                    FunctionType* method_type) {
    Type* type = get_type(type_name);
    type->object_data.methods[method_name] = method_type;
}

// Obtiene un tipo por nombre
Type* TypeRegistry::get_type(const std::string& name) const {
    auto it = types_.find(name);
    if (it == types_.end()) {
        throw std::runtime_error("Type not found: " + name);
    }
    return it->second.get();
}

// Verifica si se puede heredar
bool TypeRegistry::is_sealed(const std::string& type_name) const {
    return type_name == "Number" || type_name == "String" || type_name == "Boolean";
}

// Verifica si el tipo existe
bool TypeRegistry::has_type(const std::string& name) const {
    return types_.count(name) > 0;
}

std::vector<Type*> TypeRegistry::getAncestors(Type* type) const {
    std::vector<Type*> ancestors;
    while (type != nullptr) {
        ancestors.push_back(type);
        type = type->object_data.parent;
    }
    return ancestors;
}

Type* TypeRegistry::findLowestCommonAncestor(Type* type1, Type* type2) const {
    if (!type1 || !type2) return nullptr;
    
    // Caso especial: si son el mismo tipo
    if (type1 == type2) return type1;
    
    // Obtener ancestros de ambos tipos
    auto ancestors1 = getAncestors(type1);
    auto ancestors2 = getAncestors(type2);
    
    // Buscar el primer ancestro común
    for (auto ancestor1 : ancestors1) {
        for (auto ancestor2 : ancestors2) {
            if (ancestor1 == ancestor2) {
                return ancestor1;
            }
        }
    }
    
    // Esto no debería pasar ya que todos los tipos heredan de Object
    return get_type("Object");
}
