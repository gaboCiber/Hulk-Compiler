#include "Type.hpp"

// Constructores
std::shared_ptr<Type> Type::create_primitive(const std::string& name) {
    auto type = std::make_shared<Type>();
    type->kind = Kind::PRIMITIVE;
    type->name = name;
    return type;
}

std::shared_ptr<Type> Type::create_object(const std::string& name, Type* parent = nullptr) {
    auto type = std::make_shared<Type>();
    type->kind = Kind::OBJECT;
    type->name = name;
    type->object_data.parent = parent;
    return type;
}

// Helpers
bool Type::is_primitive() const { 
    return kind == Kind::PRIMITIVE; 
}

bool Type::is_subtype_of(const Type* other) const {
    // Todos los tipos son subtipos de Object
    if (other->name == "Object") 
        return true;

    // Tipos primitivos son iguales o no relacionados
    if (this->is_primitive()) 
        return this->name == other->name;

    // Chequea herencia para tipos objeto
    const Type* current = this;
    while (current != nullptr) {
        if (current->name == other->name) 
            return true;
        current = current->object_data.parent;
    }
    return false;
}