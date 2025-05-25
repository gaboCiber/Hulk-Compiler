#pragma once

enum class Type {
    Float,
    Bool,
    String,
    Any,
    Unknown 
};

constexpr const char* TypeToString(Type t) {
    switch (t) {
        case Type::Float:  return "Float";
        case Type::Bool:   return "Bool";  
        case Type::String: return "String";
        case Type::Unknown: return "Unknown";
        case Type::Any:     return "Any";
        default: return "";
    }
}