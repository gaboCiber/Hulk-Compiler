#pragma once

enum class Type {
    Float,
    Bool,
    String,
    Unknown 
};

constexpr const char* TypeToString(Type t) {
    switch (t) {
        case Type::Float:  return "Float";
        case Type::Bool:   return "Bool";  
        case Type::String: return "String";
        case Type::Unknown: return "Unknown";
        default: return "";
    }
}