# === VARIABLES GLOBALES ===
CXX = g++
FLEX = flex
BISON = bison

CXXFLAGS = -Wall -std=c++17 -Isrc -Isrc/ast
OUT_DIR = build
SRC_DIR = src
LEXER_DIR = $(SRC_DIR)/lexer
PARSER_DIR = $(SRC_DIR)/parser

LEX_SRC = $(LEXER_DIR)/lexer.l
YACC_SRC = $(PARSER_DIR)/parser.y
MAIN_SRC = $(SRC_DIR)/main.cpp

LEX_C = $(OUT_DIR)/lex.yy.c
YACC_C = $(OUT_DIR)/parser.tab.c
YACC_H = $(OUT_DIR)/parser.tab.h

LEX_OBJ = $(OUT_DIR)/lex.yy.o
YACC_OBJ = $(OUT_DIR)/parser.tab.o
MAIN_OBJ = $(OUT_DIR)/main.o

AST_SRC = $(SRC_DIR)/ast/ASTNode.cpp
AST_OBJ = $(OUT_DIR)/ASTNode.o

SEMANTIC_SRC = $(SRC_DIR)/semantic/SemanticChecker.cpp
SEMANTIC_OBJ = $(OUT_DIR)/SemanticChecker.o

LLVM_CONFIG = llvm-config
LLVM_CXXFLAGS = $(shell $(LLVM_CONFIG) --cxxflags)
LLVM_LDFLAGS  = $(shell $(LLVM_CONFIG) --ldflags --libs core) -Wno-unused-command-line-argument


CODEGEN_SRC = $(SRC_DIR)/codegen/LLVMCodeGenVisitor.cpp
CODEGEN_OBJ = $(OUT_DIR)/LLVMCodeGenVisitor.o

OBJS = $(MAIN_OBJ) $(LEX_OBJ) $(YACC_OBJ) $(AST_OBJ) $(SEMANTIC_OBJ) $(CODEGEN_OBJ)

EXEC = build/hulk-compiler
SCRIPT_FILE = build/script.hulk

# === TARGETS ===

all: compile

compile: $(OUT_DIR) $(EXEC) $(SCRIPT_FILE)
	@echo "✅ Build completo. Ejecutable en $(EXEC)"

run: compile
	@echo "🚀 Ejecutando script.hulk y mostrando AST..."
	@$(EXEC) < $(SCRIPT_FILE)

clean:
	rm -rf build tmp
	@echo "🧹 Proyecto limpiado."

# === REGLAS DE COMPILACIÓN ===

$(OUT_DIR):
	mkdir -p $(OUT_DIR)

$(EXEC): $(OBJS)
	$(CXX) $(CXXFLAGS) $(LLVM_CXXFLAGS) -o $(EXEC) $(OBJS) $(LLVM_LDFLAGS)

# Generación de objetos
$(MAIN_OBJ): $(MAIN_SRC)
	$(CXX) $(CXXFLAGS) -c $< -o $@

$(LEX_OBJ): $(LEX_C)
	$(CXX) $(CXXFLAGS) -c $< -o $@

$(YACC_OBJ): $(YACC_C)
	$(CXX) $(CXXFLAGS) -c $< -o $@

# Flex y Bison
$(LEX_C): $(LEX_SRC) $(YACC_H)
	$(FLEX) -o $@ $<


$(YACC_C) $(YACC_H): $(YACC_SRC)
	$(BISON) -d -o $(YACC_C) $(YACC_SRC)

# AST
$(AST_OBJ): $(AST_SRC)
	$(CXX) $(CXXFLAGS) -c $< -o $@

# Semantic
$(SEMANTIC_OBJ): $(SEMANTIC_SRC)
	$(CXX) $(CXXFLAGS) -c $< -o $@

# LLVM Code Generation
$(CODEGEN_OBJ): $(CODEGEN_SRC)
	$(CXX) $(CXXFLAGS) $(LLVM_CXXFLAGS) -c $< -o $@

# Verificar o crear script.hulk
$(SCRIPT_FILE):
	@if [ ! -f "$(SCRIPT_FILE)" ]; then touch "$(SCRIPT_FILE)"; fi

# === META ===
.PHONY: all build run clean
