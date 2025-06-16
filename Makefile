# === VARIABLES GLOBALES ===
CXX = g++
FLEX = flex
BISON = bison
LLC = llc
CLANG = clang
CC = gcc

CXXFLAGS = -Wall -std=c++17 -Isrc -Isrc/ast -Ihulk/parser

# LLVM
LLVM_CONFIG = llvm-config
LLVM_CXXFLAGS = $(shell $(LLVM_CONFIG) --cxxflags)
LLVM_LDFLAGS  = $(shell $(LLVM_CONFIG) --ldflags --libs core) -Wno-unused-command-line-argument -lm

# Directorios
SRC_DIR = src
OUT_DIR = hulk
LEXER_DIR = $(SRC_DIR)/lexer
PARSER_DIR = $(SRC_DIR)/parser

# Archivos fuentes
LEX_SRC = $(LEXER_DIR)/lexer.l
YACC_SRC = $(PARSER_DIR)/parser.y
MAIN_SRC = $(SRC_DIR)/main.cpp

# Archivos generados por flex y bison
LEX_C  = $(OUT_DIR)/lexer/lex.yy.c
YACC_C = $(OUT_DIR)/parser/parser.tab.c
YACC_H = $(OUT_DIR)/parser/parser.tab.h

# Objetos explícitos
MAIN_OBJ = $(OUT_DIR)/main.o
LEX_OBJ = $(OUT_DIR)/lexer/lex.yy.o
YACC_OBJ = $(OUT_DIR)/parser/parser.tab.o

# Runtime para el LLVM IR
RUNTIME_SRC = $(SRC_DIR)/codegen/runtime.c
RUNTIME_OBJ = $(OUT_DIR)/codegen/runtime.o

# Detectar automáticamente todos los *.cpp de src/
CPP_SRC := $(shell find $(SRC_DIR) -name "*.cpp" ! -name "main.cpp")
CPP_OBJ := $(patsubst $(SRC_DIR)/%.cpp, $(OUT_DIR)/%.o, $(CPP_SRC))

OBJS = $(MAIN_OBJ) $(LEX_OBJ) $(YACC_OBJ) $(CPP_OBJ)

# Ejecutable y archivo de entrada
EXEC = $(OUT_DIR)/hulk-compiler
SCRIPT_FILE = script.hulk
LLVM_IR = $(OUT_DIR)/output.ll
LLVM_S = $(OUT_DIR)/output.s

# Variable para archivo personalizado (nueva)
FILE ?= $(SCRIPT_FILE)

# === TARGETS ===

all: compile

build: $(OUT_DIR) $(EXEC) $(SCRIPT_FILE)
	@echo "✅ Build completo. Ejecutable en $(EXEC)"

compile: build
	@rm $(OUT_DIR)/output* || true
	@echo "🚀 Ejecutando script.hulk y generando IR..."
	@$(EXEC) < $(SCRIPT_FILE)
	@$(LLC) $(LLVM_IR) -o $(LLVM_S)
	@$(CC) -c $(RUNTIME_SRC) -o $(RUNTIME_OBJ)
	@$(CLANG) -fPIE -no-pie $(LLVM_S) $(RUNTIME_OBJ) -o $(OUT_DIR)/output $(LLVM_LDFLAGS)

execute: compile
	@echo "🚀 Ejecutando output  ..."
	@./$(OUT_DIR)/output

run:
	@./$(OUT_DIR)/output

# Nueva regla para ejecutar un archivo específico
exepath: $(EXEC) $(RUNTIME_OBJ)
	@if [ -z "$(FILE)" ]; then \
		echo "Error: FILE no definido. Uso: make exepath FILE=ruta/archivo.hulk"; \
		exit 1; \
	fi
	@rm -f $(LLVM_IR) $(LLVM_S) $(OUT_DIR)/output
	@echo "🚀 Compilando $(FILE) ..."
	@$(EXEC) < $(FILE)
	@$(LLC) $(LLVM_IR) -o $(LLVM_S)
	@$(CLANG) -fPIE -no-pie $(LLVM_S) $(RUNTIME_OBJ) -o $(OUT_DIR)/output $(LLVM_LDFLAGS)
	@echo "🚀 Ejecutando $(FILE) ..."
	@./$(OUT_DIR)/output

clean:
	rm -rf $(OUT_DIR)
	@echo "🧹 Proyecto limpiado."

# === REGLAS DE COMPILACIÓN ===

$(OUT_DIR):
	mkdir -p $(OUT_DIR)

$(OUT_DIR)/lexer:
	mkdir -p $@

$(OUT_DIR)/parser:
	mkdir -p $@

$(EXEC): $(OBJS)
	$(CXX) $(CXXFLAGS) $(LLVM_CXXFLAGS) -o $(EXEC) $(OBJS) $(LLVM_LDFLAGS)

# Regla genérica para compilar src/.../*.cpp → build/.../*.o
$(OUT_DIR)/%.o: $(SRC_DIR)/%.cpp
	@mkdir -p $(dir $@)
	$(CXX) $(CXXFLAGS) $(LLVM_CXXFLAGS) -c $< -o $@

# Flex y Bison
$(LEX_C): $(LEX_SRC) $(YACC_H) | $(OUT_DIR)/lexer
	$(FLEX) -o $@ $<

$(YACC_C) $(YACC_H): $(YACC_SRC) | $(OUT_DIR)/parser
	$(BISON) -d -o $(YACC_C) $(YACC_SRC)

$(LEX_OBJ): $(LEX_C)
	$(CXX) $(CXXFLAGS) -c $< -o $@

$(YACC_OBJ): $(YACC_C)
	$(CXX) $(CXXFLAGS) -c $< -o $@

$(MAIN_OBJ): $(MAIN_SRC)
	$(CXX) $(CXXFLAGS) -c $< -o $@

# Verificar o crear script.hulk vacío
$(SCRIPT_FILE):
	@if [ ! -f "$(SCRIPT_FILE)" ]; then touch "$(SCRIPT_FILE)"; fi

# === META ===
.PHONY: all compile run clean exepath execute