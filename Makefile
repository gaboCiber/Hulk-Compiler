# === VARIABLES GLOBALES ===
CXX = g++
FLEX = flex
BISON = bison

CXXFLAGS = -Wall -std=c++17
OUT_DIR = tmp
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

OBJS = $(MAIN_OBJ) $(LEX_OBJ) $(YACC_OBJ)
EXEC = $(OUT_DIR)/hulk-compiler
SCRIPT_FILE = $(OUT_DIR)/script.hulk

# === TARGETS ===

all: build

build: clean $(OUT_DIR) $(EXEC) $(SCRIPT_FILE)
	@# Mover tmp -> build para evitar conflictos con el nombre del target
	@mv $(OUT_DIR) build
	@echo "✅ Build completo. Ejecutable en $(EXEC)"

run: build
	@echo "🚀 Compilador listo. Agrega ejecución en esta receta si lo deseas."

clean:
	rm -rf build
	@echo "🧹 Proyecto limpiado."

# === REGLAS DE COMPILACIÓN ===

$(OUT_DIR):
	mkdir -p $(OUT_DIR)

$(EXEC): $(OBJS)
	$(CXX) $(CXXFLAGS) -o $(EXEC) $(OBJS)

# Generación de objetos
$(MAIN_OBJ): $(MAIN_SRC)
	$(CXX) $(CXXFLAGS) -c $< -o $@

$(LEX_OBJ): $(LEX_C)
	$(CXX) $(CXXFLAGS) -c $< -o $@

$(YACC_OBJ): $(YACC_C)
	$(CXX) $(CXXFLAGS) -c $< -o $@

# Flex y Bison
$(LEX_C): $(LEX_SRC)
	$(FLEX) -o $@ $<

$(YACC_C) $(YACC_H): $(YACC_SRC)
	$(BISON) -d -o $(YACC_C) $(YACC_SRC)

# Verificar o crear script.hulk
$(SCRIPT_FILE):
	@if [ ! -f "$(SCRIPT_FILE)" ]; then touch "$(SCRIPT_FILE)"; fi

# === META ===
.PHONY: all build run clean
