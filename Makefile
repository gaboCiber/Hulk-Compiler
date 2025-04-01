CXX = g++
LEX = flex
YACC = bison
CXXFLAGS = -Wall -std=c++17

LEX_SRC = lexer.l
YACC_SRC = parser.y
LEX_OUT = lex.yy.c
YACC_OUT = parser.tab.c parser.tab.h
SRC = main.cpp
OBJ = $(SRC:.cpp=.o) $(LEX_OUT:.c=.o) $(YACC_OUT:.c=.o)

BUILD_DIR = build

build: $(OBJ)
	mkdir -p $(BUILD_DIR)
	$(CXX) $(CXXFLAGS) -o $(BUILD_DIR)/hulk_compiler $(OBJ)

run: build
	@echo "Ejecutar compilador aquí más adelante"

$(LEX_OUT): $(LEX_SRC)
	$(LEX) -o $@ $<

$(YACC_OUT): $(YACC_SRC)
	$(YACC) -d $<

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

%.o: %.c
	$(CXX) $(CXXFLAGS) -c $< -o $@

clean:
	rm -rf $(BUILD_DIR) *.o $(LEX_OUT) $(YACC_OUT)

.PHONY: build run clean
