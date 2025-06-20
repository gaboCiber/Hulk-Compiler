# 🧠 Hulk Compiler

Hulk es un lenguaje de programación educativo orientado a objetos, diseñado para ser accesible y simple. Este proyecto implementa un compilador completo para Hulk, que toma programas fuente y los convierte en ejecutables nativos mediante LLVM IR.


## 👨‍💻 Autores

- Gabriel Andrés Pla Lasa - C311
- Carlos Daniel Largacha Leal - C312


## 📦 Dependencias

Este compilador ha sido desarrollado y probado en **Ubuntu 22.04**.

### Instalación de dependencias básicas

```bash
sudo apt update && sudo apt install -y g++ flex bison make m4 llvm clang libclang-dev llvm-dev
````

### Herramientas opcionales (útiles para depuración)

```bash
sudo apt install -y valgrind gdb clang-format
```

### Verificación de herramientas instaladas

```bash
g++ --version
flex --version
bison --version
make --version
llvm-config --version
```


## 📁 Estructura del Proyecto

```
├── src/
│   ├── ast/              # Definiciones de nodos del AST
│   ├── codegen/          # Generación de LLVM IR
│   ├── lexer/            # Archivo .l para Flex
│   ├── parser/           # Archivo .y para Bison
│   ├── semantic/         # Chequeo semántico e inferencia de tipos
│   └── main.cpp          # Punto de entrada del compilador
│
├── hulk/                 # Archivos generados y binarios
│
├── script.hulk           # Archivo de entrada Hulk (por defecto)
├── Makefile              # Script de construcción automatizado
└── README.md             # Este archivo
```


## 🛠️ Uso del compilador

Asegúrate de tener un archivo `script.hulk` con código fuente válido. Luego puedes compilar y ejecutar usando `make` con las siguientes reglas:

### 🔧 Compilación

```bash
make build
```

* Genera el ejecutable `hulk/hulk-compiler`.

### 🧱 Compila el programa Hulk por defecto (`script.hulk`)

```bash
make compile
```
* Genera `output.ll` (LLVM IR), `output.s` (ensamblador) y `output` (binario nativo).
* No lo ejecuta automáticamente. Útil si solo deseas compilar.


### 🚀 Compila y ejecuta el programa Hulk por defecto (`script.hulk`)

```bash
make execute
```

* Compila el script.hulk a traves de `make compile`
* Ejecuta el binario tras compilar.

### 📁 Compilar y ejecutar un archivo específico

```bash
make exepath FILE=examples/mi_programa.hulk
```

* Compila y ejecuta el archivo indicado por la variable `FILE`.

### ▶️ Ejecutar directamente el último binario generado

```bash
make run
```

* Ejecuta sin compilar el ultimo binario generado por `make compile`, `make execute` o `make exepath`


### 🧹 Limpiar los archivos generados

```bash
make clean
```

* Elimina el directorio `hulk/` con los binarios y archivos intermedios.


## 📄 Notas

* El código LLVM IR se guarda automáticamente en `hulk/output.ll`.
* El runtime necesario está definido en `src/codegen/runtime.c`.
* El backend usa `llc` y `clang` para producir ejecutables nativos.


## 🎓 Referencias

* Documentación oficial de Hulk: [https://matcom.in/hulk/guide/intro.html](https://matcom.in/hulk/guide/intro.html)

¡Gracias por revisar este proyecto! 💻
