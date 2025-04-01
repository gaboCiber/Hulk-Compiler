# requirements.md

## Paquetes necesarios para compilar el compilador Hulk en Ubuntu 22.04

### Instalación de dependencias:
Ejecutar el siguiente comando para instalar los paquetes requeridos:
```sh
sudo apt update && sudo apt install -y g++ flex bison make m4
```

### Herramientas opcionales (recomendadas para depuración y análisis):
```sh
sudo apt install -y valgrind gdb clang-format
```

### Verificación de instalación:
Ejecutar los siguientes comandos para comprobar que todas las herramientas están disponibles:
```sh
g++ --version
flex --version
bison --version
make --version
```

### Notas:
- `g++`: Compilador de C++.
- `flex`: Generador de analizadores léxicos.
- `bison`: Generador de analizadores sintácticos.
- `make`: Herramienta de automatización de compilación.
- `m4`: Requerido por Bison en algunas configuraciones.
- `valgrind`: Análisis de memoria y detección de fugas (opcional).
- `gdb`: Depurador de C++ (opcional).
- `clang-format`: Herramienta para formatear código automáticamente (opcional).