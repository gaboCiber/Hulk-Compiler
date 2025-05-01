# requirements.md

## Paquetes necesarios para compilar el compilador Hulk en Ubuntu 22.04

### Instalación de dependencias:
Ejecutar el siguiente comando para instalar los paquetes requeridos:
```sh
sudo apt update && sudo apt install -y g++ flex bison make m4 llvm clang libclang-dev llvm-dev
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
llvm-config --version
```