#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <string.h>
#include <stdbool.h>



double hulk_log_base(double base, double value) {
    return log(value) / log(base);
}

char* hulk_to_string(void* value, int type) {
    char* buffer = malloc(64);
    switch(type) {
        case 0:  // Float
            snprintf(buffer, 64, "%g", *(float*)value);  // Usamos double para mayor precisión
            break;
        case 1:  // Bool
            strcpy(buffer, *(bool*)value ? "true" : "false");
            break;
        case 2:  // String
            return strdup((char*)value);  // No necesita conversión
        default:
            strcpy(buffer, "unknown");
    }
    return buffer;
}

char* hulk_string_concat(void* a, int type_a, void* b, int type_b) {
    char* str_a = hulk_to_string(a, type_a);
    char* str_b = hulk_to_string(b, type_b);
    
    char* result = malloc(strlen(str_a) + strlen(str_b) + 1);
    strcpy(result, str_a);
    strcat(result, str_b);
    
    free(str_a);
    free(str_b);
    
    return result;
}

double hulk_print_float(double x) {
    printf("%g\n", x);  // 6 decimales
    return x;
}

const char* hulk_print_string(const char* s) {
    printf("%s\n", s);
    return s;
}

int hulk_print_bool(int b) {
    printf(b ? "true\n" : "false\n");
    return b;  // Devuelve el mismo bool
}

double hulk_rand() {
    static int initialized = 0;
    if (!initialized) {
        srand(time(NULL));
        initialized = 1;
    }
    // Generar número entre 0 y RAND_MAX, luego normalizar a [0,1]
    return (double) rand() / ((double)RAND_MAX + 1.0);
}