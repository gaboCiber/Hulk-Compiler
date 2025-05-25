#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <string.h>

// En runtime.c

char* hulk_string_concat(const char* s1, const char* s2) {
    size_t len1 = strlen(s1);
    size_t len2 = strlen(s2);
    char* result = malloc(len1 + len2 + 1);
    strcpy(result, s1);
    strcat(result, s2);
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
    return (double)rand() / RAND_MAX;
}