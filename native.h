#include <stddef.h>

typedef struct array {
    void *body;
    size_t size;
    int length;
} array_t;

typedef struct matrixi {
    int **body;
    int rows;
    int cols;
} matrixi_t;

void print_matrixi(matrixi_t *);
/**
 * Second argument should be a function that prints out
 * an element of the array; therefore, it should be able
 * to perform the correct cast at some point in its
 * execution (print_array does not handle this and is type-independent)
 */
void print_array(array_t *, void(*)(void *));

void init_matrixi(matrixi_t *);
void set_ptrs_matrixi(matrixi_t *, int *);

void *array_get(array_t *, int i);