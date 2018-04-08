#include <stdbool.h>
#include <stddef.h>

typedef struct array {
    void **body;
    size_t size;
    int length;
} array_t;

typedef struct matrixi {
    int **body;
    int rows;
    int cols;
} matrixi_t;

typedef struct slice {
    int start;
    int end;
} slice_t;

void print_bool(bool);
void print_matrixi(matrixi_t *, bool);
/**
 * Second argument should be a function that prints out
 * an element of the array; therefore, it should be able
 * to perform the correct cast at some point in its
 * execution (print_array does not handle this and is type-agnostic)
 */
void print_array(array_t *, void(*)(void *));

void init_matrixi(matrixi_t *);
void set_ptrs_matrixi(matrixi_t *, int *);
void set_ptrs_array(array_t *, void *);

void * get_array(array_t *, int);
void set_array(array_t *, int, void *);
void slice_array(array_t *, slice_t *, array_t *);
void set_slice_array(array_t *, slice_t *, array_t *);

int get_matrixi(matrixi_t *, int, int);
void set_matrixi(matrixi_t *, int, int, int);
void slice_matrixi(matrixi_t *, slice_t *, slice_t *, matrixi_t *);
void set_slice_matrixi(matrixi_t *, slice_t *, slice_t *, matrixi_t *);
