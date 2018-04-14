#include <stdbool.h>
#include <stddef.h>

enum mat_type {Int, Float};

union mat_body {
    double **fbody;
    int **ibody;
};

typedef struct array {
    void **body;
    int length;
    size_t size;
} array_t;

typedef struct matrix {
    union mat_body body;
    int rows;
    int cols;
    enum mat_type type;
} matrix_t;

typedef struct slice {
    int start;
    int end;
} slice_t;

void print_bool(bool);
void print_matrix(matrix_t *, bool);
void free_matrix(matrix_t *);
/**
 * Second argument should be a function that prints out
 * an element of the array; therefore, it should be able
 * to perform the correct cast at some point in its
 * execution (print_array does not handle this and is type-agnostic)
 */
void print_array(array_t *, void(*)(void *));
/* Same as above, but to free an element */
void free_array(array_t *, void(*)(void *));

void init_matrix(matrix_t *);
void set_ptrs_matrix(matrix_t *, void *);
void set_ptrs_array(array_t *, void *);

void * get_array(array_t *, int);
void set_array(array_t *, int, void *);
void slice_array(array_t *, slice_t *, array_t *);
void set_slice_array(array_t *, slice_t *, array_t *);

void * get_matrix(matrix_t *, int, int);
void set_matrix(matrix_t *, int, int, void *);
void slice_matrix(matrix_t *, slice_t *, slice_t *, matrix_t *);
void set_slice_matrix(matrix_t *, slice_t *, slice_t *, matrix_t *);
