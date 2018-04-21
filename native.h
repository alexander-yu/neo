#include <stdbool.h>
#include <stddef.h>

enum mat_type {Int, Float};
enum mat_op {
    Add, Sub, Mult, Div, Mod, Exp,
    Equal, Neq, Less, Leq, Greater, Geq
};

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

/* Pretty-print functions */
void _print_bool(bool);
void _print_int(int);
void _print_float(double);
void _print_string(char *);
void _print_matrix(matrix_t *);
void print_function(void *);
void print_matrix(matrix_t *, bool);
/**
 * Second argument should be a function that prints out
 * an element of the array; therefore, it should be able
 * to perform the correct cast at some point in its
 * execution (print_array does not handle this and is type-agnostic)
 */
void print_array(array_t *, void(*)(void *));

/* Array/matrix memory functions */
void _free_matrix(matrix_t *);
void _free_array(array_t *);
void deep_free_array(array_t *, void(*)(void *));
/* Same as print_array, but to free an element */
void set_ptrs_matrix(matrix_t *, void *);
void set_ptrs_array(array_t *, void *);

/* Array index/slice functions */
void * get_array(array_t *, int);
void set_array(array_t *, int, void *);
void slice_array(array_t *, slice_t *, array_t *);
void set_slice_array(array_t *, slice_t *, array_t *);

/* Matrix index/slice functions */
void * get_matrix(matrix_t *, int, int);
void set_matrix(matrix_t *, int, int, void *);
void slice_matrix(matrix_t *, slice_t *, slice_t *, matrix_t *);
void set_slice_matrix(matrix_t *, slice_t *, slice_t *, matrix_t *);

/* Binary operations */
int iexp(int, int);
double fexp(double, double);
void matmult(matrix_t *, matrix_t *, matrix_t *);
void mat_binop(matrix_t *, enum mat_op, matrix_t *, matrix_t *);

/* Miscellaneous helpers/built-ins */
void init_matrix(matrix_t *);
int length(array_t *);
int rows(matrix_t *);
int cols(matrix_t *);
int to_int(double);
double to_float(int);
