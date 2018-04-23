#include <stdbool.h>
#include <stddef.h>

const char *NULL_VALUE_ERR = "Runtime error: attempted to access null value";

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
    bool has_ptrs;
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
array_t *malloc_array(int, size_t, bool);
matrix_t *malloc_matrix(int, int, enum mat_type);

/* Array index/slice functions */
void * get_array(array_t *, int);
void set_array(array_t *, int, void *);
void slice_array(array_t *, slice_t *, array_t *);
void set_slice_array(array_t *, slice_t *, array_t *);
array_t * insert_array(array_t *, int, void *);
array_t * _delete_array(array_t *, int);
array_t * append_array(array_t *, void *);

/* Matrix index/slice functions */
void * get_matrix(matrix_t *, int, int);
void set_matrix(matrix_t *, int, int, void *);
void slice_matrix(matrix_t *, slice_t *, slice_t *, matrix_t *);
void set_slice_matrix(matrix_t *, slice_t *, slice_t *, matrix_t *);
matrix_t * _insert_matrix(matrix_t *, int, matrix_t *);
matrix_t * _delete_matrix(matrix_t *, int);
matrix_t * _append_matrix(matrix_t *, matrix_t *);

/* Binary operations */
int iexp(int, int);
double fexp(double, double);
void matmult(matrix_t *, matrix_t *, matrix_t *);
void mat_binop(matrix_t *, enum mat_op, matrix_t *, matrix_t *);

/* Miscellaneous helpers/built-ins */
void init_array(array_t *, void *);
void init_matrix(matrix_t *);
int length(array_t *);
int rows(matrix_t *);
int cols(matrix_t *);
int _float_to_int(double);
double _int_to_float(int);
matrix_t *_flip_matrix_type(matrix_t *);
void die();
void check(const bool, const char *);
