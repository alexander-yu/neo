#include <stdbool.h>
#include <stddef.h>

enum mat_type {Int, Float};
enum mat_op {
    Add, Sub, Mult, Div, Mod, Exp,
    Equal, Neq, Less, Leq, Greater, Geq
};

union mat_body {
    double** fbody;
    int** ibody;
};

typedef struct array {
    void** body;
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
void _print_bool(const bool);
void _print_int(const int);
void _print_float(const double);
void _print_string(const char*);
void _print_matrix(const matrix_t*);
void _print_flat_matrix(const matrix_t*);
void _print_function(const void*);
/**
 * Second argument should be a function that prints out
 * an element of the array; therefore, it should be able
 * to perform the correct cast at some point in its
 * execution (print_array does not handle this and is type-agnostic)
 */
void _print_array(const array_t*, void(*const)(const void*));

/* Array/matrix memory functions */
void _free_matrix(matrix_t*);
void _free_array(array_t*);
/* Same as print_array for the second argument, but to free an element */
void _deep_free_array(array_t*, void(*const)(void*));
array_t* _malloc_array(const int, const size_t, const bool);
matrix_t* _malloc_matrix(const int, const int, const enum mat_type);

/* Array index/slice functions */
void* _get_array(const array_t*, const int);
void _set_array(array_t*, const int, const void*);
array_t* _slice_array(const array_t*, const slice_t*);
void _set_slice_array(array_t*, const slice_t*, const array_t*);
array_t* _insert_array(const array_t*, const int, const void*);
array_t* _delete_array(const array_t*, const int);
array_t* _append_array(const array_t*, const void*);

/* Matrix index/slice functions */
void* _get_matrix(const matrix_t*, const int, const int);
void _set_matrix(matrix_t*, const int, const int, const void*);
matrix_t* _slice_matrix(const matrix_t*, const slice_t*, const slice_t*);
void _set_slice_matrix(matrix_t*, const slice_t*, const slice_t*, const matrix_t*);
matrix_t* _insert_matrix(const matrix_t*, const int, const matrix_t*);
matrix_t* _delete_matrix(const matrix_t*, const int);
matrix_t* _append_matrix(const matrix_t*, const matrix_t*);

/* Binary operations */
int _iexp(const int, const int);
double _fexp(const double, const double);
matrix_t* _matmult(const matrix_t*, const matrix_t*);
matrix_t* _mat_binop(const matrix_t*, const enum mat_op, const matrix_t*);

/* Miscellaneous helpers/built-ins */
void _init_array(array_t*, const void*);
void _init_matrix(matrix_t*);
int _length(const array_t*);
int _rows(const matrix_t*);
int _cols(const matrix_t*);
int _float_to_int(const double);
double _int_to_float(const int);
matrix_t* _flip_matrix_type(const matrix_t*);
void _die(const char*);
void _check(const bool, const char*);
