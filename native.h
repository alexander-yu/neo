#include <stdbool.h>
#include <stddef.h>

const char* NULL_VALUE_ERR = "Null value error: attempted to read null value";
const char* DIV_ZERO_ERR = "Zero division error: attmpted to perform division or modulo by 0";
const char* EXP_ZERO_ERR = "Zero division error: attempted to raise 0 to a negative power";
const char* EXP_NEG_ERR = "Arithmetic error: attempted to raise negative number to a non-integer power";
const char* MAT_IDX_ERR = "Matrix index error: attempted to access an out of bounds index for a matrix";
const char* ARR_IDX_ERR = "Array index error: attempted to access an out of bounds index for an array";
const char* SLICE_ERR = "Slice error: attempted to perform slice that would return zero elements \
                        (reversed or equal bounds)";
const char* ARR_LEN_ERR = "Length error: attempted to create array of nonpositive length";
const char* MAT_ROWS_ERR = "Dimension error: attempted to create matrix with nonpositive rows";
const char* MAT_COLS_ERR = "Dimension error: attempted to create matrix with nonpositive columns";
const char* ROW_DIM_ERR = "Dimension error: attempted to insert/append row with invalid dimensions";
const char* MAT_MULT_ERR = "Dimension error: attempted to perform matrix multiplication with \
                           incompatible dimensions";
const char* MAT_BINOP_ERR = "Dimension error: attempted to perform matrix binary operations with \
                            incompatible/non-broadcastable dimensions";

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
void _print_bool(bool);
void _print_int(int);
void _print_float(double);
void _print_string(char*);
void _print_matrix(matrix_t*);
void print_function(void*);
void print_matrix(matrix_t*, bool);
/**
 * Second argument should be a function that prints out
 * an element of the array; therefore, it should be able
 * to perform the correct cast at some point in its
 * execution (print_array does not handle this and is type-agnostic)
 */
void print_array(array_t*, void(*)(void*));

/* Array/matrix memory functions */
void _free_matrix(matrix_t*);
void _free_array(array_t*);
void deep_free_array(array_t*, void(*)(void*));
/* Same as print_array, but to free an element */
void set_ptrs_matrix(matrix_t*, void*);
void set_ptrs_array(array_t*, void*);
array_t* malloc_array(int, size_t, bool);
matrix_t* malloc_matrix(int, int, enum mat_type);

/* Array index/slice functions */
void* get_array(array_t*, int);
void set_array(array_t*, int, void*);
array_t* slice_array(array_t*, slice_t*);
void set_slice_array(array_t*, slice_t*, array_t*);
array_t* insert_array(array_t*, int, void*);
array_t* _delete_array(array_t*, int);
array_t* append_array(array_t*, void*);

/* Matrix index/slice functions */
void* get_matrix(matrix_t*, int, int);
void set_matrix(matrix_t*, int, int, void*);
matrix_t* slice_matrix(matrix_t*, slice_t*, slice_t*);
void set_slice_matrix(matrix_t*, slice_t*, slice_t*, matrix_t*);
matrix_t* _insert_matrix(matrix_t*, int, matrix_t*);
matrix_t* _delete_matrix(matrix_t*, int);
matrix_t* _append_matrix(matrix_t*, matrix_t*);

/* Binary operations */
int iexp(int, int);
double fexp(double, double);
matrix_t* matmult(matrix_t*, matrix_t*);
matrix_t* mat_binop(matrix_t*, enum mat_op, matrix_t*);

/* Miscellaneous helpers/built-ins */
void init_array(array_t*, void*);
void init_matrix(matrix_t*);
int length(array_t*);
int rows(matrix_t*);
int cols(matrix_t*);
int _float_to_int(double);
double _int_to_float(int);
matrix_t* _flip_matrix_type(matrix_t*);
void die(const char*);
void check(const bool, const char*);
void check_arr_index(const array_t*, const int);
void check_arr_slice(const array_t*, const slice_t*);
void check_mat_index(const matrix_t*, const int, const int);
void check_mat_slice(const matrix_t*, const slice_t*, const slice_t*);
void check_row_dims(const matrix_t*, const matrix_t*);
