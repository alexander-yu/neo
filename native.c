#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <math.h>
#include "native.h"

/* Some versions of stdlib.h may have max/min macros,
 * like Visual C++'s version. If they exist, undefine them
 * for consistency. */
#ifdef max
    #undef max
#endif

#ifdef min
    #undef min
#endif

static const char* NULL_VALUE_ERR = "Null value error: attempted to read null value";
static const char* DIV_ZERO_ERR = "Zero division error: attmpted to perform division or modulo by 0";
static const char* EXP_ZERO_ERR = "Zero division error: attempted to raise 0 to a negative power";
static const char* EXP_NEG_ERR = "Arithmetic error: attempted to raise negative number to a non-integer power";
static const char* MAT_IDX_ERR = "Matrix index error: attempted to access an out of bounds index for a matrix";
static const char* ARR_IDX_ERR = "Array index error: attempted to access an out of bounds index for an array";
static const char* SLICE_ERR = "Slice error: attempted to perform slice that would return zero elements \
                               (reversed or equal bounds)";
static const char* ARR_LEN_ERR = "Length error: attempted to create array of nonpositive length";
static const char* MAT_ROWS_ERR = "Dimension error: attempted to create matrix with nonpositive rows";
static const char* MAT_COLS_ERR = "Dimension error: attempted to create matrix with nonpositive columns";
static const char* ROW_DIM_ERR = "Dimension error: attempted to insert/append row with invalid dimensions";
static const char* MAT_MULT_ERR = "Dimension error: attempted to perform matrix multiplication with \
                                  incompatible dimensions";
static const char* MAT_BINOP_ERR = "Dimension error: attempted to perform matrix binary operations with \
                                   incompatible/non-broadcastable dimensions";

/* Min/max helpers */
static int max(const int a, const int b) {
    return (a > b) ? a : b;
}

static int min(const int a, const int b) {
    return (a < b) ? a : b;
}

/* Runtime error-checking functions */
void die(const char* err) {
    fprintf(stderr, "%s\n", err);
    exit(EXIT_FAILURE);
}

void check(const bool cond, const char* err) {
    if (!cond) {
        die(err);
    }
}

static void check_arr_index(const array_t* arr, const int i) {
    check(i >= 0 && i < arr->length, ARR_IDX_ERR);
}

static void check_arr_slice(const array_t* arr, const slice_t* slice) {
    int start = slice->start;
    int end = slice->end;
    check(start < end, SLICE_ERR);
    check_arr_index(arr, start);
    check(end <= arr->length, ARR_IDX_ERR);
}

static void check_mat_index(const matrix_t* mat, const int i, const int j) {
    check(i >= 0 && i < mat->rows, MAT_IDX_ERR);
    check(j >= 0 && j < mat->cols, MAT_IDX_ERR);
}

static void check_mat_slice(const matrix_t* mat, const slice_t* row_slice, const slice_t* col_slice) {
    int start_i = row_slice->start;
    int end_i = row_slice->end;
    int start_j = col_slice->start;
    int end_j = col_slice->end;
    check(start_i < end_i, SLICE_ERR);
    check(start_j < end_j, SLICE_ERR);
    check_mat_index(mat, start_i, start_j);
    check(end_i <= mat->rows, MAT_IDX_ERR);
    check(end_j <= mat->cols, MAT_IDX_ERR);
}

static void check_row_dims(const matrix_t* mat, const matrix_t* row) {
    check(row->rows == 1, ROW_DIM_ERR);
    check(row->cols == mat->cols, ROW_DIM_ERR);
}

static void check_mat_binop_dims(const matrix_t* a, const matrix_t* b) {
    int rows_a = a->rows;
    int rows_b = b->rows;
    int cols_a = a->cols;
    int cols_b = b->cols;
    check(rows_a == rows_b || min(rows_a, rows_b) == 1, MAT_BINOP_ERR);
    check(cols_a == cols_b || min(cols_a, cols_b) == 1, MAT_BINOP_ERR);
}

/* Pretty-printing functions */
void _print_bool(const bool b) {
    if (b) {
        printf("True");
    } else {
        printf("False");
    }
}

void _print_int(const int i) {
    printf("%d", i);
}

void _print_float(const double d) {
    printf("%g", d);
}

void _print_string(const char* s) {
    printf("%s", s);
}

void _print_matrix(const matrix_t* m) {
    print_matrix(m, false);
}

void print_function(const void* p) {
    printf("function at %p", p);
}

void print_matrix(const matrix_t* mat, const bool flat) {
    printf("[");
    int rows = mat->rows;
    int cols = mat->cols;
    union mat_body body = mat->body;
    for (int i = 0; i < rows; ++i) {
        if (i == 0) {
            printf("[");
        } else {
            printf(" [");
        }

        for (int j = 0; j < cols; ++j) {
            if (j == cols - 1) {
                switch (mat->type) {
                    case Int: printf("%d", body.ibody[i][j]); break;
                    case Float: printf("%g", body.fbody[i][j]); break;
                }
            } else {
                switch (mat->type) {
                    case Int: printf("%d, ", body.ibody[i][j]); break;
                    case Float: printf("%g, ", body.fbody[i][j]); break;
            }
        }
        }

        if (i == rows - 1) {
            printf("]");
        } else {
            printf("],");
            if (!flat) {
                printf("\n");
            }
        }
    }
    printf("]");
}

void print_array(const array_t* arr, void(*const print_element)(const void*)) {
    int length = arr->length;
    printf("{|");
    for (int i = 0; i < length; i++) {
        print_element(get_array(arr, i));
        if (i != length - 1) {
            printf(", ");
        }
    }
    printf("|}");
}

/* Array/matrix memory functions */
void _free_matrix(matrix_t* mat) {
    union mat_body body = mat->body;
    enum mat_type type = mat->type;

    switch (type) {
        case Int: free(body.ibody[0]); free(body.ibody); break;
        case Float: free(body.fbody[0]); free(body.fbody); break;
    }

    free(mat);
}

void _free_array(array_t* arr) {
    void** body = arr->body;
    free(body[0]);
    free(body);
    free(arr);
}

void deep_free_array(array_t* arr, void(*const free_element)(void*)) {
    int length = arr->length;

    for (int i = 0; i < length; i++) {
        free_element(get_array(arr, i));
    }

    _free_array(arr);
}

static void set_ptrs_matrix(matrix_t* mat, const void* body) {
    int rows = mat->rows;
    int cols = mat->cols;
    for (int i = 0; i < rows; i++) {
        switch (mat->type) {
            case Int:
                mat->body.ibody[i] = (int*)((char*)body + i * cols * sizeof(int));
                break;
            case Float:
                mat->body.fbody[i] = (double*)((char*)body + (i * cols * sizeof(double)));
                break;
        }
    }
}

static void set_ptrs_array(array_t* arr, const void* body) {
    int length = arr->length;
    size_t size = arr->size;
    for (int i = 0; i < length; i++) {
        arr->body[i] = (void*)((char*)body + i * size);
    }
}

array_t* malloc_array(const int length, const size_t size, const bool has_ptrs) {
    check(length > 0, ARR_LEN_ERR);
    array_t* arr = malloc(sizeof(array_t));
    arr->body = malloc(length * sizeof(void*));
    arr->length = length;
    arr->size = size;
    arr->has_ptrs = has_ptrs;

    void* body = malloc(length * sizeof(void*));
    set_ptrs_array(arr, body);

    return arr;
}

matrix_t* malloc_matrix(const int rows, const int cols, const enum mat_type type) {
    check(rows > 0, MAT_ROWS_ERR);
    check(cols > 0, MAT_COLS_ERR);
    matrix_t* mat = malloc(sizeof(matrix_t));
    mat->rows = rows;
    mat->cols = cols;
    mat->type = type;

    union mat_body body;
    void *raw_body;

    switch (type) {
        case Int:
            body.ibody = malloc(rows * sizeof(int*));
            raw_body = malloc(rows * cols * sizeof(int));
            break;
        case Float:
            body.fbody = malloc(rows * sizeof(double*));
            raw_body = malloc(rows * cols * sizeof(double));
            break;
    }

    mat->body = body;
    set_ptrs_matrix(mat, raw_body);
    return mat;
}

/* Array index/slice functions */
void* get_array(const array_t* arr, const int i) {
    check_arr_index(arr, i);
    if (arr->has_ptrs) {
        check(*(void**)arr->body[i] != NULL, NULL_VALUE_ERR);
    }
    return arr->body[i];
}

void set_array(array_t* arr, const int i, const void* data) {
    check_arr_index(arr, i);
    /* Perform a shallow copy */
    memcpy(arr->body[i], data, arr->size);
}

array_t* slice_array(const array_t* arr, const slice_t* slice) {
    check_arr_slice(arr, slice);
    int start_i = slice->start;
    int end_i = slice->end;
    int length = end_i - start_i;
    array_t* res = malloc_array(length, arr->size, arr->has_ptrs);

    for (int i = 0; i < length; i++) {
        set_array(res, i, get_array(arr, i + start_i));
    }

    return res;
}

void set_slice_array(array_t* arr, const slice_t* slice, const array_t* data) {
    check_arr_slice(arr, slice);
    int start_i = slice->start;
    int end_i = slice->end;
    for (int i = start_i; i < end_i; i++) {
        set_array(arr, i, data->body[i - start_i]);
    }
}

array_t* insert_array(const array_t* arr, const int pos_i, const void* data) {
    check_arr_index(arr, pos_i);
    int length = arr->length + 1;
    array_t* res = malloc_array(length, arr->size, arr->has_ptrs);

    for (int i = 0; i < length; i++) {
        if (i < pos_i) {
            set_array(res, i, arr->body[i]);
        } else if (i == pos_i) {
            set_array(res, i, data);
        } else {
            set_array(res, i, arr->body[i - 1]);
        }
    }

    return res;
}

array_t* _delete_array(const array_t* arr, const int pos_i) {
    check_arr_index(arr, pos_i);
    int length = arr->length - 1;
    array_t* res = malloc_array(length, arr->size, arr->has_ptrs);

    for (int i = 0; i < length; i++) {
        if (i < pos_i) {
            set_array(res, i, arr->body[i]);
        } else {
            set_array(res, i, arr->body[i + 1]);
        }
    }

    return res;
}

array_t* append_array(const array_t* arr, const void* data) {
    int length = arr->length + 1;
    array_t* res = malloc_array(length, arr->size, arr->has_ptrs);

    for (int i = 0; i < length; i++) {
        if (i == length - 1) {
            set_array(res, i, data);
        } else {
            set_array(res, i, arr->body[i]);
        }
    }

    return res;
}

/* Matrix index/slice functions */
void* get_matrix(const matrix_t* mat, const int i, const int j) {
    check_mat_index(mat, i, j);
    switch (mat->type) {
        case Int: return (void*)&mat->body.ibody[i][j];
        case Float: return (void*)&mat->body.fbody[i][j];
    }
    /**
     * This is for placating GCC warnings, since get_matrix is
     * a non-void function, and control flow thus can't reach the
     * end of the function
     */
    perror("get_matrix got invalid mat_type; this should never happen");
    exit(EXIT_FAILURE);
}

void set_matrix(matrix_t* mat, const int i, const int j, const void* data) {
    check_mat_index(mat, i, j);
    switch (mat->type) {
        case Int: mat->body.ibody[i][j] = *(int*)data; break;
        case Float: mat->body.fbody[i][j] = *(double*)data; break;
    }
}

matrix_t* slice_matrix(const matrix_t* mat, const slice_t* row_slice,
                       const slice_t* col_slice) {
    check_mat_slice(mat, row_slice, col_slice);
    int start_i = row_slice->start;
    int end_i = row_slice->end;
    int start_j = col_slice->start;
    int end_j = col_slice->end;

    int rows = end_i - start_i;
    int cols = end_j - start_j;
    enum mat_type type = mat->type;
    matrix_t* res = malloc_matrix(rows, cols, type);

    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols; j++) {
            switch (mat->type) {
                case Int:
                    res->body.ibody[i][j] = mat->body.ibody[i + start_i][j + start_j];
                    break;
                case Float:
                    res->body.fbody[i][j] = mat->body.fbody[i + start_i][j + start_j];
                    break;
            }
        }
    }

    return res;
}

void set_slice_matrix(matrix_t* mat, const slice_t* row_slice,
                      const slice_t* col_slice, const matrix_t* data) {
    check_mat_slice(mat, row_slice, col_slice);
    int start_i = row_slice->start;
    int end_i = row_slice->end;
    int start_j = col_slice->start;
    int end_j = col_slice->end;
    for (int i = start_i; i < end_i; i++) {
        for (int j = start_j; j < end_j; j++) {
            switch (mat->type) {
                case Int:
                    mat->body.ibody[i][j] = data->body.ibody[i - start_i][j - start_j];
                    break;
                case Float:
                    mat->body.fbody[i][j] = data->body.fbody[i - start_i][j - start_j];
                    break;
            }
        }
    }
}

matrix_t* _insert_matrix(const matrix_t* mat, const int row_i, const matrix_t* row) {
    check_mat_index(mat, row_i, 0);
    check_row_dims(mat, row);
    int rows = mat->rows + 1;
    int cols = mat->cols;
    enum mat_type type = mat->type;
    matrix_t* res = malloc_matrix(rows, cols, type);

    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols; j++) {
            if (i < row_i) {
                switch (type) {
                    case Int: res->body.ibody[i][j] = mat->body.ibody[i][j]; break;
                    case Float: res->body.fbody[i][j] = mat->body.fbody[i][j]; break;
                }
            } else if (i == row_i) {
                switch (type) {
                    case Int: res->body.ibody[i][j] = row->body.ibody[0][j]; break;
                    case Float: res->body.fbody[i][j] = row->body.fbody[0][j]; break;
                }
            } else {
                switch (type) {
                    case Int: res->body.ibody[i][j] = mat->body.ibody[i - 1][j]; break;
                    case Float: res->body.fbody[i][j] = mat->body.fbody[i - 1][j]; break;
                }
            }
        }
    }

    return res;
}

matrix_t* _delete_matrix(const matrix_t* mat, const int row_i) {
    check_mat_index(mat, row_i, 0);
    int rows = mat->rows - 1;
    int cols = mat->cols;
    enum mat_type type = mat->type;
    matrix_t* res = malloc_matrix(rows, cols, type);

    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols; j++) {
            if (i < row_i) {
                switch (type) {
                    case Int: res->body.ibody[i][j] = mat->body.ibody[i][j]; break;
                    case Float: res->body.fbody[i][j] = mat->body.fbody[i][j]; break;
                }
            } else {
                switch (type) {
                    case Int: res->body.ibody[i][j] = mat->body.ibody[i + 1][j]; break;
                    case Float: res->body.fbody[i][j] = mat->body.fbody[i + 1][j]; break;
                }
            }
        }
    }

    return res;
}

matrix_t* _append_matrix(const matrix_t* mat, const matrix_t* row) {
    check_row_dims(mat, row);
    int rows = mat->rows + 1;
    int cols = mat->cols;
    enum mat_type type = mat->type;
    matrix_t* res = malloc_matrix(rows, cols, type);

    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols; j++) {
            if (i == rows - 1) {
                switch (type) {
                    case Int: res->body.ibody[i][j] = row->body.ibody[0][j]; break;
                    case Float: res->body.fbody[i][j] = row->body.fbody[0][j]; break;
                }
            } else {
                switch (type) {
                    case Int: res->body.ibody[i][j] = mat->body.ibody[i][j]; break;
                    case Float: res->body.fbody[i][j] = mat->body.fbody[i][j]; break;
                }
            }
        }
    }

    return res;
}

/* Binary operations */
int iexp(const int a, const int b){
    return (int) pow((double) a, (double) b);
}

double fexp(const double a, const double b){
    return pow(a, b);
}

matrix_t* matmult(const matrix_t* a, const matrix_t* b){
    check(a->cols == b->rows, MAT_MULT_ERR);
    int rows = a->rows;
    int cols = b->cols;
    enum mat_type type = a->type;
    matrix_t* res = malloc_matrix(rows, cols, type);

    for (int i = 0; i < rows; i++){
        for (int j = 0; j < cols; j++){
            switch (res->type) {
                case Int: res->body.ibody[i][j] = 0; break;
                case Float: res->body.fbody[i][j] = 0.; break;
            }
            for (int k = 0; k < a->cols; k++){
                switch (res->type) {
                    case Int:
                        res->body.ibody[i][j] += a->body.ibody[i][k] * b->body.ibody[k][j];
                        break;
                    case Float:
                        res->body.fbody[i][j] += a->body.fbody[i][k] * b->body.fbody[k][j];
                        break;
                }
            }
        }
    }

    return res;
}

matrix_t* mat_binop(const matrix_t* a, const enum mat_op op, const matrix_t* b) {
    check_mat_binop_dims(a, b);
    int rows = max(a->rows, b->rows);
    int cols = max(a->cols, b->cols);
    enum mat_type type = a->type;
    matrix_t* res = malloc_matrix(rows, cols, type);

    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols; j++) {
            int a_i = min(a->rows - 1, i);
            int a_j = min(a->cols - 1, j);
            int b_i = min(b->rows - 1, i);
            int b_j = min(b->cols - 1, j);
            switch (res->type) {
                case Int: {
                    int** r_body = res->body.ibody;
                    int a_ij = a->body.ibody[a_i][a_j];
                    int b_ij = b->body.ibody[b_i][b_j];
                    switch (op) {
                        case Add: r_body[i][j] = a_ij + b_ij; break;
                        case Sub: r_body[i][j] = a_ij - b_ij; break;
                        case Mult: r_body[i][j] = a_ij * b_ij; break;
                        case Div:
                            check(b_ij != 0, DIV_ZERO_ERR);
                            r_body[i][j] = a_ij / b_ij;
                            break;
                        case Mod:
                            check(b_ij != 0, DIV_ZERO_ERR);
                            r_body[i][j] = a_ij % b_ij;
                            break;
                        case Exp:
                            check(a_ij != 0 || b_ij >= 0, EXP_ZERO_ERR);
                            /* No need to perform check for negative base
                             * raised to non-integer exponent; the exponents
                             * here are inherently integers already */
                            r_body[i][j] = iexp(a_ij, b_ij);
                            break;
                        case Equal: r_body[i][j] = a_ij == b_ij; break;
                        case Neq: r_body[i][j] = a_ij != b_ij; break;
                        case Less: r_body[i][j] = a_ij < b_ij; break;
                        case Leq: r_body[i][j] = a_ij <= b_ij; break;
                        case Greater: r_body[i][j] = a_ij > b_ij; break;
                        case Geq: r_body[i][j] = a_ij >= b_ij; break;
                    }
                    break;
                }
                case Float: {
                    double** r_body = res->body.fbody;
                    double a_ij = a->body.fbody[a_i][a_j];
                    double b_ij = b->body.fbody[b_i][b_j];
                    switch (op) {
                        case Add: r_body[i][j] = a_ij + b_ij; break;
                        case Sub: r_body[i][j] = a_ij - b_ij; break;
                        case Mult: r_body[i][j] = a_ij * b_ij; break;
                        case Div:
                            check(b_ij != 0., DIV_ZERO_ERR);
                            r_body[i][j] = a_ij / b_ij;
                            break;
                        case Mod:
                            check(b_ij != 0., DIV_ZERO_ERR);
                            r_body[i][j] = fmod(a_ij, b_ij);
                            break;
                        case Exp:
                            check(a_ij != 0. || b_ij >= 0, EXP_ZERO_ERR);
                            check(a_ij >= 0. || floor(b_ij) == b_ij, EXP_NEG_ERR);
                            r_body[i][j] = fexp(a_ij, b_ij);
                            break;
                        case Equal: r_body[i][j] = (a_ij == b_ij) ? 1. : 0.; break;
                        case Neq: r_body[i][j] = (a_ij != b_ij) ? 1. : 0.; break;
                        case Less: r_body[i][j] = (a_ij < b_ij) ? 1. : 0.; break;
                        case Leq: r_body[i][j] = (a_ij <= b_ij) ? 1. : 0.; break;
                        case Greater: r_body[i][j] = (a_ij > b_ij) ? 1. : 0.; break;
                        case Geq: r_body[i][j] = (a_ij >= b_ij) ? 1. : 0.; break;
                    }
                    break;
                }
            }
        }
    }

    return res;
}

/* Miscellaneous helpers/built-ins */
void init_array(array_t* arr, const void* data) {
    int length = arr->length;
    for (int i = 0; i < length; i++) {
        set_array(arr, i, data);
    }
}

void init_matrix(matrix_t* mat) {
    int rows = mat->rows;
    int cols = mat->cols;
    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols; j++) {
            switch (mat->type) {
                case Int: mat->body.ibody[i][j] = 0; break;
                case Float: mat->body.fbody[i][j] = 0.0; break;
            }
        }
    }
}

int length(const array_t* arr) {
    return arr->length;
}

int rows(const matrix_t* mat) {
    return mat->rows;
}

int cols(const matrix_t* mat) {
    return mat->cols;
}

int _float_to_int(const double d) {
    return (int) d;
}

double _int_to_float(const int i) {
    return (double) i;
}

matrix_t* _flip_matrix_type(const matrix_t* mat) {
    int rows = mat->rows;
    int cols = mat->cols;
    /* Flip the type */
    enum mat_type type = 1 - mat->type;
    matrix_t* res = malloc_matrix(rows, cols, type);

    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols; j++) {
            switch (type) {
                case Int:
                    res->body.ibody[i][j] = (int) mat->body.fbody[i][j];
                    break;
                case Float:
                    res->body.fbody[i][j] = (double) mat->body.ibody[i][j];
                    break;
            }
        }
    }

    return res;
}
