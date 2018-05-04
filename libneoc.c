#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <math.h>
#include "libneoc.h"

/* Some versions of stdlib.h may have max/min macros,
 * like Visual C++'s version. If they exist, undefine them
 * for consistency. */
#ifdef max
    #undef max
#endif

#ifdef min
    #undef min
#endif

#define BUFFER_SIZE 1024

static const char* NULL_VALUE_ERR = "Null value error: attempted to read null value";
static const char* DIV_ZERO_ERR = "Zero division error: attmpted to perform division or modulo by 0";
static const char* EXP_ZERO_ERR = "Zero division error: attempted to raise 0 to a negative power";
static const char* EXP_NEG_ERR = "Arithmetic error: attempted to raise negative number to a non-integer power";
static const char* MAT_IDX_ERR = "Matrix index error: attempted to access an out of bounds index";
static const char* MAT_INS_ERR = "Matrix index error: attempted to insert to an invalid index";
static const char* ARR_IDX_ERR = "Array index error: attempted to access an out of bounds index";
static const char* ARR_INS_ERR = "Array index error: attempted to insert to an invalid index";
static const char* SLICE_ERR = "Slice error: attempted to perform slice that would return zero elements \
                               (reversed or equal bounds)";
static const char* ARR_LEN_ERR = "Length error: attempted to create array of nonpositive length";
static const char* MAT_ROWS_ERR = "Dimension error: attempted to create matrix with nonpositive rows";
static const char* MAT_COLS_ERR = "Dimension error: attempted to create matrix with nonpositive columns";
static const char* ROW_DIM_ERR = "Dimension error: attempted to insert/append row with incompatible dimensions";
static const char* MAT_MULT_ERR = "Dimension error: attempted to perform matrix multiplication with \
                                  incompatible dimensions";
static const char* MAT_BINOP_ERR = "Dimension error: attempted to perform matrix binary operations with \
                                   incompatible/non-broadcastable dimensions";

typedef union value {
    int i;
    double f;
} value_t;

/* Min/max helpers */
static int max(const int a, const int b) {
    return (a > b) ? a : b;
}

static int min(const int a, const int b) {
    return (a < b) ? a : b;
}

/* Runtime error-checking functions */
void _die(const char* err) {
    fprintf(stderr, "%s\n", err);
    exit(EXIT_FAILURE);
}

void _check(const bool cond, const char* err) {
    if (!cond) {
        _die(err);
    }
}

static void check_arr_index(const array_t* arr, const int i) {
    _check(i >= 0 && i < arr->length, ARR_IDX_ERR);
}

static void check_arr_slice(const array_t* arr, const slice_t* slice) {
    int start = slice->start;
    int end = slice->end;
    _check(start < end, SLICE_ERR);
    check_arr_index(arr, start);
    _check(end <= arr->length, ARR_IDX_ERR);
}

static void check_mat_index(const matrix_t* mat, const int i, const int j) {
    _check(i >= 0 && i < mat->rows, MAT_IDX_ERR);
    _check(j >= 0 && j < mat->cols, MAT_IDX_ERR);
}

static void check_mat_slice(const matrix_t* mat, const slice_t* row_slice, const slice_t* col_slice) {
    int start_i = row_slice->start;
    int end_i = row_slice->end;
    int start_j = col_slice->start;
    int end_j = col_slice->end;
    _check(start_i < end_i, SLICE_ERR);
    _check(start_j < end_j, SLICE_ERR);
    check_mat_index(mat, start_i, start_j);
    _check(end_i <= mat->rows, MAT_IDX_ERR);
    _check(end_j <= mat->cols, MAT_IDX_ERR);
}

static void check_row_dims(const matrix_t* mat, const matrix_t* row) {
    _check(row->rows == 1, ROW_DIM_ERR);
    _check(row->cols == mat->cols, ROW_DIM_ERR);
}

static void check_mat_binop_dims(const matrix_t* a, const matrix_t* b) {
    int rows_a = a->rows;
    int rows_b = b->rows;
    int cols_a = a->cols;
    int cols_b = b->cols;
    _check(rows_a == rows_b || min(rows_a, rows_b) == 1, MAT_BINOP_ERR);
    _check(cols_a == cols_b || min(cols_a, cols_b) == 1, MAT_BINOP_ERR);
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

static void print_matrix(const matrix_t* mat, const bool flat) {
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

void _print_matrix(const matrix_t* m) {
    print_matrix(m, false);
}

void _print_flat_matrix(const matrix_t* m) {
    print_matrix(m, true);
}

void _print_function(const void* p) {
    printf("function at %p", p);
}

void _print_array(const array_t* arr, void(*const print_element)(const void*)) {
    int length = arr->length;
    printf("{|");
    for (int i = 0; i < length; i++) {
        print_element(_get_array(arr, i));
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

void _deep_free_array(array_t* arr, void(*const free_element)(void*)) {
    int length = arr->length;

    for (int i = 0; i < length; i++) {
        free_element(_get_array(arr, i));
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
        arr->body[i] = (char*)body + i * size;
    }
}

array_t* _malloc_array(const int length, const size_t size, const bool has_ptrs) {
    _check(length > 0, ARR_LEN_ERR);
    array_t* arr = malloc(sizeof(array_t));
    arr->body = malloc(length * sizeof(void*));
    arr->length = length;
    arr->size = size;
    arr->has_ptrs = has_ptrs;

    void* body = malloc(length * sizeof(void*));
    set_ptrs_array(arr, body);

    return arr;
}

matrix_t* _malloc_matrix(const int rows, const int cols, const enum mat_type type) {
    _check(rows > 0, MAT_ROWS_ERR);
    _check(cols > 0, MAT_COLS_ERR);
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
void* _get_array(const array_t* arr, const int i) {
    check_arr_index(arr, i);
    if (arr->has_ptrs) {
        _check(*(void**)arr->body[i] != NULL, NULL_VALUE_ERR);
    }
    return arr->body[i];
}

void _set_array(array_t* arr, const int i, const void* data) {
    check_arr_index(arr, i);
    /* Perform a shallow copy */
    memcpy(arr->body[i], data, arr->size);
}

array_t* _slice_array(const array_t* arr, const slice_t* slice) {
    check_arr_slice(arr, slice);
    int start_i = slice->start;
    int end_i = slice->end;
    int length = end_i - start_i;
    array_t* res = _malloc_array(length, arr->size, arr->has_ptrs);

    for (int i = 0; i < length; i++) {
        _set_array(res, i, _get_array(arr, i + start_i));
    }

    return res;
}

void _set_slice_array(array_t* arr, const slice_t* slice, const array_t* data) {
    check_arr_slice(arr, slice);
    int start_i = slice->start;
    int end_i = slice->end;
    for (int i = start_i; i < end_i; i++) {
        _set_array(arr, i, data->body[i - start_i]);
    }
}

array_t* _insert_array(const array_t* arr, const int pos_i, const void* data) {
    _check(pos_i >= 0 && pos_i <= _length(arr), ARR_INS_ERR);
    int length = arr->length + 1;
    array_t* res = _malloc_array(length, arr->size, arr->has_ptrs);

    for (int i = 0; i < length; i++) {
        if (i < pos_i) {
            _set_array(res, i, arr->body[i]);
        } else if (i == pos_i) {
            _set_array(res, i, data);
        } else {
            _set_array(res, i, arr->body[i - 1]);
        }
    }

    return res;
}

array_t* _delete_array(const array_t* arr, const int pos_i) {
    check_arr_index(arr, pos_i);
    int length = arr->length - 1;
    array_t* res = _malloc_array(length, arr->size, arr->has_ptrs);

    for (int i = 0; i < length; i++) {
        if (i < pos_i) {
            _set_array(res, i, arr->body[i]);
        } else {
            _set_array(res, i, arr->body[i + 1]);
        }
    }

    return res;
}

array_t* _append_array(const array_t* arr, const void* data) {
    return _insert_array(arr, _length(arr), data);
}

/* Matrix index/slice functions */
void* _get_matrix(const matrix_t* mat, const int i, const int j) {
    check_mat_index(mat, i, j);
    switch (mat->type) {
        case Int: return &mat->body.ibody[i][j];
        case Float: return &mat->body.fbody[i][j];
    }
    /**
     * This is for placating GCC warnings, since get_matrix is
     * a non-void function, and control flow thus can't reach the
     * end of the function
     */
    perror("get_matrix got invalid mat_type; this should never happen");
    exit(EXIT_FAILURE);
}

void _set_matrix(matrix_t* mat, const int i, const int j, const void* data) {
    check_mat_index(mat, i, j);
    switch (mat->type) {
        case Int: mat->body.ibody[i][j] = *(int*)data; break;
        case Float: mat->body.fbody[i][j] = *(double*)data; break;
    }
}

matrix_t* _slice_matrix(const matrix_t* mat, const slice_t* row_slice,
                       const slice_t* col_slice) {
    check_mat_slice(mat, row_slice, col_slice);
    int start_i = row_slice->start;
    int end_i = row_slice->end;
    int start_j = col_slice->start;
    int end_j = col_slice->end;

    int rows = end_i - start_i;
    int cols = end_j - start_j;
    enum mat_type type = mat->type;
    matrix_t* res = _malloc_matrix(rows, cols, type);

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

void _set_slice_matrix(matrix_t* mat, const slice_t* row_slice,
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
    _check(row_i >= 0 && row_i <= _rows(mat), MAT_INS_ERR);
    check_row_dims(mat, row);
    int rows = mat->rows + 1;
    int cols = mat->cols;
    enum mat_type type = mat->type;
    matrix_t* res = _malloc_matrix(rows, cols, type);

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
    matrix_t* res = _malloc_matrix(rows, cols, type);

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
    return _insert_matrix(mat, _rows(mat), row);
}

/* Binary operations */
int _iexp(const int a, const int b){
    return (int) pow((double) a, (double) b);
}

double _fexp(const double a, const double b){
    return pow(a, b);
}

matrix_t* _matmult(const matrix_t* a, const matrix_t* b){
    _check(a->cols == b->rows, MAT_MULT_ERR);
    int rows = a->rows;
    int cols = b->cols;
    enum mat_type type = a->type;
    matrix_t* res = _malloc_matrix(rows, cols, type);

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

matrix_t* _mat_binop(const matrix_t* a, const enum mat_op op, const matrix_t* b) {
    check_mat_binop_dims(a, b);
    int rows = max(a->rows, b->rows);
    int cols = max(a->cols, b->cols);
    enum mat_type type = a->type;
    matrix_t* res = _malloc_matrix(rows, cols, type);

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
                            _check(b_ij != 0, DIV_ZERO_ERR);
                            r_body[i][j] = a_ij / b_ij;
                            break;
                        case Mod:
                            _check(b_ij != 0, DIV_ZERO_ERR);
                            r_body[i][j] = a_ij % b_ij;
                            break;
                        case Exp:
                            _check(a_ij != 0 || b_ij >= 0, EXP_ZERO_ERR);
                            /* No need to perform check for negative base
                             * raised to non-integer exponent; the exponents
                             * here are inherently integers already */
                            r_body[i][j] = _iexp(a_ij, b_ij);
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
                            _check(b_ij != 0., DIV_ZERO_ERR);
                            r_body[i][j] = a_ij / b_ij;
                            break;
                        case Mod:
                            _check(b_ij != 0., DIV_ZERO_ERR);
                            r_body[i][j] = fmod(a_ij, b_ij);
                            break;
                        case Exp:
                            _check(a_ij != 0. || b_ij >= 0, EXP_ZERO_ERR);
                            _check(a_ij >= 0. || floor(b_ij) == b_ij, EXP_NEG_ERR);
                            r_body[i][j] = _fexp(a_ij, b_ij);
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

/* File I/O functions */
static size_t trim(char *out, const char* str) {
    if (str == NULL) {
        return 0;
    }

    size_t chars = strlen(str);
    size_t out_size;
    const char* end = str + chars - 1;

    if (chars == 0) {
        return 0;
    }

    /* Left-trim */
    while (isspace(*str)) {
        str++;
    }

    /* If all spaces, return empty string */
    if (*str == 0) {
        *out = 0;
        return 1;
    }

    /* Right-trim */
    while (end > str && isspace(*end)) {
        end--;
    }

    end++;
    out_size = min(end - str, chars);
    memcpy(out, str, out_size);
    out[out_size] = 0;

    return out_size;
}

matrix_t* _read_mat(const char* filename, const enum mat_type t) {
    const char* delim = ",";

    size_t buf_size = BUFFER_SIZE;
    char* buffer = malloc(BUFFER_SIZE);
    char err[BUFFER_SIZE];
    matrix_t* mat;
    size_t chars;

    FILE* stream = fopen(filename, "r");

    if (stream == NULL) {
        snprintf(err, BUFFER_SIZE, "Failed to open %s\n", filename);
        goto cleanup;
    }

    if ((chars = getline(&buffer, &buf_size, stream)) <= 0) {
        snprintf(err, BUFFER_SIZE, "No matrix header line in %s\n", filename);
        goto file_cleanup;
    }

    char* line_ptr = buffer;

    int rows, cols;
    enum mat_type type;

    int header_count = 0;
    char* token = malloc(chars + 1);
    size_t token_size = trim(token, strsep(&line_ptr, delim));

    while (token_size > 0) {
        char* val_ptr;

        switch (header_count) {
            case 0: {
                rows = (int) strtol(token, &val_ptr, 10);
                if (strlen(val_ptr) > 0 || rows <= 0) {
                    snprintf(err, BUFFER_SIZE,
                             "Invalid rows quantifier %s in %s\n", token, filename);
                    goto token_cleanup;
                }
                break;
            }
            case 1:
                cols = (int) strtol(token, &val_ptr, 10);
                if (strlen(val_ptr) > 0 || cols <= 0) {
                    snprintf(err, BUFFER_SIZE,
                             "Invalid columns quantifier %s in %s\n", token, filename);
                    goto token_cleanup;
                }
                break;
            case 2:
                if (strcmp(token, "int") == 0) {
                    type = Int;
                } else if (strcmp(token, "float") == 0) {
                    type = Float;
                } else {
                    snprintf(err, BUFFER_SIZE,
                             "Invalid type specifier %s in %s\n", token, filename);
                    goto token_cleanup;
                }

                if (type != t) {
                    char* type_str = type ? "float" : "int";
                    char* t_str = t ? "float" : "int";
                    snprintf(err, BUFFER_SIZE,
                             "Attempted to read a matrix<%s> but found matrix<%s> in %s\n",
                             t_str, type_str, filename);
                    goto token_cleanup;
                }

                break;
            default:
                snprintf(err, BUFFER_SIZE,
                         "Should be 3 header fields but found %d in %s\n",
                         header_count + 1, filename);
                goto token_cleanup;
        }

        token_size = trim(token, strsep(&line_ptr, delim));
        header_count++;
    }

    mat = _malloc_matrix(rows, cols, type);
    int row_count = 0;

    while ((chars = getline(&buffer, &buf_size, stream)) != -1) {
        line_ptr = buffer;
        free(token);
        token = malloc(chars + 1);

        if (row_count >= rows) {
            snprintf(err, BUFFER_SIZE,
                     "Found at least %d rows but rows quantifier is %d in %s\n",
                     row_count + 1, rows, filename);
            goto mat_cleanup;
        }

        int col_count = 0;
        token_size = trim(token, strsep(&line_ptr, delim));

        while (token_size > 0) {
            if (col_count >= cols) {
                snprintf(err, BUFFER_SIZE,
                         "Row %d has %d values but columns specifier is %d in %s\n",
                         row_count, col_count + 1, cols, filename);
                goto mat_cleanup;
            }

            char* val_ptr;
            value_t data;

            if (mat->type == Int) {
                data.i = (int) strtol(token, &val_ptr, 10);
                if (strlen(val_ptr) > 0) {
                    snprintf(err, BUFFER_SIZE,
                             "Non-integer value %s at row %d, column %d in %s\n",
                             token, row_count, col_count, filename);
                    goto mat_cleanup;
                }
            } else {
                data.f = (double) strtod(token, &val_ptr);
                if (strlen(val_ptr) > 0) {
                    snprintf(err, BUFFER_SIZE,
                             "Non-float value %s at row %d, column %d in %s\n",
                             token, row_count, col_count, filename);
                    goto mat_cleanup;
                }
            }

            _set_matrix(mat, row_count, col_count, &data);
            token_size = trim(token, strsep(&line_ptr, delim));
            col_count++;
        }

        if (col_count < cols) {
            snprintf(err, BUFFER_SIZE,
                     "Row %d has %d values but columns quantifier is %d in %s\n",
                     row_count, col_count, cols, filename);
            goto mat_cleanup;
        }

        row_count++;
    }

    if (row_count < rows) {
        snprintf(err, BUFFER_SIZE,
                 "Found only %d rows but rows quantifier is %d in %s\n",
                 row_count, rows, filename);
        goto mat_cleanup;
    }

    free(token);
    fclose(stream);
    free(buffer);
    return mat;

mat_cleanup:
    _free_matrix(mat);

token_cleanup:
    free(token);

file_cleanup:
    fclose(stream);

cleanup:
    free(buffer);
    _die(err);
    return mat; // This never returns, due to _die call
}

void _write_mat(matrix_t* mat, const char* filename) {
    char err[BUFFER_SIZE];

    FILE* stream = fopen(filename, "w");

    if (stream == NULL) {
        snprintf(err, BUFFER_SIZE, "Failed to open %s\n", filename);
        _die(err);
    }

    int rows = mat->rows;
    int cols = mat->cols;
    enum mat_type type = mat->type;

    fprintf(stream, "%d,%d,%s\n", rows, cols, type ? "float" : "int");

    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols; j++) {
            if (j == cols - 1) {
                switch (type) {
                    case Int: fprintf(stream, "%d", mat->body.ibody[i][j]); break;
                    case Float: fprintf(stream, "%g", mat->body.fbody[i][j]); break;
                }
            } else {
                switch (type) {
                    case Int: fprintf(stream, "%d, ", mat->body.ibody[i][j]); break;
                    case Float: fprintf(stream, "%g, ", mat->body.fbody[i][j]); break;
                }
            }
        }

        fprintf(stream, "\n");
    }

    fflush(stream);
    fclose(stream);
}

/* Miscellaneous helpers/built-ins */
void _init_array(array_t* arr, const void* data) {
    int length = arr->length;
    for (int i = 0; i < length; i++) {
        _set_array(arr, i, data);
    }
}

void _init_matrix(matrix_t* mat) {
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

int _length(const array_t* arr) {
    return arr->length;
}

int _rows(const matrix_t* mat) {
    return mat->rows;
}

int _cols(const matrix_t* mat) {
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
    matrix_t* res = _malloc_matrix(rows, cols, type);

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
