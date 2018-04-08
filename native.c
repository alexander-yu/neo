#include <stdio.h>
#include <stdlib.h>
#include "native.h"

void print_bool(bool b) {
    if (b) {
        printf("True");
    } else {
        printf("False");
    }
}

void print_matrixi(matrixi_t *matrix, bool flat) {
    printf("[");
    int rows = matrix->rows;
    int cols = matrix->cols;
    int **mat = matrix->body;
    for (int i = 0; i < rows; ++i) {
        if (i == 0) {
            printf("[");
        } else {
            printf(" [");
        }

        for (int j = 0; j < cols; ++j) {
            if (j == cols - 1) {
                printf("%d", mat[i][j]);
            } else {
                printf("%d, ", mat[i][j]);
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

void print_array(array_t *arr, void(*print_element)(void *)) {
    void **body = arr->body;
    int length = arr->length;
    printf("{|");
    for (int i = 0; i < length; i++) {
        print_element(body[i]);
        if (i != length - 1) {
            printf(", ");
        }
    }
    printf("|}");
}

void init_matrixi (matrixi_t *mat) {
    int **body = mat->body;
    int rows = mat->rows;
    int cols = mat->cols;
    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols; j++) {
            body[i][j] = 0;
        }
    }
}

void set_ptrs_matrixi(matrixi_t *mat, int *body) {
    int rows = mat->rows;
    int cols = mat->cols;
    for (int i = 0; i < rows; i++) {
        mat->body[i] = &body[i * cols];
    }
}

void set_ptrs_array(array_t *arr, void *body) {
    int length = arr->length;
    size_t size = arr->size;
    for (int i = 0; i < length; i++) {
        arr->body[i] = (void *)((char *)body + i * size);
    }
}

void *get_array(array_t *arr, int i) {
    return arr->body[i];
}

void set_array(array_t *arr, int i, void *data) {
    arr->body[i] = data;
}

void slice_array(array_t *arr, slice_t *slice, array_t *res) {
    int start_i = slice->start;
    int end_i = slice->end;
    for (int i = start_i; i < end_i; i++) {
        res->body[i - start_i] = arr->body[i];
    }
}

void set_slice_array(array_t *arr, slice_t *slice, array_t *data) {
    int start_i = slice->start;
    int end_i = slice->end;
    for (int i = start_i; i < end_i; i++) {
        arr->body[i] = data->body[i - start_i];
    }
}

int get_matrixi(matrixi_t *mat, int i, int j) {
    return mat->body[i][j];
}

void set_matrixi(matrixi_t *mat, int i, int j, int data) {
    mat->body[i][j] = data;
}

void slice_matrixi(matrixi_t *mat, slice_t *row_slice, slice_t *col_slice, matrixi_t *res) {
    int start_i = row_slice->start;
    int end_i = row_slice->end;
    int start_j = col_slice->start;
    int end_j = col_slice->end;
    for (int i = start_i; i < end_i; i++) {
        for (int j = start_j; j < end_j; j++) {
            res->body[i - start_i][j - start_j] = mat->body[i][j];
        }
    }
}

void set_slice_matrixi(matrixi_t *mat, slice_t *row_slice, slice_t *col_slice, matrixi_t *data) {
    int start_i = row_slice->start;
    int end_i = row_slice->end;
    int start_j = col_slice->start;
    int end_j = col_slice->end;
    for (int i = start_i; i < end_i; i++) {
        for (int j = start_j; j < end_j; j++) {
            mat->body[i][j] = data->body[i - start_i][j - start_j];
        }
    }
}
