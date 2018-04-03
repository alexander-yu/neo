#include <stdio.h>
#include <stdlib.h>
#include "native.h"

void print_matrixi(matrixi_t *matrix) {
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
            printf("],\n");
        }
    }
    printf("]\n");
}

void print_array(array_t *arr, void(*print_element)(void *)) {
    printf("{|");
    for (int i = 0; i < arr->length; i++) {
        print_element(array_get(arr, i));
        if (i != arr->length - 1) {
            printf(", ");
        }
    }
    printf("|}\n");
}

void init_matrixi (matrixi_t *mat) {
    for (int i = 0; i < mat->rows; i++) {
        for (int j = 0; j < mat->cols; j++) {
            mat->body[i][j] = 0;
        }
    }
}

void set_ptrs_matrixi(matrixi_t *mat, int *body) {
    for (int i = 0; i < mat->rows; i++) {
        mat->body[i] = &body[i * mat->cols];
    }
}

void *array_get(array_t *arr, int i) {
    return &arr->body + i * arr->size;
}
