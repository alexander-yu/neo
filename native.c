#include <stdio.h>
#include <stdlib.h>
#include "native.h"

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
    printf("{|");
    for (int i = 0; i < arr->length; i++) {
        print_element(arr->body[i]);
        if (i != arr->length - 1) {
            printf(", ");
        }
    }
    printf("|}");
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

void set_ptrs_array(array_t *arr, void *body) {
    for (int i = 0; i < arr->length; i++) {
        arr->body[i] = (void *)((char *)body + i * arr->size);
    }
}
