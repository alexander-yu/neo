#include <stdio.h>
#include <stdlib.h>
#include "native.h"

void printm_int(mat_int_t *matrix) {
    printf("[");
    int m = matrix->rows;
    int n = matrix->cols;
    int **mat = matrix->body;
    for (int i = 0; i < m; ++i) {
        if (i == 0) {
            printf("[");
        } else {
            printf(" [");
        }

        for (int j = 0; j < n; ++j) {
            if (j == n - 1) {
                printf("%d", mat[i][j]);
            } else {
                printf("%d, ", mat[i][j]);
            }
        }

        if (i == m - 1) {
            printf("]");
        } else {
            printf("],\n");
        }
    }
    printf("]\n");
    fflush(stdout);
}

mat_int_t * makem_int(int *body, int rows, int cols) {
    mat_int_t *mat = malloc(sizeof(mat_int_t));
    int **mat_body = malloc(rows * sizeof(int *));

    for (int i = 0; i < rows; i++) {
        mat_body[i] = &body[i * rows];
    }

    mat->body = mat_body;
    mat->rows = rows;
    mat->cols = cols;
    return mat;
}
