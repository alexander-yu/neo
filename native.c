#include <stdio.h>
#include "native.h"

void printm_int(mat_int_t *matrix) {
    printf("[");
    int m = matrix->rows;
    int n = matrix->cols;
    int **mat = matrix->mat;
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
