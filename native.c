#include <stdio.h>

void printm_int(int* mat, int m, int n) {
    printf("[");
    for (int i = 0; i < m; ++i) {
        if (i == 0) {
            printf("[");
        } else {
            printf(" [");
        }

        for (int j = 0; j < n; ++j) {
            if (j == n - 1) {
                printf("%d", *((mat + i * n) + j));
            } else {
                printf("%d, ", *((mat + i * n) + j));
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
