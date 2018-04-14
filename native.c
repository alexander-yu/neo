#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <math.h>
#include "native.h"

void print_bool(bool b) {
    if (b) {
        printf("True");
    } else {
        printf("False");
    }
}

void print_matrix(matrix_t *mat, bool flat) {
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

void free_matrix(matrix_t *mat) {
    union mat_body body = mat->body;
    enum mat_type type = mat->type;

    switch (type) {
        case Int: free(body.ibody[0]); free(body.ibody); break;
        case Float: free(body.fbody[0]); free(body.fbody); break;
    }

    free(mat);
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

void free_array(array_t *arr, void(*free_element)(void *)) {
    void **body = arr->body;
    int length = arr->length;

    for (int i = 0; i < length; i++) {
        free_element(body[i]);
    }

    free(body[0]);
    free(body);
    free(arr);
}

void init_matrix(matrix_t *mat) {
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

void set_ptrs_matrix(matrix_t *mat, void *body) {
    int rows = mat->rows;
    int cols = mat->cols;
    for (int i = 0; i < rows; i++) {
        switch (mat->type) {
            case Int:
                mat->body.ibody[i] = (int *)((char *)body + i * cols * sizeof(int));
                break;
            case Float:
                mat->body.fbody[i] = (double *)((char *)body + (i * cols * sizeof(double)));
                break;
        }
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

void * get_matrix(matrix_t *mat, int i, int j) {
    switch (mat->type) {
        case Int: return (void *)&mat->body.ibody[i][j];
        case Float: return (void *)&mat->body.fbody[i][j];
    }
    /**
     * This is for placating GCC warnings, since get_matrix is
     * a non-void function, and control flow thus can't reach the
     * end of the function
     */
    perror("get_matrix got invalid mat_type; this should never happen");
    exit(EXIT_FAILURE);
}

void set_matrix(matrix_t *mat, int i, int j, void *data) {
    switch (mat->type) {
        case Int: mat->body.ibody[i][j] = *(int *)data; break;
        case Float: mat->body.fbody[i][j] = *(double *)data; break;
    }
}

void slice_matrix(matrix_t *mat, slice_t *row_slice, slice_t *col_slice, matrix_t *res) {
    int start_i = row_slice->start;
    int end_i = row_slice->end;
    int start_j = col_slice->start;
    int end_j = col_slice->end;
    for (int i = start_i; i < end_i; i++) {
        for (int j = start_j; j < end_j; j++) {
            switch (mat->type) {
                case Int:
                    res->body.ibody[i - start_i][j - start_j] = mat->body.ibody[i][j];
                    break;
                case Float:
                    res->body.fbody[i - start_i][j - start_j] = mat->body.fbody[i][j];
                    break;
            }
        }
    }
}

void set_slice_matrix(matrix_t *mat, slice_t *row_slice, slice_t *col_slice, matrix_t *data) {
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

int iexp(int e1, int e2){
    return (int) pow((double) e1, (double) e2);
}

double fexp(double e1, double e2){
    return pow(e1,e2);
}

void matmult(matrix_t * a, matrix_t * b, matrix_t * res){
    int rows = res->rows;
    int cols = res->cols;
    for (int i = 0; i < rows; i++){
        for (int j = 0; j<cols; j++){
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
}
