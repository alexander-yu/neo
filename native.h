typedef struct mat_int {
    int **body;
    int rows;
    int cols;
} mat_int_t;

void printm_int(mat_int_t *);

mat_int_t * makem_int(int *, int, int);
