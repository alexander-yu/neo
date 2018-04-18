# Neo

A matrix language

by Alex Yu, Andrés Aguayo, Ani Bezjian, Dominique Gordon, and Mohamed Abedelmalik

## Installation

- Navigate to root directory of project
- Compile using the command `make`

## Testing

### Scanner Testing

- Compile Neo (see above)
- Run tests using the command `./test/scanner/test.sh [output_file]`
- **Note**: tests with the `_fail.neo` suffix will be shown as passing if they fail as intended

### Integration Testing

- Compile Neo (see above)
- Run tests using the command `./testall.sh [options] [.neo files]` (see `./testall.sh -h` for help); if no `.neo` files are provided, the test script will automatically validate against all tests in the `test` directory
- Test suite will use the Neo compiler to compile to LLVM, which then links with external C code (see `native.c`) and produces an executable for each test program (specified either by the `[.neo files]` command line argument, or the files in the `test` directory otherwise)
- Tests with the `test-` prefix are positive tests; these tests produce observable output which are compared against gold standard output (`*.out` files)
- Tests with the `fail-` prefix are negative tests; these tests produce error messages which are compared against expected error messages (`*.err` files)
- An overall log of the test results will be produced in addition to console output in `testall.log`

#### Integration Tests

- **fail-matmult.neo**: checks that the matrix multiplication operator (`@`) fails when the operands are of different matrix types
- **fail-print.neo**: checks that attempting to redefine the `print` built-in fails
- **fail-uninitialized.neo**: checks that attempting to read/access an uninitialized local variable fails
- **test-assign.neo**: checks that various types of variable assignments work, such as standard assignments, index assignments, and block/slice assignments
- **test-decls.neo**: checks that local/global declarations work, in particular with initializer expressions
- **test-exp.neo**:  checks that the exponent operator (`^`) works on integers and floats
- **test-free.neo**: checks that the `free` and `deep_free` built-in functions properly free memory for different container types
- **test-hello.neo**: basic Hello World program that prints a matrix
- **test-index-slice.neo**: checks that index/slice expressions work
- **test-mat-arith.neo**: checks that element-wise matrix arithmetic (including broadcasting) works
- **test-matmult.neo**: checks that the matrix multiplication operator (`@`) works
- **test-mod.neo**: checks that the modulus operator (`%`) works
- **test-print.neo**: checks that the `print` built-in function properly prints various data types
- **test-return-container.neo**: checks that functions can properly return container types
- **test-transpose.neo**: checks that the `transpose` built-in functions works
