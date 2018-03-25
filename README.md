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
- Run tests using the command `./testall.sh [options] [.neo files]` (see `./testall.sh -h` for help)
- Test suite will use the Neo compiler to compile to LLVM, which then links with external C code (see `native.c`) and produces an executable for each test program (specified either by the `[.neo files]` command line argument, or the files in the `test` directory otherwise)
- Tests with the `test-` prefix are positive tests; these tests produce observable output which are compared against gold standard output (`*.out` files)
- Tests with the `fail-` prefix are negative tests; these tests produce error messages which are compared against expected error messages (`*.err` files)
- An overall log of the test results will be produced in addition to console output in `testall.log`
