# Neo

A matrix language
by Alex Yu, Andrés Aguayo, Ani Bezjian, Dominique Gordon, and Mohamed Abedelmalik

## Installation

- Navigate to root directory of project
- Compile using the command `ocamlbuild src/neo.native`
- Execute a program using the command `./neo.native file_name`

## Testing

- Navigate to root directory of project (**Note**: if you have compiled Neo, make sure you run `ocamlbuild -clean` before running the test script)
- Run tests using the command `bash test/test.sh [output_file]` or `test/test.sh [output_file]` if you have execute permissions set on the script
- **Note**: tests with the `_fail.neo` suffix will be shown as passing if they fail as intended

## Missing Syntax

- Exceptions (i.e. declaring exception types, raising exceptions, and try/catch blocks)
- Functions as first-class objects; we don't have types for functions, or the ability to parse function literals
- Declaring and assigning variables simultaneously/interweaving variable declarations with other statements in a function/file (we used the MicroC parser as a baseline)
- Array/matrix indexing/slicing
