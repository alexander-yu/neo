#!/bin/bash

SCANNER_TEST_DIR="./test/scanner/"
FAIL_C='\033[0;31m'
PASS_C='\033[0;32m'
NO_C='\033[0m'

clean() {
    echo "Cleaning up..."
    make clean
}

color_echo() {
    color=$1
    string=$2
    echo -e "$color$string$NO_C"
}

test_pass() {
    test_path=$1
    test_status=$2

    if [[ $test_path == *_fail.neo ]] ; then
        [ $test_status -gt 0 ] ; echo $?
    else
        [ $test_status -eq 0 ] ; echo $?
    fi
}

has_match() {
    pattern=$1
    if compgen -G $pattern > /dev/null; then
        echo 1
    else
        echo 0
    fi
}

test_scanner() {
    program_output=$1
    test_pattern=${SCANNER_TEST_DIR}*.neo
    passed=0
    failed=0

    test_files_exist=$(has_match $test_pattern)

    echo "--------Scanner Test Results--------"
    echo "========Scanner Program Output========" > $program_output
    echo "" >> $program_output

    if [ $test_files_exist ]; then
        for file in $test_pattern ; do
            echo "------$file------" >> $program_output
            echo "" >> $program_output
            ./neo.native -a $file >> $program_output 2>&1

            status=$?
            pass=$(test_pass $file $status)

            if [ $pass -eq 0 ]; then
                color_echo $PASS_C "PASS: $file"
                ((passed++))
            else
                color_echo $FAIL_C "FAIL: $file"
                ((failed++))
            fi

            echo "" >> $program_output

        done
    else
        echo "No test files found."
    fi

    echo "Tests Passed: $passed"
    echo "Tests Failed: $failed"
}

if [ $# -ge 1 ]; then
    program_output="$1"
else
    program_output=/dev/null
fi

test_scanner $program_output
clean
exit 0
