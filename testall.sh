#!/bin/bash
# Author(s): Alex

# Regression testing script for Neo
# Step through a list of files
#  Compile, run, and check the output of each expected-to-work test
#  Compile and check the error of each expected-to-fail test

# Path to the LLVM interpreter
LLI="lli"

# Path to the LLVM compiler
LLC="llc"

# Path to the C compiler
CC="gcc"

# C Flags
CFLAGS="-g -Wall"

# Path to the Neo compiler.  Usually "./neo.native"
# Try "_build/neo.native" if ocamlbuild was unable to create a symbolic link.
NEO="./neo.native"
#NEO="_build/neo.native"

# Set time limit for all operations
ulimit -t 30

globallog=testall.log
rm -f $globallog
error=0
globalerror=0

keep=0

pass=0
fail=0

Usage() {
    echo "Usage: testall.sh [options] [.neo files]"
    echo "-k    Keep intermediate files"
    echo "-h    Print this help"
    exit 1
}

SignalError() {
    if [ $error -eq 0 ]; then
        echo "FAILED"
        error=1
    fi
    echo "  $1"
}

# Compare <outfile> <reffile> <difffile>
# Compares the outfile with reffile.  Differences, if any, written to difffile
Compare() {
    generatedfiles="$generatedfiles $3"
    hexpattern="0x[a-z0-9]\+"
    # Ignore any printing of memory addresses, since this will
    # vary by test, possibly
    echo diff -b <(sed "s/$hexpattern//g" $1) <(sed "s/$hexpattern//g" $2) ">" $3 1>&2
    diff -b <(sed "s/$hexpattern//g" $1) <(sed "s/$hexpattern//g" $2) > "$3" 2>&1 || {
	SignalError "$1 differs"
	echo "FAILED $1 differs from $2" 1>&2
    }
}

# Run <args>
# Report the command, run it, and report any errors
Run() {
    echo $* 1>&2
    eval $* || {
        SignalError "$1 failed on $*"
        return 1
    }
}

# RunFail <args>
# Report the command, run it, and expect an error
RunFail() {
    echo $* 1>&2
    eval $* && {
        SignalError "failed: $* did not report an error"
        return 1
    }
    return 0
}

Check() {
    error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.neo//'`
    reffile=`echo $1 | sed 's/.neo$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."

    echo -n "$basename..."

    echo 1>&2
    echo "###### Testing $basename" 1>&2

    generatedfiles=""

    generatedfiles="$generatedfiles ${basename}.ll ${basename}.s ${basename}.exe ${basename}.out" &&
    Run "$NEO" "$1" ">" "${basename}.ll" &&
    Run "$LLC" "${basename}.ll" ">" "${basename}.s" &&
    Run "$CC" "$CFLAGS" "-o" "${basename}.exe" "${basename}.s" "libneoc.o" "-lm" &&
    Run "./${basename}.exe" > "${basename}.out" &&
    Compare ${basename}.out ${reffile}.out ${basename}.diff

    # Report the status and clean up the generated files

    if [ $error -eq 0 ]; then
        if [ $keep -eq 0 ]; then
            rm -f $generatedfiles
        fi
        echo "OK"
        echo "###### SUCCESS" 1>&2
        ((pass=pass+1))
    else
        echo "FAILED"
        echo "###### FAILED" 1>&2
        globalerror=$error
        ((fail=fail+1))
    fi
}

CheckFail() {
    error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.neo//'`
    reffile=`echo $1 | sed 's/.neo$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."

    echo -n "$basename..."

    echo 1>&2
    echo "###### Testing $basename" 1>&2

    generatedfiles=""

    if [ $2 -eq 0 ]; then
        generatedfiles="$generatedfiles ${basename}.err ${basename}.diff"
        RunFail "$NEO" "<" $1 "2>" "${basename}.err" ">>" $globallog
        Compare ${basename}.err ${reffile}.err ${basename}.diff
    else
        generatedfiles="$generatedfiles ${basename}.ll ${basename}.s ${basename}.exe ${basename}.out ${basename}.err"
        Run "$NEO" "$1" ">" "${basename}.ll" &&
        Run "$LLC" "${basename}.ll" ">" "${basename}.s" &&
        Run "$CC" "$CFLAGS" "-o" "${basename}.exe" "${basename}.s" "libneoc.o" "-lm" &&
        RunFail "./${basename}.exe" "2>" "${basename}.err" ">" "${basename}.out"
        Compare ${basename}.err ${reffile}.err ${basename}-err.diff
        Compare ${basename}.out ${reffile}.out ${basename}-out.diff
    fi

    # Report the status and clean up the generated files

    if [ $error -eq 0 ]; then
        if [ $keep -eq 0 ]; then
            rm -f $generatedfiles
        fi
        echo "OK"
        echo "###### SUCCESS" 1>&2
        ((pass=pass+1))
    else
        echo "FAILED"
        echo "###### FAILED" 1>&2
        globalerror=$error
        ((fail=fail+1))
    fi
}

while getopts kdpsh c; do
    case $c in
	k) # Keep intermediate files
	    keep=1
	    ;;
	h) # Help
	    Usage
	    ;;
    esac
done

shift `expr $OPTIND - 1`

LLIFail() {
  echo "Could not find the LLVM interpreter \"$LLI\"."
  echo "Check your LLVM installation and/or modify the LLI variable in testall.sh"
  exit 1
}

which "$LLI" >> $globallog || LLIFail

if [ ! -f libneoc.o ]; then
    echo "Could not find libneoc.o"
    echo "Try \"make libneoc.o\""
    exit 1
fi

if [ $# -ge 1 ]; then
    files=$@
else
    files="test/test-*.neo test/fail-*.neo test/runtime-fail-*.neo"
fi

for file in $files
do
    case $file in
	*test-*)
	    Check $file 2>> $globallog
	    ;;
    *runtime-fail-*)
        CheckFail $file 1 2>> $globallog
        ;;
	*fail-*)
	    CheckFail $file 0 2>> $globallog
	    ;;
	*)
	    echo "unknown file type $file"
	    globalerror=1
	    ;;
    esac
done

echo "Passed: $pass, Failed: $fail"

exit $globalerror
