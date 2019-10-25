#!/bin/bash
# Demonstrates using simple functions
# Example: bash ./Functions.sh
# Adapted from: https://www.tldp.org/LDP/abs/html/functions.html

funcHello()
{
  # Can be on a single line: "funcHello() { echo Hello world!; }"
  echo Hello world!
}

funcHello2() { echo Hello world 2!; }

interactive=TRUE
echo Using functions in Bash
echo

echo "### List functions in script ###"
echo "  "1\) Use \"declare -f\" to list all functions \(including function body\)
declare -f; echo .
echo "  "2\) Use \"declare -f FUNC-NAME\" to list only the specified function
declare -f funcHello; echo .
echo "  "3\) Use \"declare -F\" to list only the names of defined functions
declare -F; echo .

echo "### Calling functions ###"
funcHello
funcHello2
echo .
echo The end!
