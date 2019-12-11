#!/bin/bash
# Demonstrates using functions in bash
# Example: bash ./Functions.sh
# Adapted from: https://www.tldp.org/LDP/abs/html/functions.html

funcHello1()
{
    echo Hello world 1! \(No parameter\)
}

# For single-line functions, you need a trailing semi-colon
funcHello2() { echo Hello world 2! \(No parameter, single line\); }
funcHello3()
{
    echo Hello $1 3! \(Single parrmater\)
}

funcHello4()
{
    echo $1 $2 4! \(Multiple parameters\)
}

funcTake()
{
    echo hello from funcTake
    touch funcTake.txt
}

interactive=TRUE
echo Using functions in Bash
echo .

echo "### Calling local functions ###"
funcHello1
funcHello2
funcHello3 world
funcHello4 Hello world
echo .

echo "### Calling global functions ###"
echo "These have been included in the user .bashrc file (look for .bash_usr_functions)"
echo "The function needs to be exported"
take    # Normally you would provide the directory name...
echo .

echo "### List functions in script ###"
echo "  "1\) Use \"declare -f\" to list all functions \(including function body\)
# declare -f; echo .
echo "  "2\) Use \"declare -F\" to list only the names of defined functions
# declare -F; echo .
echo "  "3\) Use \"declare -f FUNC-NAME\" to list only the specified function
#declare -f funcHello1; echo .
#declare -f take; echo .
echo .

echo The end!
