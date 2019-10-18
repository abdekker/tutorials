#!/bin/bash
# Demonstrates checking parameters passed to the script
# Example: bash ./Parameters.sh --help --unknown
version="1.0.0"

show_help()
{
    cat << EOF
Usage: $0 [options]
Options:
  --help            print this message
  --version         print version of this script file
EOF
}

show_version()
{
    echo Version: $version
}

interactive=TRUE
declare -i paramcount=0     # Declare as integer, but simply "paramcount=0" also works
declare -i paramknown=0
for param in "$@"; do
    # Alternatives for incrementing an integer include:
    # ((var=var+1))
    # let "var=var+1"
    ((paramcount+=1))

    # "Help" checked one way...
    if echo $param | grep "^--help" > /dev/null 2> /dev/null; then
        show_help
        ((paramknown+=1))
    fi

    # "Version" checked another way...
    if [ $param = "--version" ]; then
        show_version
        ((paramknown+=1))
    fi
done

if [ $paramcount = 0 ]; then
    echo No parameters supplied...
else
    # Add a blank line if any parameters were processed; "-gt" is "greater than"
    if [ $paramknown -gt 0 ]; then
        echo
    fi

    # Total number of parameters
    echo Total parameters: $paramcount

    # Known parameters (the "elif [...]" can simply be "else", but demonstrates "else if")
    if [ $paramknown = 0 ]; then
        echo Known parameters: None
    elif [ $paramknown -gt 0 ]; then
        echo Known parameters: $paramknown
    fi
fi
