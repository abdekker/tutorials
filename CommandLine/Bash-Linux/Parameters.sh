#!/bin/bash
# Demonstrates checking parameters passed to the script
# Example: bash ./Parameters.sh --help --unknown
version="1.0.1"

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
echo The \"\$\#\" environment variable is the number of parameters to this script
echo "  "Raw count=$#
if [ $# -gt 0 ]; then
    echo "  "First parameter is \"$1\"
fi
echo

echo Processing known parameters
declare -i paramcount=0     # Declare as integer, but "paramcount=0" also works
declare -i paramknown=0
declare -i IsHelp=0
declare -i IsVersion=0
for param in "$@"; do
    # Alternatives for incrementing an integer include:
    # ((var=var+1))
    # let "var=var+1"
    ((paramcount+=1))

    # Help checked one way...
    if echo $param | grep "^--help" > /dev/null 2> /dev/null; then
        ((IsHelp+=1))
        ((paramknown+=1))
    fi

    # Version checked another way...
    if [ $param = "--version" ]; then
        ((IsVersion+=1))
        ((paramknown+=1))
    fi
done

# Display the number of known parameters
if [ $paramcount = 0 ]; then
    echo "  "No parameters supplied...
else
    # Total number of parameters
    echo "  "Total parameters: $paramcount

    # Known parameters (the "elif [...]" can simply be "else", but demonstrates "else if")
    if [ $paramknown = 0 ]; then
        echo "  "Known parameters: None
    elif [ $paramknown -gt 0 ]; then
        echo "  "Known parameters: $paramknown
    fi
fi

# Now use the parameters (if any)
if [ $IsHelp -gt 0 ] || [ $IsVersion -gt 0 ]; then
    # Show help. The following are all equivalent:
        # if [ $IsHelp -gt 0 ]; then
        # if [ ${IsHelp} -gt 0 ]; then
        # if [ "$IsHelp" -gt 0 ]; then
    if [ $IsHelp -gt 0 ]; then
        echo; show_help
    fi

    if [ $IsVersion -gt 0 ]; then
        # Show version
        echo; show_version
    fi
fi
