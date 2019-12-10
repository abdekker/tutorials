#!/bin/bash
# Demonstrates checking parameters passed to the script
# Example: bash ./Parameters.sh --help --unknown
version="1.0.23

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
echo "### Parameters in bash scripts ###"
echo The \"\$\#\" environment variable is the number of parameters passed to this script
declare -i RawCount=$#
echo "  "Raw count = $RawCount
if [ "$RawCount" -gt 0 ]; then
    echo "  "First parameter is \"$1\"
fi
echo .

echo Other ways to check if there are parameters include:
echo "  1) "if [ -n \"\$1\" ] \(Is the first parameter NOT null?\)
if [ -n "$1" ]; then
    echo "    "First parameter is \"$1\" \(1\)
else
    echo "    "No parameters found \(1\)
fi
echo "  2) "if [ -z \"\$1\" ] \(Is the first parameter empty?\)
if [ -z "$1" ]; then
    echo "    "No parameters found \(2\)
else
    echo "    "First parameter is \"$1\" \(2\)
fi
echo .

echo To loop through the parameters, use \"for param in \"\$\@\"\; do\"
echo Note: This script understands the concept of \"tags\" using \"-t TAG\"
declare -i nextParamIsTag=0
declare -i numTags=0
declare -a tagsProvided=( )
if [ -n "$1" ]; then
    for param in "$@"; do
        if [ $param = "--help" ]; then
            # Display help?
            echo "  "Help!
        elif [ $param = "--save" ]; then
            # Save something to disk?
            echo "  "Save!
        elif [ $param = "--version" ]; then
            # Show the version of this script
            echo "  "Version!
        elif [ $param = "-t" ]; then
            # The next parameter is a tag!
            ((nextParamIsTag++))
            ((numTags++))
        else
            if [ $nextParamIsTag -gt 0 ]; then
                # This is a "tag"
                nextParamIsTag=0
                tagsProvided[$numTags]=$param
            else
                # Unknown parameter!
                echo Parameter \"$param\" not recognised...
            fi
        fi
    done
fi

# Build any tags received into a string. Tags should be in the format "-t TAG".
if [ $numTags -gt 0 ]; then
    listOfTags=""
    declare -i tagPos=0
    for tag in "${tagsProvided[@]}"; do
        # Build the tags, received as parameters in the form "-t TAG", into
        ((tagPos++))
        listOfTags+="Tag"$tagPos" = $tag"
        if [ $tagPos -lt $numTags ]; then
            listOfTags+=", "
        fi
    done

    # Display the list of tags to the user
    echo "  Tags received! "$listOfTags
fi
echo .

# Known parameters are "--help" and "--version". Update as appropriate.
echo Processing known parameters \(eg. \"--help\"\)
echo "  "Note: Parameters are being counted using \"for param in \"\$@\"\; do
declare -i paramCount=0     # Declare as integer, but "paramCount=0" also works
declare -i paramKnown=0
declare -i IsHelp=0
declare -i IsVersion=0
for param in "$@"; do
    # Alternatives for incrementing an integer include:
    # ((paramCount+=1)), (( paramCount+=1 )), ((paramCount += 1)) or ((param
    # ((var=var+1))
    # let "var=var+1"
    ((paramCount+=1))

    # Help checked one way...
    if echo $param | grep "^--help" > /dev/null 2> /dev/null; then
        ((IsHelp+=1))
        ((paramKnown+=1))
    fi

    # Version checked another way...
    if [ $param = "--version" ]; then
        ((IsVersion+=1))
        ((paramKnown+=1))
    fi
done

# Display the number of known parameters
if [ $paramCount = 0 ]; then
    echo "  "No parameters supplied...
else
    # Total number of parameters
    echo "  "Total parameters: $paramCount

    # Known parameters (the "elif [...]" can simply be "else", but demonstrates "else if")
    if [ $paramKnown = 0 ]; then
        echo "  "Known parameters: None
    elif [ $paramKnown -gt 0 ]; then
        echo "  "Known parameters: $paramKnown
    fi
fi

# Now use the parameters (if any)
if [ $IsHelp -gt 0 ] || [ $IsVersion -gt 0 ]; then
    # The following are all equivalent:
        # if [ $IsHelp -gt 0 ]; then
        # if [ ${IsHelp} -gt 0 ]; then
        # if [ "$IsHelp" -gt 0 ]; then
        # if (($IsHelp > 0)); then
    if [ $IsHelp -gt 0 ]; then
        # Show help
        echo; show_help
    fi

    if [ $IsVersion -gt 0 ]; then
        # Show version
        echo; show_version
    fi
fi
echo .

echo All done!
