#!/bin/bash
# Using strings in bash
# Example: bash ./Strings.sh
# Adapted from: https://www.tldp.org/LDP/abs/html/string-manipulation.html

funcCheckEmpty()
{
    # Various ways to check for an empty string
    # Note: $STR1 should not be empty and $STR4 should be empty
    echo "  1) "if [ -n \"\$STR\" ] \(i.e. is the string NOT null?\)
    if [ -n "$STR1" ]; then
        echo "    "STR1 is not empty \(expected\)
    else
        echo "    "STR1 is empty \(??\)
    fi

    if [ -n "$STR4" ]; then
        echo "    "STR4 is not empty \(??\)
    else
        echo "    "STR4 is empty \(expected\)
    fi
    echo

    echo "  2) "if [ -z \"\$STR\" ] \(i.e. is the string empty?\)
    if [ -z "$STR1" ]; then
        echo "    "STR1 is empty \(??\)
    else
        echo "    "STR1 is not empty \(expected\)
    fi

    if [ -z "$STR4" ]; then
        echo "    "STR4 is empty \(expected\)
    else
        echo "    "STR4 is not empty \(??\)
    fi
    echo

    echo "  3) "if [ \${\#STR} -gt 0 ] \(i.e. does the string have non-zero length?\)
    if [ ${#STR1} -gt 0 ]; then
        echo "    "STR1 is not empty \(expected\)
    else
        echo "    "STR1 is empty \(??\)
    fi

    if [ ${#STR4} -gt 0 ]; then
        echo "    "STR4 is not empty \(??\)
    else
        echo "    "STR4 is empty \(expected\)
    fi
}

interactive=TRUE
echo "### Strings and string manipulation in bash scripts ###"
STR1="Hello World!"
STR2=orl
STR3=$STR1","$STR2  # Concatenatation with comma separator; use "STR3=$STR1$STR2" for no comma
STR4=""             # Empty
echo STR1 = $STR1
echo STR2 = $STR2
echo STR3 = $STR3
echo STR4 = $STR4 \(Empty\)
echo .

echo "# Check for empty strings #"
funcCheckEmpty
echo .

echo "# String length #"
echo "  STR1: "${#STR1} \(1\)
tmp=`expr length "$STR1"`
echo "  STR1: "$tmp \(2\)
echo "  STR4: "${#STR4}
echo .

echo "# Characters from string #"
echo Using string index \(0-based\) eg. \$\{STR:0:1\}
echo "  "${STR1:0:1}                    # H
echo "  "${STR1:2:5}                    # llo W
echo Using \"expr substr\" \(1-based\) eg. \$\(expr substr \"\$STR\" 1 1\)
echo "  "$(expr substr "$STR1" 1 1)     # H
echo "  "$(expr substr "$STR1" 3 5)     # llo W
echo .

echo "# Array of strings #"
declare -a myStrings=( "string1" "num2" "last" )
declare -i numStrings=${#myStrings[@]}
echo "  "Array has $numStrings elements
if [ $numStrings -gt 0 ]; then
    listOfStrings=""
    declare -i stringPos=0
    for str in "${myStrings[@]}"; do
        ((stringPos++))
        listOfStrings+=$str
        if [ $stringPos -lt $numStrings ]; then
            listOfStrings+=", "
        fi
    done

    # Display the list of tags to the user
    echo "  "Elements listed: $listOfStrings
fi
echo .

echo All done!
