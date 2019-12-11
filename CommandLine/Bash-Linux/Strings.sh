#!/bin/bash
# Some usage of strings
# Example: bash ./Strings.sh
# Adapted from: https://www.tldp.org/LDP/abs/html/string-manipulation.html

interactive=TRUE
echo "### Strings and string manipulation in bash scripts ###"
STR1="Hello World!"
STR2=orl
STR3=$STR1","$STR2  # Concatenatation with comma separator; use "STR3=$STR1$STR2" for no comma
echo STR1 = $STR1
echo STR2 = $STR2
echo STR3 = $STR3
echo .

echo "# String length #"
echo "  STR1: "${#STR1} \(1\)
tmp=`expr length "$STR1"`
echo "  STR1: "$tmp \(2\)
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
echo "  "Our array has $numStrings elements
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
    echo "  "List of strings: $listOfStrings
fi
echo .

echo All done!
