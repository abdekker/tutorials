#!/bin/bash
# Demonstrates comparing numbers and strings
# Adapted from: https://www.tldp.org/LDP/abs/html/comparison-ops.html

# Strings
STR1="andrew"
STR2="fred"
STR3="fred"     # STR2 == STR3
STR4="Fred"
STR5="fred!"
STR6="jane"
STR7=""         # Empty

# Numbers (in this case integers)
INT1=-1
INT2=0
INT3=5
INT4=5          # INT3 == INT4
INT5=999

interactive=TRUE
echo Comparison operators in Bash. Version: $version
echo
echo Strings:
echo STR1: $STR1
echo STR2="fred"
echo STR3="fred"     # STR2 == STR3echo
echo STR4="Fred"
echo STR5="fred!"
echo STR6="jane"
echo STR7=""         # Empty
