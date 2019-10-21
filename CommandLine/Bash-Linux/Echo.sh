#!/bin/bash
# Demonstrates using the "echo" or "printf" commands
# Example: bash ./Echo.sh

# Brackets and other special characters
echo Brackets \(and other special characters\) need to be escaped
#echo Hello (world 1) from echo     # Error!
echo Hello '('world 2')' from echo  # Single quote around character
echo Hello '(world 3)' from echo    # Single quote around entire string
echo Hello "(world 4)" from echo    # Double quote around entire string
echo Hello \(world 5\) from echo    # Backslash character, like \n
echo Hello \$world 6\$ from echo    # $ is also a special character...
echo .

# Using printf
echo \"printf\" is a powerful alternative to \"echo\"
printf "Hello world from printf 1\n"
test=world
printf "Hello %s from printf 2\n" "$test"
