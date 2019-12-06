#!/bin/bash
# Generate random strings and numbers in Bash
# Example: bash ./Randomisation.sh
# Adapted from: https://unix.stackexchange.com/questions/230673

# Constants can also be defined with "readonly VAR=VALUE"
declare -i DEFAULT_PWD_LENGTH=10

# Functions
funcPwgenSecure()   # Can also be "function NAME()"
{
    # Generate a random string using the "pwgen" password generator

    # 1st parameter: Password length
    pwgenLength=$1
    if [ -z "$1" ]; then
        pwgenLength=$DEFAULT_PWD_LENGTH
    fi

    # 2nd parameter: Characters to exclude. Leave blank to include all characters.
    # Note: Typical use is to exclude special characters that might, for example, prevent
    # the generated string being used to create a file or folder on Linux.
    if [ -n "$2" ]; then
        FUNC_RETURN=$(pwgen -sy -r {$"$2"} $pwgenLength 1)
    else
        FUNC_RETURN=$(pwgen -sy $pwgenLength 1)
    fi
}

funcRandomGPW()
{
    # Generate a random string using "gpw" (simpler, though less secure, than "pwgen")

    # 1st parameter: Password length
    pwgenLength=$1
    if [ -z "$1" ]; then
        pwgenLength=$DEFAULT_PWD_LENGTH
    fi

    # To generate multiple strings, change the 1st parameter to "gpw" (eg. 5)
    FUNC_RETURN=$(gpw 1 $pwgenLength)
}

interactive=TRUE
echo "### Randomisation in bash scripts ###"
echo Note: String examples are $DEFAULT_PWD_LENGTH characters long
echo

echo Using \"pwgen\" \(password generator, secure usage with \"-y\"\)
echo "  "Can generate a variety of complex or simple passwrd \(but must be installed\)
# Check whether pwgen is installed...usually not
pwgenPath=$(which pwgen)
if [ -z $pwgenPath ]; then
    echo "  "Command \'pwgen\' not found, but can be installed with:
    echo "  "sudo apt install pwgen
    exit
fi

# Is the environment variable $PWGEN_SPECIAL declared?
declare -i isStdExcludeSpecial=1
if [ -z $PWGEN_SPECIAL ]; then
    echo "  "Missing environment variable \"PWGEN_SPECIAL\" in .bashrc
    echo "  "Recommend \"export PWGEN_SPECIAL=\'\"\@\?\^\&\*\(\)\`\:\~\?\;\:\[\]\{\}\.\,\\\/\|\"
    PWGEN_SPECIAL_TMP=\'\"\@\?\^\&\*\(\)\`\:\~\?\;\:\[\]\{\}\.\,\\\/\|
    isStdExcludeSpecial=0
fi

# Note: To include all special characters, use "funcPwgenSecure LENGTH" (no 2nd parameter).
if [ $isStdExcludeSpecial -gt 0 ]; then
    funcPwgenSecure $DEFAULT_PWD_LENGTH $PWGEN_SPECIAL
else
    funcPwgenSecure $DEFAULT_PWD_LENGTH $PWGEN_SPECIAL_TMP
fi
RAND_PWGEN=$FUNC_RETURN
echo "  "$DEFAULT_PWD_LENGTH-char string \(Limit specials"   "\)  = $RAND_PWGEN \(Could use eg. to create a file or folder with \"touch\" or \"mkdir\"\)

funcPwgenSecure $DEFAULT_PWD_LENGTH
RAND_PWGEN=$FUNC_RETURN
echo "  "$DEFAULT_PWD_LENGTH-char string \(All special chars\) = $RAND_PWGEN \(User password, no guarantee to successfully create file or folder\)
# Note: If special characters are excluded, this can be used to generate a file or folder in Linux
echo .

echo Using \"pwgen\" \(additional examples, less secure usage\)
RAND_PWGEN=$(pwgen $DEFAULT_PWD_LENGTH 1)
echo "  "pwgen $DEFAULT_PWD_LENGTH 1"      " = $RAND_PWGEN \(upper, lower, numbers\)
RAND_PWGEN=$(pwgen -c $DEFAULT_PWD_LENGTH 1)
echo "  "pwgen -c $DEFAULT_PWD_LENGTH 1"   " = $RAND_PWGEN \(at least one upper, lower, numbers\)
RAND_PWGEN=$(pwgen -A $DEFAULT_PWD_LENGTH 1)
echo "  "pwgen -A $DEFAULT_PWD_LENGTH 1"   " = $RAND_PWGEN \(no upper, lower, numbers\)
RAND_PWGEN=$(pwgen -n $DEFAULT_PWD_LENGTH 1)
echo "  "pwgen -n $DEFAULT_PWD_LENGTH 1"   " = $RAND_PWGEN \(upper, lower, at least one number\)
RAND_PWGEN=$(pwgen -0 $DEFAULT_PWD_LENGTH 1)
echo "  "pwgen -0 $DEFAULT_PWD_LENGTH 1"   " = $RAND_PWGEN \(upper, lower, no numbers\)
echo .

echo Using \"gpw\" \(simple pronounceable password generator\)
echo "  "Can be used to create a file or folder \(but must be installed\)
# Check whether gpw is installed...usually not
gpwPath=$(which gpw)
if [ -z $gpwPath ]; then
    echo "  "Command \'gpw\' not found, but can be installed with:
    echo "  "sudo apt install gpw
    exit
fi

funcRandomGPW $DEFAULT_PWD_LENGTH
RAND_GPW=$FUNC_RETURN
echo "  "$DEFAULT_PWD_LENGTH-char string = $RAND_GPW \(lowercase only\)
echo .

echo Using \"openssl\" \(software library for online encryption - best method\)
echo "  "Can be used to create a file or folder \(generally installed\)
# Check whether openssl is installed...usually will be
sslPath=$(which openssl)
if [ -z $sslPath ]; then
    echo "  "Command \'openssl\' not found, but can be installed with:
    echo "  "sudo apt install openssl
    exit
fi

# Strings generated with "openssl rand -hex X" are 2X in length...
declare -i DEFAULT_LENGTH_SSL
let DEFAULT_LENGTH_SSL=(DEFAULT_PWD_LENGTH/2)
RAND_SSL=$(openssl rand -hex $DEFAULT_LENGTH_SSL)
echo "  "$DEFAULT_PWD_LENGTH-char string = $RAND_SSL \(lowercase, numbers only\)

# Strings generated with "openssl rand -base64 X" may contain the special characters "/=+"
let DEFAULT_LENGTH_SSL=(DEFAULT_PWD_LENGTH/2)
RAND_SSL=$(openssl rand -base64 32 | tr -d /=+ | cut -c -$DEFAULT_PWD_LENGTH)
echo "  "$DEFAULT_PWD_LENGTH-char string = $RAND_SSL \(alphanumeric, no special characters\)
echo ""
echo .

echo The end!
