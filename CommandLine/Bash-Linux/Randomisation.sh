#!/bin/bash
# Generate random strings and numbers in Bash
# Example: bash ./Randomisation.sh
# Adapted from: https://unix.stackexchange.com/questions/230673

# Constants (can also be defined with "readonly VAR=VALUE")
declare -i DEFAULT_PWD_LENGTH=10

# Functions
funcRandomPWGEN()   # Can also be "function NAME()"
{
  # Generate a random string using the "pwgen" password generator
  # From the command-line

  # Set password length
  pwgenLength=$1
  if [ -z "$1" ]; then
    pwgenLength=$DEFAULT_PWD_LENGTH
  fi

  # Exclude special characters? Leave blank to include all special characters.
  if [ -n "$2" ]; then
    RAND_PWGEN=$(pwgen -sy $pwgenLength -r {$"$2"})
  else
    RAND_PWGEN=$(pwgen -sy $pwgenLength)
  fi
}

interactive=TRUE
echo "### Randomisation in bash scripts ###"
echo

echo Using \"pwgen\" \(secure password generator\)
# Check whether pwgen is installed...
pwgenPath=$(which pwgen)
if [ -z $pwgenPath ]; then
    echo "  "Command \'pwgen\' not found, but can be installed with:
    echo "  "sudo apt install pwgen
    exit
fi

# Is the environment variable $PWGEN_SPECIAL declared?
declare -i isStdExcludeSpecial=1
if [ -z $PWGEN_SPECIAL1 ]; then
  echo "  "Missing environment variable \"PWGEN_SPECIAL\" in .bashrc
  echo "  "Recommend \"export PWGEN_SPECIAL=\'\"\@\?\^\&\*\(\)\`\:\~\?\;\:\[\]\{\}\.\,\\\/\|\"
  PWGEN_SPECIAL_TMP=\'\"\@\?\^\&\*\(\)\`\:\~\?\;\:\[\]\{\}\.\,\\\/\|
  isStdExcludeSpecial=0
fi

# Note: To include all special characters, use "funcRandomPWGEN LENGTH".
# Note: In general, this cannot be used to create a folder in Linux.
if [ $isStdExcludeSpecial -gt 0 ]; then
  funcRandomPWGEN $DEFAULT_PWD_LENGTH $PWGEN_SPECIAL
else
  funcRandomPWGEN $DEFAULT_PWD_LENGTH $PWGEN_SPECIAL_TMP
fi
echo Generated $DEFAULT_PWD_LENGTH-char string = $RAND_PWGEN \(Could use eg. to create a file or folder with \"touch\" or \"mkdir\"\)
# Note: Since special characters have been excluded, this can be used to generate a file or folder in Linux
echo .
