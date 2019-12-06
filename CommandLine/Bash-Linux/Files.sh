#!/bin/bash
# Demonstrates some basic file and directory commands
# Example: bash ./Files.sh

interactive=TRUE
echo Using files and folders in Bash
echo .

echo "### Listing files in a folder ###"
echo All files...
declare -i fileNum=1
for filename in ./*; do   # Or "for filename in *; do"
    filename="$(basename -- $filename)"
    echo "  "$fileNum") "$filename
    (( fileNum++ ))
done
echo .

echo Only .txt files...
fileNum=1
for filename in ./*.txt; do
    filename="$(basename -- $filename)"
    echo "  "$fileNum") "$filename
    (( fileNum++ ))
done
echo .

echo "### Convert from relative to absolute path ###"
for filename in ./*.sh; do
    echo "  "Relative path: $filename
    filename="$(cd "$(dirname "$filename")"; pwd)/$(basename "$filename")"
    echo "  "Absolute path: $filename
    break
done
echo .

echo "### Check for the existence of a file ###"
# Generate two strings:
# * The name of a script file in this folder (an existing file)
# * A randomised string (representing a file which does not exist)
declare -a myFiles=()
for filename in ./*.sh; do
    myFiles[0]="$(basename -- $filename)"
    break
done

sslPath=$(which openssl)
if [ -z $sslPath ]; then
    # "openssl" is not installed; assume the filename below does not exist
    myFiles[1]="quickredfox.xyz"
else
    # "openssl" is installed; generate a random string
    myFiles[1]=$(openssl rand -base64 32 | tr -d /=+ | cut -c -10)".xyz"
fi

for myFile in "${myFiles[@]}"; do
    if [ -r $myFile ]; then
        echo "  "File \"$myFile\" exists!
    else
        echo "  "File \"$myFile\" was not found
    fi
done
echo .

echo The end!
