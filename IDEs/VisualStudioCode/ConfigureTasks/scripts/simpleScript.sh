#!/bin/bash
# Demonstrates running a simple script from Visual Studio Code
# Note: A random string will be generated using "gpw". Installed with:
#   sudo apt install gpw

# Check whether gpw is installed...
gpwPath=$(which gpw)
if [ -z $gpwPath ]; then
  echo "  "Command \'gpw\' not found, but can be installed with:
  echo "  "sudo apt install gpw
  exit
fi

# Generate a random string using "gpw" (simpler, though less secure, than "pwgen")
RANDOM_STRING=$(gpw 1 15)   # 1 random string, 15-characters long, lowercase letters only

# Create a folder using this randomised string. This folder is created in the main project root.
mkdir $RANDOM_STRING
echo Folder $RANDOM_STRING create...
cd $RANDOM_STRING
xmessage -timeout 10 "Now in $RANDOM_STRING!"
echo Currently in \"$PWD\"
echo Folder will now be deleted...
cd ..
rm -rf $RANDOM_STRING
xmessage -timeout 2 "Folder deleted...bye!"
echo All done!
