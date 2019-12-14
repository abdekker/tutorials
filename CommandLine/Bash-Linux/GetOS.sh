#!/bin/bash
# Checks for some system details, like operating system
# Example: bash ./GetOS.sh

# It is common to have "interactive=TRUE" as the first line
interactive=TRUE
echo "### Advanced system details ###"
echo "  "Operating system
echo "  OS: "${OSTYPE}
if [[ "$OSTYPE" == "linux-gnu" ]]; then
    # Linux
    echo "    Linux"
elif [[ "$OSTYPE" == "darwin"* ]]; then
    # Mac OSX
    echo "    Mac OSX"
elif [[ "$OSTYPE" == "cygwin" ]]; then
    # POSIX compatibility layer and Linux environment emulation for Windows
    echo "    Linux emulation on Windows"
elif [[ "$OSTYPE" == "msys" ]]; then
    # Lightweight shell and GNU utilities compiled for Windows (part of MinGW)
    echo "    MinGW shell for Windows"
elif [[ "$OSTYPE" == "win32" ]]; then
    # Can this happen?
    echo "    Windows"
elif [[ "$OSTYPE" == "freebsd"* ]]; then
    # BSD
    echo "    BSD UNIX"
else
    # Unknown
    echo "    Unknown OS"
fi
echo .

echo All done!
