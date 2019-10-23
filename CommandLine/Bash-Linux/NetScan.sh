#!/bin/bash
# Demonstrates scanning the wireless or ethernet interface of this computer
# Adapted from a Bash script supplied by Matt Holzel
# Example: bash ./NetScan.sh

# Typically, you would call this with either "wireless" or "ethernet", that is:
#
# $ scan.sh wireless
# $ scan.sh ethernet

echo Parameter count=$#
if [ $# -eq 0 ]; then
    DEVICE_TYPE=wireless    # No parameter => wireless
else
    DEVICE_TYPE=$1          # Use parameter
fi

echo Device to scan: $DEVICE_TYPE
NAME_STRING=$(sudo lshw -class network | grep -A 5 -i "description: ${DEVICE_TYPE}" | grep "logical name: ")
SPLIT=(${NAME_STRING// / })
DEVICE=${SPLIT[2]}

echo "scanning ${DEVICE}"

# If required, install with "sudo apt install arp-scan"
sudo arp-scan -I ${DEVICE} --localnet
