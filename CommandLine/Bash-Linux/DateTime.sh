#!/bin/bash
# Demonstrates using date and time in bash
# Example: bash ./DateTime.sh

interactive=TRUE
echo "### Date / Time in bash ###"

# Basic ISO 8601 date/time strings
DateTimeISO8601=$(date +"%Y-%m-%dT%T")
DateISO8601=$(date +"%Y-%m-%d")
echo "  Date/time (ISO 8601 format):    "$DateTimeISO8601
echo "  Date (ISO 8601 format):         "$DateISO8601
echo .

# Manipulate the date (in this case year)
declare -i Year=$(date +"%Y")
((Year--))
OneYearAgo_v1=$(date +"$Year-%m-%d")
OneYearAgo_v2=$Year"-"$(date +"%m-%d")  # Same result, but slightly more elegant
echo "  Date (one year ago):            "$OneYearAgo_v1"  "\(1\)
echo "  Date (one year ago):            "$OneYearAgo_v2"  "\(2\)
echo .

echo "The end!"
