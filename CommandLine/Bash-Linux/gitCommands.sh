#!/bin/bash
# Some examples of using git from the command-line
# To use, place this script inside a git repo (can be the root where the .git folder is or anywhere else)
# Example: bash ./gitCommands.sh
#ADAD, this line is a test

getGitStats()
{
    # Some statistics about your git repository
    echo "  "git commit statistics
    gitRepoName=$(basename `git rev-parse --show-toplevel`)
    echo "    Repository name:      "$gitRepoName

    # The starting date/time is provided as a parameter
    beginTime=$1

    # Analyse the git log and save as a variable
    gitLog=$(git log --shortstat --since="$beginTime")

    # Commits, insertions and deletions
    gitCommits=$(grep -c commit <<< "$gitLog")
    gitInserted=$(grep insertions <<< "$gitLog" | cut -d "," -f2 | sed 's/^[ \t]*//' | cut -d" " -f1 | paste -sd+ | tr -s "+" | bc)
    gitDeleted=$(grep deletions <<< "$gitLog"   | cut -d "," -f3 | sed 's/^[ \t]*//' | cut -d" " -f1 | paste -sd+ | tr -s "+" | bc)

    # Show the results
    echo "    Commits since:        "$beginTime
    echo "    Number of commits:    "$gitCommits
    echo "    Inserted lines:       "$gitInserted
    echo "    Deleted lines:        "$gitDeleted

    # Note,1: An early version of this script used the code below
    #echo "Number of commits: "$(git log --shortstat --since="$beginTime" | grep commit | wc -l)
    #echo "Inserted lines: "$(git log --shortstat --since="$beginTime" | grep insertions | cut -d "," -f2 | sed 's/^[ \t]*//' | cut -d" " -f1 | paste -sd+ | tr -s "+" | bc)
    #echo "Deleted lines:  "$(git log --shortstat --since="$beginTime" | grep deletions  | cut -d "," -f3 | sed 's/^[ \t]*//' | cut -d" " -f1 | paste -sd+ | tr -s "+" | bc)

    # Note,2: "paste" causes errors on MacOS, and the following can be used (see GetOS.sh for further details)
    #if [[ "$OSTYPE" == "linux-gnu" ]]; then    # To test on Linux (ensure the next line is commented out)
    if [[ "$OSTYPE" == "darwin"* ]]; then
        # Mac OSX
        echo "  (Statistics on MacOS)"
        gitInserted2=$(sum=0;
            for i in $(grep insertions <<< "$gitLog" | cut -d "," -f2 | sed 's/^[ \t]*//' | cut -d " " -f1); do
                sum=$((sum + ${i}));
            done;
            echo "${sum}")
        gitDeleted2=$(sum=0;
            for i in $(grep deletions <<< "$gitLog" | cut -d "," -f3 | sed 's/^[ \t]*//' | cut -d " " -f1); do
                sum=$((sum+${i}));
            done;
            echo "${sum}")
        echo "    Inserted lines:       "$gitInserted2
        echo "    Deleted lines:        "$gitDeleted2
    fi
}

# It is common to have "interactive=TRUE" as the first line
echo "Current shell options: "$-" (before)"
interactive=TRUE
echo "Current shell options: "$-" (after)"
echo .

# To check whether the script is interactive use this:
# [[ $- == *i* ]] && echo "Shell Interactive" || echo "Shell Not interactive"

# To ensure the script is interactive: # [[ $- != *i* ]] && return

# First check that we are in a git repository
IsGitRepo="$(git rev-parse --is-inside-work-tree 2> /dev/null)"
if [ -z "$IsGitRepo" ]; then
    echo Please run this script from inside a git repository...
    exit
fi

# We're in a repository!
echo "### Advanced git commands from bash ###"

# Command-line parameters (if any, these should be a list of dates in YYYY-MM-DD format)
declare -i ParamCount=$#
echo "  Parameter count: "${ParamCount}
echo .

if [ ${ParamCount} -gt 0 ]; then
    # One or more parameters were provided...
    Dates="$@"
    declare -i dateNum=0
    for localDate in `echo "${Dates}" | awk -v RS=, '{print}'`; do
        ((dateNum++))
        echo "  Date "$dateNum": "${localDate}
        getGitStats ${localDate}
        echo
    done
else
    # No parameters (dates) provided
    
    # Use a fixed date (change as appropriate)
    echo "  "Using a fixed date
    getGitStats "2019-11-01"
    echo

    # Also show the statistics for exactly one year
    echo "  "Statistics for the last year
    declare -i Year=$(date +"%Y")
    ((Year--))
    getGitStats "$(date +"$Year-%m-%d")"
fi
echo .

echo All done!
