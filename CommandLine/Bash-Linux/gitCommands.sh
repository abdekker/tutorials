#!/bin/bash
# Some examples of using git from the command-line
# To use, place this script inside a git repo (can be the root where the .git folder is or anywhere else)
# Example: bash ./gitCommands.sh

getGitStats()
{
    # Some statistics about your git repository
    echo "  "Basic git statistics
    gitRepoName=$(basename `git rev-parse --show-toplevel`)
    echo "    Repository name:      "$gitRepoName
    echo

    # Change the time or provide a new starting date/time
    echo "  "Using a fixed date
    begintime="2019-11-01"

    # Analyse the git log and save as a variable
    gitLog=$(git log --shortstat --since="$begintime")

    # Commits, insertions and deletions
    gitCommits=$(grep -c commit <<< "$gitLog")
    gitInserted=$(grep insertions <<< "$gitLog" | cut -d "," -f2 | sed 's/^[ \t]*//' | cut -d" " -f1 | paste -sd+ | tr -s "+" | bc)
    gitDeleted=$(grep deletions <<< "$gitLog"   | cut -d "," -f3 | sed 's/^[ \t]*//' | cut -d" " -f1 | paste -sd+ | tr -s "+" | bc)

    # Show the results
    echo "    Commits since:        "$begintime
    echo "    Number of commits:    "$gitCommits
    echo "    Inserted lines:       "$gitInserted
    echo "    Deleted lines:        "$gitDeleted
    echo

    # Note: An early version of this script used the code below
    #echo "Number of commits: "$(git log --shortstat --since="$begintime" | grep commit | wc -l)
    #echo "Inserted lines: "$(git log --shortstat --since="$begintime" | grep insertions | cut -d "," -f2 | sed 's/^[ \t]*//' | cut -d" " -f1 | paste -sd+ | tr -s "+" | bc)
    #echo "Deleted lines:  "$(git log --shortstat --since="$begintime" | grep deletions  | cut -d "," -f3 | sed 's/^[ \t]*//' | cut -d" " -f1 | paste -sd+ | tr -s "+" | bc)

    # Now update the date to show statistics for exactly one year
    echo "  "Statistics for the last year
    declare -i Year=$(date +"%Y")
    ((Year--))
    begintime=$(date +"$Year-%m-%d")

    gitLog=$(git log --shortstat --since="$begintime")

    gitCommits=$(grep -c commit <<< "$gitLog")
    gitInserted=$(grep insertions <<< "$gitLog" | cut -d "," -f2 | sed 's/^[ \t]*//' | cut -d" " -f1 | paste -sd+ | tr -s "+" | bc)
    gitDeleted=$(grep deletions <<< "$gitLog"   | cut -d "," -f3 | sed 's/^[ \t]*//' | cut -d" " -f1 | paste -sd+ | tr -s "+" | bc)
    
    echo "    Commits since:        "$begintime
    echo "    Number of commits:    "$gitCommits
    echo "    Inserted lines:       "$gitInserted
    echo "    Deleted lines:        "$gitDeleted
}

interactive=TRUE
# First check that we are in a git repository
IsGitRepo="$(git rev-parse --is-inside-work-tree 2> /dev/null)"
if [ -z "$IsGitRepo" ]; then
    echo Please run this script from inside a git repository...
    exit
fi

# We're in a repository!
echo "### Advanced git commands from bash ###"

# Basic git statistics
getGitStats
echo .

echo All done!
