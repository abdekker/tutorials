#!/bin/bash
# Demonstrates variable declaration (such as using "declare")
# Example: bash ./Variables.sh

func1 ()
{
  echo Hello from func1
}

func2 ()
{
    declare -i paramcount=0     # Declare as integer, but simply "paramcount=0" also works
    declare -i paramknown=0
    for param in "$@"; do
        # Alternatives for incrementing an integer include:
        # ((var=var+1))
        # let "var=var+1"
        ((paramcount+=1))

        # "Help" checked one way...
        if echo $param | grep "^--help" > /dev/null 2> /dev/null; then
            show_help
            ((paramknown+=1))
        fi

        # "Version" checked another way...
        if [ $param = "--version" ]; then
            show_version
            ((paramknown+=1))
        fi
    done
  echo Hello from func2: $1
}

interactive=TRUE
echo Variable declarations in Bash
echo

echo "### Read-only ###"
declare -r VAR_READONLY=1
echo Attempt to modify the variable "=>" error!
(( VAR_READONLY++ ))        # error!
echo "###"; echo

echo "### Integer ###"
declare -i VAR_NUM1         # VAR_NUM1 defined as an integer
VAR_NUM1=3
VAR_NUM2=3
echo \(Using declare\) INT1 = $VAR_NUM1
echo \(Without declare\) INT2 = $VAR_NUM2
echo .; echo "  "Increment using "(( VAR++ ))". Whitespsace is ignored, so "((VAR++))" also works.
(( VAR_NUM1++ ))            # works
((VAR_NUM2++))              # works
echo INT1 = $VAR_NUM1, INT2 = $VAR_NUM2 "(works as expected)"
echo .; echo "  "Use mathematical operator to double value "(( VAR*=2 ))"
VAR_NUM2=$VAR_NUM1
(( VAR_NUM1*=2 ))           # works
(( VAR_NUM2*=2 ))           # works
echo INT1 = $VAR_NUM1, INT2 = $VAR_NUM2 "(works as expected)"
echo .; echo "  "Increment using "let VAR=VAR+1"
let VAR_NUM1=VAR_NUM1+1     # works
let VAR_NUM2=VAR_NUM2+1     # works
echo INT1 = $VAR_NUM1, INT2 = $VAR_NUM2 "(works as expected)"
echo .; echo "  "Now increment using "VAR=VAR+1"
VAR_NUM1=VAR_NUM1+1         # works
VAR_NUM2=VAR_NUM2+1         # doesn't work as expected (converted to string); VAR_NUM2 not defined as an integer
echo INT1 = $VAR_NUM1, INT2 = $VAR_NUM2 "(oops! INT2 converted to string)"
echo .; echo "  "Attempting to change integer to float \(VAR=1.1\) "=>" error!
if [ -z $BASH_SOURCE ]; then
    # The next line crashes if debugging (eg. in VS Code), hence the check for $BASH_SOURCE
    VAR_NUM1=123.4          # error!
fi
echo INT1 = $VAR_NUM1 "(no change)"
echo "###"; echo

echo "### Arrays 1 ###"
echo "  "Using \"ARRAY[xx]\" notation. Elements do not need to be contiguous.
area[11]=23
area[13]=37
area[51]=UFOs
echo "area[11] = ${area[11]}"
echo "area[13] = ${area[13]}"
echo "area[51] = ${area[51]}"
echo "area[12] = ${area[12]} (uninitialised elements are blank)"
echo .; echo "  "Assign sum of two members to a third
area[5]=`expr ${area[11]} + ${area[13]}`
echo "area[5] = area[11] + area[13] = ${area[5]}"
echo .; echo "  "Initialisation with differing types fails
area[6]=`expr ${area[11]} + ${area[51]}`
echo "area[6] = area[11] + area[51] = ${area[6]}"
echo "###"; echo .

echo "### Arrays 2 ###"
echo "  "Arrays can also be declared using \"declare -a ARRAY=\(X Y Z\)\"
echo "  "Elements are accesed using \"ARRAY[xx]\" notation
echo "  "This example searches for files with specified extensions defined in an array
declare -i totalFiles=0
declare -a exts=("sh" "txt")  # declare -a exts=(sh txt) also works
echo "    "exts = \""${exts[*]}"\" and has "${#exts[*]}" elements
for ext in "${exts[@]}"
do
    shopt -s nullglob
    files=(./*.$ext)
    echo "    "${#files[@]} $ext files detected
    (( totalFiles+=${#files[@]} ))
done
echo "    "There are $totalFiles files in total; echo .

echo "  "Arrays can also contain numbers
declare -a nums=(1 3 5)
declare -i myNum=nums[1]
echo "    "The second element of \""${nums[*]}"\" is $myNum; echo .

echo "### Export variable for use outside this script ###"
echo Use "declare -x VAR"
echo "###"; echo .

echo "### List functions defined in this script ###"
# Warning: This dumps the *entire* contents of all functions!
declare -f
echo "  "Now call the function using \"FUNC-NAME\" \(note no brackets\)
func1
func2 fred
echo "###"; echo .
echo "The end!"
