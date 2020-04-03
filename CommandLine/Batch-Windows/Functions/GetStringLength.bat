@echo off
setlocal EnableDelayedExpansion

rem Function to calculate the length of a string
rem Usage: GetStringLength MY_STRING MY_STRING_LENGTH
rem Example:
rem     set "MY_STRING=test"
rem     call :GetStringLength MY_STRING MY_STRING_LENGTH
rem     echo "%MY_STRING%" is %MY_STRING_LENGTH% characters long
rem         > "test" is 4 characters long

:GetStringLength
set LOCAL_LENGTH=0
:StringLengthLoop
if not "!%1:~%LOCAL_LENGTH%,1%!"=="" (
    set /a LOCAL_LENGTH+=1
    goto :StringLengthLoop
)
set %2=%LENGTH%
exit /b 0
