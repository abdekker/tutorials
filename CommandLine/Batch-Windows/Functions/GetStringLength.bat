@echo off
rem Function to calculate the length of a string
rem Usage: strlen MY_STRING MY_STRING_LENGTH
rem Example:
rem     set "MY_STRING=test"
rem     call :strlen MY_STRING MY_STRING_LENGTH
rem     echo "%MY_STRING%" is %MY_STRING_LENGTH% characters long
rem         > "test" is 4 characters long

:strlen
setlocal EnableDelayedExpansion
set LOCAL_LENGTH=0
:strlenLoop
if not "!%1:~%LOCAL_LENGTH%,1%!"=="" (
    set /a LOCAL_LENGTH+=1
    goto :strlenLoop
)
endlocal & set %2=%LOCAL_LENGTH%
exit /b 0
