@echo off
rem setlocal EnableDelayedExpansion

rem This script demonstrate using strings in DOS scripting

echo ### Basic construction of strings###
echo   Construct a string with:
echo      set STR="VALUE"
echo      set "STR=VALUE" (recommended)
echo   Concatenate strings with:
echo      set STR3=%%STR1%%%%STR2%%
set STR1=Hello
set STR2=World!
set STR3=%STR1% %STR2%
echo   1 = %STR1%, 2 = %STR2%, 3 = %STR3%
echo .

echo ### Basic comparisons on strings ###
set String_abc="abc"
set String_ABC1="ABC"
set String_ABC2="ABC"
set String_xyz="xyz"
echo   1 = %String_abc%, 2 = %String_ABC1%, 3 = %String_ABC2%, 4 = %String_xyz%
echo .
echo   Basic string comparison operators are LSS, EQU, NEQ, LEQ, GTR, GEQ
echo   Demonstrate the EQU (equals) operator (see .\ArithmeticOperations.bat for additional examples)
call :StringsEQU_v1 %String_abc% %String_ABC1%
call :StringsEQU_v1 %String_ABC1% %String_abc%
call :StringsEQU_v1 %String_ABC1% %String_ABC2%
call :StringsEQU_v1 %String_xyz% %String_abc%
echo .
echo   Also test for string equality with "=="
call :StringsEQU_v2 %String_ABC1% %String_ABC2%
echo .
goto StringComparisonOperatorsEnd

:StringsEQU_v1
rem Using the "EQU" operator
if %1 EQU %2 (echo     %1 is equal to %2) else (echo     %1 is NOT equal to %2)
exit /b 0

:StringsEQU_v2
rem Using the "==" operator
if %1 == %2 (echo     %1 is equal to %2) else (echo     %1 is NOT equal to %2)
exit /b 0

:StringComparisonOperatorsEnd
echo ### Extract characters from a string (0-based) ###
set "MY_STRING=Hello World"
echo   String is "%MY_STRING%"
echo   Character 2:       %MY_STRING:~1,1%       (using "%%STR:~1,1%%")
echo   Characters 0 to 3: %MY_STRING:~0,4%    (using "%%STR:~0,4%%")
echo   Characters 3 to 8: %MY_STRING:~2,6%  (using "%%STR:~2,6%%")
echo .

echo ### Calculate the length of a string ###
echo   There is no "strlen" equivalent in DOS scripting, so let's create one...
call :strlen MY_STRING MY_STRING_LENGTH
echo   "%MY_STRING%" is %MY_STRING_LENGTH% characters long
echo .
goto StringsEnd

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

:StringsEnd
echo All done!
