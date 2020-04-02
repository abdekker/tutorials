@echo off
setlocal EnableDelayedExpansion

rem This script aims to demonstrate strings in DOS scripting

echo ### Basic comparisons on strings ###
set String_abc="abc"
set String_ABC1="ABC"
set String_ABC2="ABC"
set String_xyz="xyz"
echo 1 = %String_abc%, 2 = %String_ABC1%, 3 = %String_ABC2%, 4 = %String_xyz%
echo .
echo   Basic string comparison operators are LSS, EQU, NEQ, LEQ, GTR, GEQ
echo   Demonstrate the EQU (equals) operator (see .\ArithmeticOperations.bat for additional examples)
call :StringsEQU_v1 %String_abc% %String_ABC1%
call :StringsEQU_v1 %String_ABC1% %String_abc%
call :StringsEQU_v1 %String_ABC1% %String_ABC2%
call :StringsEQU_v1 %String_xyz% %String_abc%
echo .
echo   Can also test for string equality with "=="
call :StringsEQU_v2 %String_ABC1% %String_ABC2%
echo .
goto StringsOperatorsEnd

:StringsEQU_v1
rem Using the "EQU" operator
if %1 EQU %2 (echo     %1 is equal to %2) else (echo     %1 is NOT equal to %2)
exit /b 0

:StringsEQU_v2
rem Using the "==" operator
if %1 == %2 (echo     %1 is equal to %2) else (echo     %1 is NOT equal to %2)
exit /b 0

:StringsOperatorsEnd
echo ### Extract characters from a string (0-based) ###
endlocal
set "MY_STRING=Hello World"
echo   String is "%MY_STRING%"
echo   Characters 0 to 3: %MY_STRING:~0,4%    (using "%%STR:~0,4%%")
echo   Characters 3 to 8: %MY_STRING:~2,6%  (using "%%STR:~2,6%%")
setlocal EnableDelayedExpansion
echo .

echo ### Calculate the length of a string ###
echo   There is no "strlen" equivalent in DOS scripting, so let's create one...
call :strlen MY_STRING MY_STRING_LENGTH
echo   "%MY_STRING%" is %MY_STRING_LENGTH% characters long
echo .
goto StringsEnd

:strlen
setlocal EnableDelayedExpansion
set LENGTH=0
:strlenLoop
if not "!%1:~%LENGTH%,1%!"=="" (
    set /A LENGTH+=1
    goto :strlenLoop
)
endlocal & set %2=%LENGTH%
exit /b 0

:StringsEnd
endlocal
echo All done!
