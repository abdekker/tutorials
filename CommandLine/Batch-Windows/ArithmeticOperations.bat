@echo off

rem This script demonstrates arithmetic operations in DOS scripting
setlocal

rem Test git connection from VS Code

echo ### Introductory notes ###
rem The symbols < and > are not allowed since these are file redirection operators in DOS
echo   1) Do not use std "less than" or "greater than" symbols. They are file redirection operators.
echo   2) Use "set /a %%VAR%%+=1" for operations on numeric variables
echo(

echo ### Basic comparisons on numbers (less than, equals, etc) ###
set /a Negative=-5
set /a SmallA=10
set /a SmallB=10
set /a Large=50
echo   Negative = %Negative%, Small (x2) = %SmallA%, Large = %Large%
echo .
echo Six basic comparion operators: LSS, EQU, NEQ, LEQ, GTR, GEQ
echo   1) LSS (less than)
call :NumericLSS %Negative% %SmallA%
call :NumericLSS %SmallA% %Negative%
call :NumericLSS %SmallA% %SmallB%
call :NumericLSS %Large% %SmallA%
echo .
echo   2) EQU (equal to)
call :NumericEQU %Negative% %SmallA%
call :NumericEQU %SmallA% %Negative%
call :NumericEQU %SmallA% %SmallB%
call :NumericEQU %Large% %SmallA%
echo .
echo   3) NEQ (not equal to)
call :NumericNEQ %Negative% %SmallA%
call :NumericNEQ %SmallA% %Negative%
call :NumericNEQ %SmallA% %SmallB%
call :NumericNEQ %Large% %SmallA%
echo .
echo   4) LEQ (less than or equal to)
call :NumericLEQ %Negative% %SmallA%
call :NumericLEQ %SmallA% %Negative%
call :NumericLEQ %SmallA% %SmallB%
call :NumericLEQ %Large% %SmallA%
echo .
echo   5) GTR (greater than)
call :NumericGTR %Negative% %SmallA%
call :NumericGTR %SmallA% %Negative%
call :NumericGTR %SmallA% %SmallB%
call :NumericGTR %Large% %SmallA%
echo .
echo   6) GEQ (greater than or equal to)
call :NumericGEQ %Negative% %SmallA%
call :NumericGEQ %SmallA% %Negative%
call :NumericGEQ %SmallA% %SmallB%
call :NumericGEQ %Large% %SmallA%
echo .
goto BasicOperationsNumericEnd

:NumericLSS
if %1 LSS %2 (echo     %1 is less than %2) else (echo     %1 is NOT less than %2)
exit /b 0

:NumericEQU
if %1 EQU %2 (echo     %1 is equal to %2) else (echo     %1 is NOT equal to %2)
exit /b 0

:NumericNEQ
if %1 NEQ %2 (echo     %1 is NOT equal to %2) else (echo     %1 is equal to %2)
exit /b 0

:NumericLEQ
if %1 LEQ %2 (echo     %1 is less than or equal to %2) else (echo     %1 is NOT less than or equal to %2)
exit /b 0

:NumericGTR
if %1 GTR %2 (echo     %1 is greater than %2) else (echo     %1 is NOT greater than %2)
exit /b 0

:NumericGEQ
if %1 GEQ %2 (echo     %1 is greater than or equal to %2) else (echo     %1 is NOT greater than or equal to %2)
exit /b 0

:BasicOperationsNumericEnd

echo ### Basic comparisons on strings ###
echo   Similiar comparison operators (LSS, etc) are available for strings
set String_abc="abc"
set String_ABC1="ABC"
set String_ABC2="ABC"
set String_xyz="xyz"
echo 1 = %String_abc%, 2 = %String_ABC1%, 3 = %String_ABC2%, 4 = %String_xyz%
echo .
echo   1) LSS
call :NumericLSS %String_abc% %String_ABC1%
call :NumericLSS %String_ABC1% %String_abc%
call :NumericLSS %String_ABC1% %String_ABC2%
call :NumericLSS %String_xyz% %String_abc%
echo .
echo   2) EQU
echo   For case-insensitive comparisons, use the "/i" switch
call :NumericEQU %String_abc% %String_ABC1%
call :NumericEQU %String_ABC1% %String_abc%
call :NumericEQU %String_ABC1% %String_ABC2%
call :NumericEQU %String_xyz% %String_abc%
echo .
echo Note: Also test for string equality with "=="
call :StringsEQU %String_ABC1% %String_ABC2%
echo .
echo   3) NEQ
call :NumericNEQ %String_abc% %String_ABC1%
call :NumericNEQ %String_ABC1% %String_abc%
call :NumericNEQ %String_ABC1% %String_ABC2%
call :NumericNEQ %String_xyz% %String_abc%
echo .
echo   4) LEQ
call :NumericLEQ %String_abc% %String_ABC1%
call :NumericLEQ %String_ABC1% %String_abc%
call :NumericLEQ %String_ABC1% %String_ABC2%
call :NumericLEQ %String_xyz% %String_abc%
echo .
echo   5) GTR
call :NumericGTR %String_abc% %String_ABC1%
call :NumericGTR %String_ABC1% %String_abc%
call :NumericGTR %String_ABC1% %String_ABC2%
call :NumericGTR %String_xyz% %String_abc%
echo .
echo   6) GEQ
call :NumericGEQ %String_abc% %String_ABC1%
call :NumericGEQ %String_ABC1% %String_abc%
call :NumericGEQ %String_ABC1% %String_ABC2%
call :NumericGEQ %String_xyz% %String_abc%
echo .
goto BasicOperationsStringsEnd

:StringsEQU
rem Using the "==" operator
if %1 == %2 (echo     %1 is equal to %2) else (echo     %1 is NOT equal to %2)
exit /b 0

:BasicOperationsStringsEnd
echo All done!
