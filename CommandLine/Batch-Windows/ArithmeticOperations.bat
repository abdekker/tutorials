@echo off

rem This script demonstrates some artithmetic operations in DOS scripting
setlocal

echo ### Introductory notes ###
rem The symbols < and > are not allowed since these are file redirection operators in DOS
echo   1) Do not use std "less than" or "greater than" symbols. They are file redirection operators.
echo   2) Use "set /a %%VAR%%+=1" for operations on numeric variables
echo(

echo ### Basic comparison operations (less than, equals, etc) ###
set /a Negative=-5
set /a SmallA=10
set /a SmallB=10
set /a Large=50
echo Negative = %Negative%, Small (x2) = %SmallA%, Large = %Large%
echo .
echo Six basic comparion operators: LSS, EQU, NEQ, LEQ, GTR, GEQ
echo   1) LSS
call :TestLSS %Negative% %SmallA%
call :TestLSS %SmallA% %Negative%
call :TestLSS %SmallA% %SmallB%
call :TestLSS %Large% %SmallA%
echo .
echo   2) EQU
call :TestEQU %Negative% %SmallA%
call :TestEQU %SmallA% %Negative%
call :TestEQU %SmallA% %SmallB%
call :TestEQU %Large% %SmallA%
echo .
echo   3) NEQ
call :TestNEQ %Negative% %SmallA%
call :TestNEQ %SmallA% %Negative%
call :TestNEQ %SmallA% %SmallB%
call :TestNEQ %Large% %SmallA%
echo .
echo   4) LEQ
call :TestLEQ %Negative% %SmallA%
call :TestLEQ %SmallA% %Negative%
call :TestLEQ %SmallA% %SmallB%
call :TestLEQ %Large% %SmallA%
echo .
echo   5) GTR
call :TestGTR %Negative% %SmallA%
call :TestGTR %SmallA% %Negative%
call :TestGTR %SmallA% %SmallB%
call :TestGTR %Large% %SmallA%
echo .
echo   6) GEQ
call :TestGEQ %Negative% %SmallA%
call :TestGEQ %SmallA% %Negative%
call :TestGEQ %SmallA% %SmallB%
call :TestGEQ %Large% %SmallA%
echo .
goto BasicOperationsEnd

:TestLSS
if %1 LSS %2 (echo     %1 is less than %2) else (echo     %1 is NOT less than %2)
exit /b 0

:TestEQU
if %1 EQU %2 (echo     %1 is equal to %2) else (echo     %1 is NOT equal to %2)
exit /b 0

:TestNEQ
if %1 NEQ %2 (echo     %1 is NOT equal to %2) else (echo     %1 is equal to %2)
exit /b 0

:TestLEQ
if %1 LEQ %2 (echo     %1 is less than or equal to %2) else (echo     %1 is NOT less than or equal to %2)
exit /b 0

:TestGTR
if %1 GTR %2 (echo     %1 is greater than %2) else (echo     %1 is NOT greater than %2)
exit /b 0

:TestGEQ
if %1 GEQ %2 (echo     %1 is greater than or equal to %2) else (echo     %1 is NOT greater than or equal to %2)
exit /b 0

:BasicOperationsEnd
