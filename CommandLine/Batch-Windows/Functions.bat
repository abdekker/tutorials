@echo off
setlocal EnableDelayedExpansion

rem This script demonstrates using functions in DOS scripting
echo ### Functions in batch scripts ###
echo   Declare with ":funcName"
echo   Use with "call :funcName"
echo   Exit function with "exit /b X" where "X" is typically "0"
echo .

echo ### Simple function (no parameters) ###
echo   Before simple function
call :funcSimpleNoParams
echo   After simple function
echo .

echo ### Simple function (with parameters) ###
call :funcSimpleWithParams
call :funcSimpleWithParams 123
echo .

echo ### Set variable inside function ###
set MY_VAR=one
echo   Before: MY_VAR = "%MY_VAR%"
call :funcSetParamTwo MY_VAR
echo   After:  MY_VAR = "%MY_VAR%"
echo .

echo Take care inside "if" statements^^! Use "setlocal EnableDelayedExpansion" and inside the code
echo block use ^^!VAR^^! (not %%VAR%%).
if defined MY_VAR (
    echo   Before: MY_VAR = "%MY_VAR%"
    call :funcSetParamThree MY_VAR
    echo   After:  MY_VAR = "%MY_VAR%" (global)
    echo   After:  MY_VAR = "!MY_VAR!" (local)
)
echo .

echo ### Use the return value (saved in %%ERRORLEVEL%%) ###
echo   Before: ERRORLEVEL = %ERRORLEVEL%
call :funcSetErrorLevel
echo   After:  ERRORLEVEL = %ERRORLEVEL%
echo Setting an error (with "exit /b X") can be used as a function return value.
echo Exclamation marks do NOT work with %%ERRORLEVEL%% inside an "if" statement^^!
echo .
echo Final comment about %%ERRORLEVEL%% is that it can be set to "1", without exiting, by running a
echo small invalid command like "COLOR 00".
echo   Before: ERRORLEVEL = %ERRORLEVEL%
COLOR 00
echo   After:  ERRORLEVEL = %ERRORLEVEL%
echo .
echo All done^^!
goto :EOF

:funcSimpleNoParams
echo     Inside simple function
exit /b 0

:funcSimpleWithParams
if not %1x==x (echo   Parameter 1 is "%1") else (echo   No parameter supplied)
exit /b 0

:funcSetParamTwo
set %1=two
exit /b 0

:funcSetParamThree
set %1=three
exit /b 0

:funcSetErrorLevel
exit /b 7
