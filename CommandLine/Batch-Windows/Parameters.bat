@echo off
setlocal EnableDelayedExpansion

rem Demonstrates checking parameters passed to the script
rem Example: ./Parameters.bat --help --version
set SCRIPT_VERSION=1.0.23

echo ### Count arguments and place them in an array ###
set argCount=0
for %%x in (%*) do (
   set /a argCount += 1
   set "argVec[!argCount!]=%%~x"
)
echo   Number of processed arguments: %argCount%

if %argCount% GTR 0 (
    rem One or more arguments (which are now stored in the "argVec" array)
    for /l %%i in (1,1,%argCount%) do (
        echo     %%i: "!argVec[%%i]!"
    )
    echo .

    rem Do any arguments require action?
    rem Note: For string comparisons, you can use "EQU" or "=="
    set /a calledHelp=0
    set /a calledVersion=0
    for /l %%i in (1,1,%argCount%) do (
        if !argVec[%%i]! EQU --help (set /a showHelp += 1) else (
            if !argVec[%%i]! EQU --version (set /a showVersion += 1)
        )

        rem Help?
        if !argVec[%%i]! EQU --help (
            if !calledHelp! EQU 0 (
                call :ShowHelp %0
                set /a calledHelp += 1
            ) else (echo   Help already requested! & echo .)
        )

        rem Version?
        if !argVec[%%i]! EQU --version (
            if !calledVersion! EQU 0 (
                call :ShowVersion
                set /a calledVersion += 1
            ) else (echo   Version already displayed! & echo .)
        )
    )
) else (
    echo No parameters were passed...try sending "--help" or "--version"
)

echo All done!
goto :EOF

:ShowHelp
rem %~nx1 gives filename with extension (ie. Parameters.bat)
echo Usage: %~nx1 [options]
echo Options:
echo   --help       print this message
echo   --version    print version of this script file
echo .
exit /b 0

:ShowVersion
echo Version = %SCRIPT_VERSION%
echo .
exit /b 0
