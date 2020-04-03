@echo off
setlocal EnableDelayedExpansion

rem Utility to create a random (alphanumeric) file or folder in the specified path
rem Usage: CreateRandom [--file --dir] PATH [length (optional)]
rem Example: CreateRandom --file C:\Tmp 16
rem     Might create the file C:\Tmp\Px8I3BAZIKTuA4f7.txt (".txt" is appended to files)

rem Put parameters into a (1-based) temporary array
set argCount=0
for %%x in (%*) do (
   set /a argCount += 1
   set "argVec[!argCount!]=%%~x"
)
rem echo   DEBUG: Number of arguments = %argCount%

rem Validate arguments
if %argCount% LSS 2 (echo Too few parameters & call :ShowHelp & goto :EOF)
if %argCount% GTR 3 (echo Too many parameters & call :ShowHelp & goto :EOF)

rem Parameter 1 (--file or --dir)
set /a CreateFile=0
set /a CreateDir=0
if !argVec[1]! EQU --file (
    set /a CreateFile=1
) else (
    if !argVec[1]! EQU --dir (
        set /a CreateDir=1
    ) else (
        call :ShowHelp
        goto :EOF
    )
)

rem Parameter 2 (PATH) [just assume this is correct]

rem Parameter 3 (length of random string)
set STR_LENGTH=16
if %argCount% EQU 2 (
    rem Only two parameters were supplied, so use the default length of 16 characters
    goto :ValidationComplete
)

call :IsNumeric !argVec[3]! IS_NUMERIC
if %IS_NUMERIC% EQU 1 (
    rem Valid integer but ensure its between our range (3-64)
    set STR_LENGTH=!argVec[3]!
    if !STR_LENGTH! LSS 3 (echo LENGTH too short & call :ShowHelp & goto :EOF)
    if !STR_LENGTH! GTR 64 (echo LENGTH too long & call :ShowHelp & goto :EOF)
) else (
    rem Invalid length...
    echo LENGTH is not numeric
    call :ShowHelpAndExit
)

:ValidationComplete
rem Validation completed!
rem echo   DEBUG: String length = %STR_LENGTH%
set ALPHA=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789
set RAND_STR=
call :GetStringLength ALPHA ALPHA_LENGTH
rem echo   DEBUG: "%ALPHA%" is %ALPHA_LENGTH% characters long
for /l %%i in (1,1,%STR_LENGTH%) do call :AddCharAlpha %ALPHA% %ALPHA_LENGTH%
rem echo   DEBUG: Random string = %RAND_STR%

rem Create a file or directory at the specified PATH
if %CreateFile% EQU 1 (
    echo Help, I'm trapped in a mezuzah factory >> !argVec[2]!\%RAND_STR%.txt
    goto :ScriptComplete
)

if %CreateDir% EQU 1 (
    mkdir !argVec[2]!\%RAND_STR%
    goto :ScriptComplete
)

:ScriptComplete
goto :EOF

:ShowHelp
rem Show options if parameters are missing or do not validate correctly
echo   Usage: %1 [--file --dir] PATH [LENGTH (optional)]
echo   Options:
echo     --file       Create random file in PATH (.txt is appended)
echo     --dir        Create random directory in PATH
echo     PATH         Directory where file/directory will be created
echo     LENGTH       (Optional) Length in the range 3 to 64 inclusive (16 is used if missing)
echo .
exit /b 0

:IsNumeric
rem Returns 1 for valid and 0 for invalid numbers. "2>nul" swallows errors.
set %2=0
set /a VAR_CHECK=%1 2>nul
if %VAR_CHECK%x==%1x (set %2=1)
exit /b 0

:GetStringLength
set TMP_LENGTH=0
:StringLengthLoop
if not "!%1:~%TMP_LENGTH%,1%!"=="" (
    set /a TMP_LENGTH+=1
    goto :StringLengthLoop
)
set %2=%TMP_LENGTH%
exit /b 0

:AddCharAlpha
set /a x=%random% %% %ALPHA_LENGTH%
set RAND_STR=%RAND_STR%!ALPHA:~%x%,1!
exit /b 0
