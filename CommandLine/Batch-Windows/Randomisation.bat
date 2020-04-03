@echo off
setlocal EnableDelayedExpansion

rem This script demonstrates methods of generating random variables in DOS scripting

echo ### Random numbers ###
echo The %%random%% command returns a random number from 0 to 32767. Generate five numbers...
for /l %%a in (1,1,5) do (
    echo   %%a = !random!
)
echo .

echo Adjust range to 1 to 100 (inclusive)
echo   Method 1 (Using the modulus operator)
set RAND_MIN=100
set RAND_MAX=0
for /l %%a in (1,1,5) do (
    set /a RAND_NUM=!random! %%100 + 1
    if !RAND_NUM! LSS !RAND_MIN! (set RAND_MIN=!RAND_NUM!)
    if !RAND_NUM! GTR !RAND_MAX! (set RAND_MAX=!RAND_NUM!)
    echo     %%a = !RAND_NUM!
)
echo   Min = %RAND_MIN%, Max = %RAND_MAX%
echo .

echo   Method 2 (Using integer math)
set RAND_MIN=100
set RAND_MAX=0
for /l %%a in (1,1,5) do (
    call :GenerateRandomNumber 1 100
    if !RAND_NUM! LSS !RAND_MIN! (set RAND_MIN=!RAND_NUM!)
    if !RAND_NUM! GTR !RAND_MAX! (set RAND_MAX=!RAND_NUM!)
    echo     %%a = !RAND_NUM!
)
echo   Min = %RAND_MIN%, Max = %RAND_MAX%
echo .
goto RandomNumbersEnd

:GenerateRandomNumber
:: General method which generates a number in the range %1 to %2 (inclusive)
:: Alternatively, use "%RANDOM% * 100 / 32768 + 1" for a number between 1 and 100
set /a RAND_NUM=%random% * (%2 - %1 + 1) / 32768 + %1
exit /b 0

:RandomNumbersEnd

echo ### Random strings ###
echo   Example 1 (length 16, lowercase)
set LOWER=abcdefghijklmnopqrstuvwxyz
set RAND_STR=
for /l %%i in (1,1,16) do call :AddCharLower
echo   Random string (lower) = %RAND_STR%
echo .
goto RandomStringLowerEnd

:AddCharLower
set /a x=%random% %% 26
set RAND_STR=%RAND_STR%!LOWER:~%x%,1!
exit /b 0

:RandomStringLowerEnd

echo   Example 2 (length 16, alphanumeric)
set ALPHA=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789
set RAND_STR=
call :GetStringLength ALPHA ALPHA_LENGTH
rem echo   DEBUG: "%ALPHA%" is %ALPHA_LENGTH% characters long
for /l %%i in (1,1,16) do call :AddCharAlpha %ALPHA% %ALPHA_LENGTH%
echo   Random string (alphanumeric) = %RAND_STR%
echo .
goto RandomStringsEnd

:GetStringLength
set LENGTH=0
:StringLengthLoop
if not "!%1:~%LENGTH%,1%!"=="" (
    set /a LENGTH+=1
    goto :StringLengthLoop
)
set %2=%LENGTH%
exit /b 0

:AddCharAlpha
set /a x=%random% %% %ALPHA_LENGTH%
set RAND_STR=%RAND_STR%!ALPHA:~%x%,1!
exit /b 0

:RandomStringsEnd
echo All done!
endlocal
