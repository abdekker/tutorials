@echo off
rem setlocal EnableDelayedExpansion

rem This script demonstrate using strings in DOS scripting

echo ### Basic string construction ###
echo   Construct a string with:
echo      set STR="VALUE"
echo      set "STR=VALUE" (recommended)
echo   Concatenate strings with:
echo      set STR3=%%STR1%%%%STR2%%
set STR1=Hello
set STR2=World!
set STR3=%STR1% %STR2%
echo   STR1 = %STR1%, STR2 = %STR2%, STR3 = %STR3%
echo .

echo ### Calculate the length of a string ###
echo   There is no "strlen" equivalent in DOS scripting, so let's create one...
set "MyString=Hello World"
call :strlen MyString MyStringLength
echo   "%MyString%" is %MyStringLength% characters long
echo .
goto StringLengthEnd

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

:StringLengthEnd

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
goto StringComparisonsEnd

:StringsEQU_v1
rem Using the "EQU" operator
if %1 EQU %2 (echo     %1 is equal to %2) else (echo     %1 is NOT equal to %2)
exit /b 0

:StringsEQU_v2
rem Using the "==" operator
if %1 == %2 (echo     %1 is equal to %2) else (echo     %1 is NOT equal to %2)
exit /b 0

:StringComparisonsEnd
echo ### Extracting characters and sub-strings ###
set "MyString=Hello World"
echo   String is "%MyString%"
echo   Strings are 0-based so...
echo     Character 2:       %MyString:~1,1%       (using "%%STR:~1,1%%")
echo     Characters 0 to 3: %MyString:~0,4%    (using "%%STR:~0,4%%")
echo     Characters 3 to 8: %MyString:~2,6%  (using "%%STR:~2,6%%")
echo .
set LString=%MyString:~0,4%
set RString=%MyString:~-4%
set OneLessBothEnds=%MyString:~1,-1%
echo   Left string (first 4 characters) = %LString%
echo   Right string (last 4 characters) = %RString%
echo   One less character each end      = %OneLessBothEnds%
echo .

echo ### String substitution/replacement ###
set "MyString=the cat in the hat"
set MyStringBadSpelling=%MyString:the=teh%
set MyStringNoSpaces=%MyString: =%
set MyStringNoThe=%MyString:the =%
call :strlen MyString MyStringLength
call :strlen MyStringBadSpelling MyStringBadSpellingLength
call :strlen MyStringNoSpaces MyStringNoSpacesLength
call :strlen MyStringNoThe MyStringNoTheLength
echo   Original     = %MyString% (%MyStringLength% characters)
echo   Bad spelling = %MyStringBadSpelling% (%MyStringBadSpellingLength% characters)
echo   No spaces    = %MyStringNoSpaces% (%MyStringNoSpacesLength% characters)
echo   No "the"s    = %MyStringNoThe% (%MyStringNoTheLength% characters)
echo .

echo ### Trim left (remove leading spaces) ###
set MyString=               15 leading spaces to truncate
echo   Original     = %MyString%
for /f "tokens=* delims= " %%a in ("%MyString%") do set MyString=%%a
echo   Left-trimmed = %MyString%
echo .

echo ### Trim right (remove trailing spaces) ###
setlocal EnableDelayedExpansion
set MyString=15 trailing Spaces to truncate               & rem
call :strlen MyString MyStringOriginalLength
echo   Original      = %MyString% (%MyStringOriginalLength% characters)
for /l %%a in (1,1,31) do if "!MyString:~-1!"==" " set MyString=!MyString:~0,-1!
call :strlen MyString MyStringTrimmedLength
echo   Right-trimmed = %MyString% (%MyStringTrimmedLength% characters)
endlocal
echo .

echo ### Trim quotes ###
set MyString="cmd politic"
echo   Original  = %MyString%
for /f "useback tokens=*" %%a in ('%MyString%') do set MyString=%%~a
echo   No quotes = %MyString%
echo .

echo ### String formatting ###
set x=3000
set y=2
echo   X=%x%, Y=%y%
echo .
echo Align right
set xWithSpaces=        %x%
set yWithSpaces=        %y%
echo   X=%xWithSpaces:~-8%
echo   Y=%yWithSpaces:~-8%
echo .

echo ### Date/Time strings ###
echo Date and time = %DATE% %TIME%
echo Be aware of locale! On English (US) or non-English systems, date/time formats can be different.
echo   Year     : %DATE:~6,4%
echo   Month    : %DATE:~3,2%
echo   Day      : %DATE:~0,2%
echo   Hour     : %TIME:~0,2%
echo   Minute   : %TIME:~3,2%
echo   Second   : %TIME:~6,2%
set TimeStamp=%DATE:~6,4%_%DATE:~3,2%_%DATE:~0,2%T%TIME:~0,2%_%TIME:~3,2%_%TIME:~6,2%
set TimeStamp=%TimeStamp: =0%
echo Combine into an ISO 8601 format (suitable for a log file): %TimeStamp%
rem Note: TimeStamp=%TimeStamp: =0% replaces all spaces with 0s
echo .

:StringsEnd
echo All done!
