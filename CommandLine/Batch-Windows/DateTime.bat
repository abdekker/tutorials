@echo off

rem This script demonstrates some simple date and time commands
echo ### Date/Time strings (taken from Strings.bat) ###
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

echo All done^!
goto :EOF
