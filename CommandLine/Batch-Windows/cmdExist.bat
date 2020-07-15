@echo off

rem Demonstrates the "exists" command

echo File or folder known to exist
if EXIST %windir%\regedit.exe (echo   Registry editor found, huzzah!) else (echo   Registry editor missing, oops)
echo .

echo File or folder known to NOT exist
if EXIST C:\zzz\yyy\xxx.abc (echo   File was found...that's weird) else (echo   File was missing, as expected)
echo .

echo Check for the absence of a file or folder (opposite sense)
if NOT EXIST C:\zzz\yyy\xxx.abc (echo   File missing...we could abort, but launching our nukes sounds more fun)
echo .

echo Done!
