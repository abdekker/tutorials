@echo off

rem This script demonstrates using the "forfiles" command
echo ### List all log files in %WINDIR% that are older than 30 days ###
forfiles /p C:\Windows /m *.log /d -30 /c "cmd /c echo @FDATE @FILE"
echo .

echo ### List and delete all test*.txt files in C:\Tmp ###
echo   To demonstrate, create (in C:\Tmp) test1.txt, testXXX.txt, etc
echo List...
forfiles /p C:\Tmp /m test*.txt /c "cmd /c echo @FDATE @FILE"
for /f %%a in ('dir C:\Tmp\test*.txt /a-d-s-h /b ^| find /v /c ""') do set CountFiles=%%a
echo   (%CountFiles% files counted)
echo .
echo Delete ...
forfiles /p C:\Tmp /m test*.txt /c "cmd /c del @PATH"
for /f %%a in ('dir C:\Tmp\test*.txt /a-d-s-h /b ^| find /v /c ""') do set CountFiles=%%a
echo   (%CountFiles% files remaining)
echo .

echo All done!
