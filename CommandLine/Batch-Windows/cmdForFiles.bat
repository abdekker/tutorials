@echo off

rem This script demonstrates using the "forfiles" command
echo ### List all log files in %WINDIR% older than 30 days ###
forfiles /p C:\Windows /m *.log /d -30 /c "cmd /c echo @FDATE @FILE"
echo .

echo ### List and delete all test*.txt files in C:\Tmp ###
echo   To demonstrate, create (in C:\Tmp) test1.txt, testXXX.txt, etc
echo List...
forfiles /p C:\Tmp /m test*.txt /c "cmd /c echo @FDATE @FILE"
for /f %%a in ('dir C:\Tmp\test*.txt /a-d-s-h /b ^| find /v /c ""') do set CountFiles=%%a
echo   (%CountFiles% files counted)
echo .
if %CountFiles% EQU 0 goto :NoFilesToDelete AfterDeleteFiles
echo Delete...
forfiles /p C:\Tmp /m test*.txt /c "cmd /c del @PATH"
for /f %%a in ('dir C:\Tmp\test*.txt /a-d-s-h /b ^| find /v /c ""') do set CountFiles=%%a
echo   (%CountFiles% files remaining)
goto :AfterDeleteFiles

:NoFilesToDelete
echo No files to delete!

:AfterDeleteFiles
echo .

echo ### Delete all files and sub-folders, but not the folder itself ###
echo   To demonstrate, create ^(in C:\Tmp\DirToKeep^) some files and sub-folders
rem The simpler command "rmdir C:\Tmp\DirToKeep /Q /S" will also delete the folder itself
if exist C:\Tmp\DirToKeep (
    forfiles /p C:\Tmp\DirToKeep /m * /c "cmd /c if @isdir==FALSE del @file"
    forfiles /p C:\Tmp\DirToKeep /m * /c "cmd /c if @isdir==TRUE rmdir /s /q @file"
) else (
    echo   Folder not found!
)
echo .

echo All done!
