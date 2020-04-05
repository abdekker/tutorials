@echo off

rem This script demonstrates simple counting of files in a folder

echo ### Count files in current folder (syntax 1) ###
rem Using "for /f" with pipe (|) command. This searches for the line "X File(s)" when calling "dir".
echo Using "for /f" with pipe (^|) looking for "X File(s)" when calling the "dir" command
for /f %%a in ('dir ^| find "File(s)"') do set cnt=%%a
echo   Count (current folder) = %cnt%
for /f %%a in ('dir %WINDIR% ^| find "File(s)"') do set cnt=%%a
echo   Count (all files in %WINDIR%) = %cnt%
for /f %%a in ('dir %WINDIR%\*.log ^| find "File(s)"') do set cnt=%%a
echo   Count (log files only in %WINDIR%) = %cnt%
for /f %%a in ('dir %WINDIR% ^| find "Dir(s)"') do set cnt=%%a
echo   Count (directories in %WINDIR%) = %cnt%
echo .

echo ### Count of all files in %WINDIR% (syntax 2) ###
echo Using "for /f" with "dir /b" (bare format) and looking for non-empty lines
rem The "/a-d-s-h" ensures that directories, system and hidden files are not returned
for /f %%a in ('dir %WINDIR% /a-d-s-h /b ^| find /v /c ""') do set cnt=%%a
echo   Count (all files in %WINDIR%) = %cnt%
for /f %%a in ('dir %WINDIR% /ad /b ^| find /v /c ""') do set cnt=%%a
echo   Count (directories in %WINDIR%) = %cnt% (different count compared with "dir")
echo .

echo ### Iterate through all files, incrementing a variable ###
echo Simple iteration (which counts hidden and system files too so may be different)
rem Use "for %%a in (*) do set /a cnt+=1" for the current folder
set cnt=0
for %%a in (%WINDIR%\*) do set /a cnt+=1
echo   Count (all files in %WINDIR%) = %cnt%
echo .

echo All done!
