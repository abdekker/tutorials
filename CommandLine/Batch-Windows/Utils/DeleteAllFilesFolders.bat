@echo off
setlocal EnableDelayedExpansion

rem Utility to delete files and sub-folders from the specified path (but not the directory itself)
rem Usage: DeleteAllFilesFolders [drive:][path] [/Q]
rem   /Q    Quiet mode (Warning, no prompt!)

rem Example: DeleteAllFilesFolders C:\Tmp
rem     Deletes files and sub-folders in C:\Tmp (but not C:\Tmp)
rem Note: "rmdir C:\Tmp /Q /S" deletes C:\Tmp as well

set argCount=0
set isShowHelp=0
set isQuietMode=0
for %%x in (%*) do (
   set /a argCount += 1
   set "argVec[!argCount!]=%%~x"
   if %%~x == /? (set isShowHelp=1)
   if /i %%~x == /q (set isQuietMode=1)
)
rem echo   DBG: Number of processed arguments: %argCount%

rem Show help?
if %argCount% EQU 0 (set isShowHelp=1)
if %isShowHelp% EQU 1 (
    call :ShowHelp %~nx0
    goto :ScriptEnd
)

rem Check whether the path exists
if not exist !argVec[1]! (
    echo The path "!argVec[1]!" was not recognised
    goto :ScriptEnd
)

rem Check whether the user really wants to delete files and sub-folders!
if %isQuietMode% EQU 1 (goto :ReadyToDelete)
echo *All* files and sub-folders will be deleted from !argVec[1]! ^^!
set /p AREYOUSURE=  Are you sure (Y/[N])?
if /i "%AREYOUSURE%" NEQ "Y" goto :ScriptEnd

:ReadyToDelete
rem Count the number of files and folders in the selected path
for /f %%a in ('dir !argVec[1]! /a-d-s-h /b ^| find /v /c ""') do set fileCount=%%a
for /f %%a in ('dir !argVec[1]! /ad /b ^| find /v /c ""') do set dirCount=%%a
set /a totalCount = !fileCount! + !dirCount!
if !totalCount! GTR 0 (
    forfiles /p !argVec[1]! /m * /c "cmd /c if @isdir==FALSE del @file"
    forfiles /p !argVec[1]! /m * /c "cmd /c if @isdir==TRUE rmdir /s /q @file"
)

:ScriptEnd
endlocal
goto :eof

:ShowHelp
rem Show options if parameters are missing or do not validate correctly
rem %0 is the function name (in this case ":ShowHelp")
echo   Usage: %1 PATH [/Q]
echo   Options:
echo     PATH   Directory where files and sub-directories will be deleted
echo     /Q     (Optional) Quiet mode (no prompt)
echo .
exit /b 0
