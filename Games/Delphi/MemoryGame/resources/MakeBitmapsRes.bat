@echo off
rem Set the resource path - just change this your path if different
@echo on
set PROG_PATH=C:\TestProgs\MemoryGame\resources

@echo off
rem Delete the old resource file
@echo on
cd %PROG_PATH%\
IF EXIST Bitmaps.res del Bitmaps.res /f /q

@echo off
rem Use the Visual Studio tools to generate the .res files from the .rc files
@echo on
cd C:\Program Files\Microsoft Visual Studio .NET 2003\Vc7\bin\
call vcvars32
rc /r %PROG_PATH%\Bitmaps_noPas.rc

@echo off
rem Rename the "noPas" .res file to its proper name
@echo on
cd %PROG_PATH%\
ren Bitmaps_noPas.res Bitmaps.res

pause

exit
