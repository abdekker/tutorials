@echo off
rem Script was adapted from https://stackoverflow.com/questions/673523
rem To time how long a code block (or command) takes to run:
rem   1) Add "set LocalStart=%time%" before the code block
rem   2) Add "set LocalEnd=%time%" after the code block
rem   3) Add "call TimeCommands %LocalStart% %LocalEnd%"
rem   4) This script will set two environment variables:
rem         %TimeTaken% (includes hours - use this one it is likely to be very slow)
rem         %TimeTakenSeconds% (just the seconds - use this for normal commands)

rem Set the start and end time
set StartTime=%1
set EndTime=%2
rem echo Start: %StartTime%, End: %EndTime%

rem Set options for parsing the time
set Options="tokens=1-4 delims=:.,"
for /f %Options% %%a in ("%StartTime%") do (
    set start_h=%%a
    set /a start_m=100%%b %% 100
    set /a start_s=100%%c %% 100
    set /a start_ms=100%%d %% 100
)

for /f %Options% %%a in ("%EndTime%") do (
    set end_h=%%a
    set /a end_m=100%%b %% 100
    set /a end_s=100%%c %% 100
    set /a end_ms=100%%d %% 100
)

rem echo Start: h=%start_h%, m=%start_m%, s=%start_s%, ms=%start_ms%
rem echo End:   h=%end_h%, m=%end_m%, s=%end_s%, ms=%end_ms%

set /a hours=%end_h%-%start_h%
set /a mins=%end_m%-%start_m%
set /a secs=%end_s%-%start_s%
set /a ms=%end_ms%-%start_ms%
if %ms% lss 0 set /a secs = %secs% - 1 & set /a ms = 100%ms%
if %secs% lss 0 set /a mins = %mins% - 1 & set /a secs = 60%secs%
if %mins% lss 0 set /a hours = %hours% - 1 & set /a mins = 60%mins%
if %hours% lss 0 set /a hours = 24%hours%
if 1%ms% lss 100 set ms=0%ms%

:: Mission accomplished!
set /a totalsecs = %hours%*3600 + %mins%*60 + %secs%
set TimeTaken=%hours%:%mins%:%secs%.%ms%
set TimeTakenSeconds=%totalsecs%.%ms%s
rem echo command took %TimeTaken% (%TimeTakenSeconds% total)
