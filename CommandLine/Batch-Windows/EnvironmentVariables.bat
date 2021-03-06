@echo off

rem This script covers some aspects of environment variables in DOS and Windows
rem On a development machine, create a new User environment variable called "DEV_TMP"

rem To create new (permanent) system environment variables use "setx VAR VALUE". This command is
rem not demonstrated in this script. Variables created created with "setx" are available in future
rem command windows (but not the current command window). Variables cannot be removed with "setx".

echo ### Environment Variables ###
echo Access these in System Properties / Advanced / Environment Variables. Variables are saved at
echo the following registry addresses:
echo   User   = HKCU\Environment
echo   System = HKLM\SYSTEM\CurrentControlSet\Control\Session Manager\Environment
echo If variables are added or modified programmatically, broadcast a WM_SETTINGCHANGE message
echo with lParam set to the string "Environment". This allows applications, such as the shell, to
echo pick up your updates.
echo .

echo ### Use an environment variable to prevent a script running multiple times ###
rem This trick can be used to prevent circular calls, and variations, such as:
rem ScriptA) call ScriptB
rem ScriptB) call ScriptA
if defined SCRIPT_IS_RUNNING (exit)
echo Now "SCRIPT_IS_RUNNING" is defined
echo .

echo ### Incrementing environment variables ###
set TEST_INTEGER=1
set /A TEST_INTEGER=TEST_INTEGER+1
if %TEST_INTEGER%==2 (echo   TEST_INTEGER is 2) else (echo   TEST_INTEGER is something else)
echo .

echo ### Check whether an environment variable is defined ###
echo 1) Using "defined"
set TEST_PARAM_1=test1
echo   TEST_PARAM_1 = %TEST_PARAM_1%
if defined TEST_PARAM_1 (echo   Parameter 1 is defined) else (echo   Parameter 1 is NOT defined!)
if defined TEST_PARAM_2 (echo   Parameter 2 is defined) else (echo   Parameter 2 is NOT defined!)
echo .

echo 2a) Using "==" (using quotation marks)
rem Note: Quotation marks are used because the variable may have spaces
if "%TEST_PARAM_1%"=="" (echo   Parameter 1 is NOT defined!) else (echo   Parameter 1 is defined)
if "%TEST_PARAM_2%"=="" (echo   Parameter 2 is NOT defined!) else (echo   Parameter 2 is defined)
echo .

echo 2b) Using "==" (using a trick that does not require quotation marks)
if %TEST_PARAM_1%x==x (echo   Parameter 1 is NOT defined!) else (echo   Parameter 1 is defined)
if %TEST_PARAM_2%x==x (echo   Parameter 2 is NOT defined!) else (echo   Parameter 2 is defined)
echo .

echo 3) Using not "=="
if not "%TEST_PARAM_1%"=="" (echo   Parameter 1 is defined) else (echo   Parameter 1 is NOT defined!)
if not "%TEST_PARAM_2%"=="" (echo   Parameter 2 is defined) else (echo   Parameter 2 is NOT defined!)
echo .

echo 4) Using NEQ
if "%TEST_PARAM_1%" NEQ "" (echo   Parameter 1 is defined) else (echo   Parameter 1 is NOT defined!)
if "%TEST_PARAM_2%" NEQ "" (echo   Parameter 2 is defined) else (echo   Parameter 2 is NOT defined!)
echo .

rem Uncomment the following line to see the results of the script
rem pause
