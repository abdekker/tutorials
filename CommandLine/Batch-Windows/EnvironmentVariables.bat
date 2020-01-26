@echo off

rem This script covers some aspects of environment variables in DOS and Windows

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
pause

echo ### Check whether an environment variable is defined ###
echo 1) Using "defined"
set TEST_PARAM_1=test1
echo   TEST_PARAM_1 = %TEST_PARAM_1%
if defined TEST_PARAM_1 (echo   Parameter 1 is defined) else (echo   Parameter 1 is NOT defined!)
if defined TEST_PARAM_2 (echo   Parameter 2 is defined) else (echo   Parameter 2 is NOT defined!)
echo .

echo 2a) Using "==" (using quotation marks)
rem Note: Quotation marks are useful if the variable may have spaces
if "%TEST_PARAM_1%"=="" (echo   Parameter 1 is NOT defined) else (echo   Parameter 1 is defined!)
if "%TEST_PARAM_2%"=="" (echo   Parameter 2 is NOT defined) else (echo   Parameter 2 is defined!)
echo .

echo 2b) Using "==" (using a trick that does not require quotation marks)
if %TEST_PARAM_1%x==x (echo   Parameter 1 is NOT defined) else (echo   Parameter 1 is defined!)
if %TEST_PARAM_2%x==x (echo   Parameter 2 is NOT defined) else (echo   Parameter 2 is defined!)
echo .

echo 3) Using not "=="
if not "%TEST_PARAM_1%"=="" (echo   Parameter 1 is defined) else (echo   Parameter 1 is NOT defined!)
if not "%TEST_PARAM_2%"=="" (echo   Parameter 2 is defined) else (echo   Parameter 2 is NOT defined!)
echo .

echo 4) Using NEQ
if not "%TEST_PARAM_1%" NEQ "" (echo   Parameter 1 is NOT defined) else (echo   Parameter 1 is defined!)
if not "%TEST_PARAM_2%" NEQ "" (echo   Parameter 2 is NOT defined) else (echo   Parameter 2 is defined!)
echo .

rem Uncomment the following line to see the results of the script
rem pause
