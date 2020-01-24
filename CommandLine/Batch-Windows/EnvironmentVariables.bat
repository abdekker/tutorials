@echo off

rem This script covers some aspects of environment variables in DOS and Windows

echo ### Check whether an environment variable is defined ###
echo 1) Using "defined"
set TEST_PARAM_1=test1
echo   TEST_PARAM_1 = %TEST_PARAM_1%
if defined TEST_PARAM_1 (echo   Parameter 1 is defined) else (echo   Parameter 1 is NOT defined!)
if defined TEST_PARAM_2 (echo   Parameter 2 is defined) else (echo   Parameter 2 is NOT defined!)
echo .

echo 2a) Using "==" (using qutoation marks)
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
