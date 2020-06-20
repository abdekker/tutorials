@echo off

rem This script covers some of the intricacies of variable expansion

echo ### Use an environment variable to prevent a script running multiple times ###
rem This trick can be used to prevent circular calls, and variations, such as:
rem ScriptA) call ScriptB
rem ScriptB) call ScriptA
if defined SCRIPT_IS_RUNNING (exit)
echo Now "SCRIPT_IS_RUNNING" is defined
echo .

C:\Users\Alain\Projects\tutorials\CommandLine\Batch-Windows\EnableDelayedExpansion.bat
