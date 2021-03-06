:: Bubblesort (horribly inefficient for large lists!)
:: Adapted from https://stackoverflow.com/questions/10166386
@echo off
setlocal EnableDelayedExpansion

:: Number of entries to populate and sort
:: BubbleSort is inefficient, taking O(N^2)...keep this number low!
set MaxValue=50

:: Fill a list of vars with Random numbers and print them
for /l %%a in (1,1,%MaxValue%) do (
    set /a ToSort[%%a]=!random!
)

:: Show the (unsorted) array
for /l %%a in (1,1,%MaxValue%) do (echo ToSort[%%a]=!ToSort[%%a]!)

:: Commence bubble sort
set LocalStart=%time%
echo Sorting...
set /a LoopMax=%MaxValue%-1
set Iterations=0
for /l %%a in (%LoopMax%,-1,1) do ( rem Decrease the number of checks by 1 each time (the top value floats to the end)
    set HasSwapped=0
        for /l %%b in (1,1,%%a) do (
            set /a Next=%%b+1
            set Next=ToSort[!Next!]
            set Next=!Next!
            call :GrabValues ToSort[%%b] !Next!
            rem Comparing ToSort[%%b] = !ToSortValue! and !Next! = !NextValue!
            if !NextValue! LSS !ToSortValue! (
                rem set /a num_of_swaps+=1
                rem echo Swapping !num_of_swaps!
                set !Next!=!ToSortValue!
                set ToSort[%%b]=!NextValue!
                set /a HasSwapped+=1
            )
        )
    set /a Iterations+=1
    if !HasSwapped!==0 goto Sorted
)

goto:eof
:GrabValues
set ToSortValue=!%1!
set NextValue=!%2!

goto:eof
:Sorted
:: The array is now sorted, nice!
set LocalEnd=%time%
call TimeCommands %LocalStart% %LocalEnd%
set ToSortValue=
echo Iterations: %Iterations% and Time: %TimeTakenSeconds%
for /l %%a in (1,1,%MaxValue%) do (echo ToSort[%%a]=!ToSort[%%a]!)
endlocal
