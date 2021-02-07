# This script lock a file to prevent it from being modified or copied

# Specify the file name
$fileName = "C:\Project\LomaX4\target\Access__.ini"

# Open the file in read-only mode, without sharing (ie. locked)
$file = [System.io.File]::Open($fileName, 'Open', 'Read', 'None')

# Wait in the file locked state until the user presses a key...
Write-Host $fileName "is locked...press any key to continue ..."
$null = $host.UI.RawUI.ReadKey("NoEcho,IncludeKeyDown")

# Close the file
$file.Close()