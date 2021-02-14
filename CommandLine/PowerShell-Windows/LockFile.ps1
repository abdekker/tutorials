# This script lock a file to prevent it from being modified or copied

if ($($args.count) -eq 0)
{
	# No arguments...specify a hard-coded file
	$fileName = "C:\Project\LomaX4\target\Access__.ini"
}
else
{
	# The first argument is the name of the file to lock
	$fileName = $args[0]
}
Write-Host "  (attempting to lock: $fileName)"

# Open the file in read-only mode, without sharing (ie. locked)
$file = [System.io.File]::Open($fileName, 'Open', 'Read', 'None')

# Wait in the file locked state until the user presses a key...
Write-Host "$fileName is locked and cannot be modified or copied"
Write-Host "Press any key to unlock the file and continue ..."
$null = $host.UI.RawUI.ReadKey("NoEcho,IncludeKeyDown")

# Close the file
$file.Close()
Write-Host "  ($fileName is now unlocked...bye!)"
