<# There are a few options to stop the script exiting after executiong:
1) From the command prompt run: PowerShell -NoExit "Path\to\Script\script.ps1"
	(Does not close PowerShell after running the script; type "exit" to close Powershell)
2a) Add to the end of the script: pause
	(Displays "Press Enter to continue...:", similar to "pause" in a command prompt script)
2b) Add to the end of the script: read-Host -Prompt "Press Enter to exit"
	(Similar to "pause", but allows a customised message to be displayed)
3) Add to the end of the script: $host.enternestedprompt()
	(This creates a nested prompt. When the nested prompt is closed, then PowerShell will exit.)
4) Add to the end of the script: $null = $host.UI.RawUI.ReadKey("NoEcho,IncludeKeyDown")
	(Waits until a key is pressed)
#>
Write-Host "Hello, World!"
pause
#read-Host -Prompt "The script is completed...press a key to exit"
#$host.enternestedprompt()
#$null = $host.UI.RawUI.ReadKey("NoEcho,IncludeKeyDown")