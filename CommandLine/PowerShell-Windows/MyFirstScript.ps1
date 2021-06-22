# This is a single line comment
<# And this...
...is a multi-line comment #>

<# This script will probably not run due to security concerns. PowerShell enforces these
execution policies:
* Restricted – Scripts won’t run. Period. (Default setting)
* RemoteSigned – Locally-created scripts will run. Scripts that were created on another machine will
not run unless they are signed by a trusted publisher.
* AllSigned – Scripts will only run if signed by a trusted publisher (including locally-created scripts).
* Unrestricted – All scripts will run regardless of who created them and whether or not they are signed.

To run this script, re-open PS in Administrator mode, then type this command:
    $ Set-ExecutionPolicy RemoteSigned
and finally re-run the script.

To run any script that you trust:
	$ Set-ExecutionPolicy Unrestricted -Force #>

<# Several methods to run Powershell scripts:
1) Right-click the script in Explorer and select "Run with Powershell"
2) Open Powershell, and use: ".\Path\to\Script\MyFirstScript.ps1"
	(or navigate to the folder and use: .\MyFirstScript.ps1
3) Open the Command Prompt and use: PowerShell ".\Path\to\Script\MyFirstScript.ps1"
	(or navigate to the folder and use: PowerShell .\MyFirstScript.ps1 #>
Write-Host "Hello, World!"
Write-Host ""

# You can insert new lines with:
#	Write-Host ""
#	Write-Host `n
Write-Host "AAA"`n"BBB"`n"CCC"
Write-Host ""
Write-Host "DDD"

# Show the current Powershell version
Get-Host | Select-Object Version
Write-Host ""

# Set a simple variable and write in a different colour
$MyVariable = "Donkey"
Write-Host -ForegroundColor cyan "Your variable has the value: $MyVariable"

# Pause before exiting...
pause