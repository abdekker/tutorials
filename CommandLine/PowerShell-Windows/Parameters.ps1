# Demonstrates checking parameters passed to the script. Uncomment the section you need.

# Option 1: Unnamed arguments. Run this script as ".\Parameters.ps1 fred 23".
Write-Host "There are a total of $($args.count) arguments"
for ($i = 0; $i -lt $args.count; $i++)
{
    write-host "  Argument $i is $($args[$i])"
}

# Option 2: Array of command-separate arguments. Run this script as ".\Parameters.ps1 fred,23"
#param($params)
#$i = 1
#foreach ($p in $params) 
#{
#	write-host "  Argument $i is $p"
	#$i = ($i + 1)
#} 

# Option 2a: Named parameters. Run this script as:
<#	$ .\Parameters.ps1 fred 23					=> Parameter 1 is "fred" and Parameter 2 is "23"
	$ .\Parameters.ps1 -param1 fred -param2 23 	=> Parameter 1 is "fred" and Parameter 2 is "23"
	$ .\Parameters.ps1 -param2 fred -param1 23 	=> Parameter 1 is "23" and Parameter 2 is "fred" #>
#param($param1,$param2)
#Write-Host "  Parameter 1 is |$param1|"
#Write-Host "  Parameter 2 is |$param2|"

# Option 2b: Named parameters with a default
<#	$ .\Parameters.ps1							=> Parameter 1 is "(blank)" and Parameter 2 is "23"
	$ .\Parameters.ps1 fred						=> Parameter 1 is "fred" and Parameter 2 is "23"
	$ .\Parameters.ps1 -param1 fred -param2 23 	=> Parameter 1 is "fred" and Parameter 2 is "23" #>
#param($param1,$param2='23')
#if ($param1 -eq $null)
#{
#	$param1 = read-host -Prompt "Please enter a value for parameter 1..." 
#}
#Write-Host "  Parameter 1 is |$param1|"
#Write-Host "  Parameter 2 is |$param2|"

# Pause before exiting...
pause