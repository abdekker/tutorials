# Demonstrates some aspects of working with network adapters.

# Following command shows connected network adapters on the local PC. The result might be:
#Idx     Met         MTU          State                Name
#---  ----------  ----------  ------------  ---------------------------
#  1          50  4294967295  connected     Loopback Pseudo-Interface 1
#  4          20        1500  connected     WiFi
#  5           5        1500  disconnected  Local Area Connection* 3
Write-Host "### Network adapters (netsh) ###"
netsh interface ipv4 show interfaces

# Following command shows all network adapters (including ones that are not connected)
Write-Host "### All network adapters (Get-WmiObject) ###"
Get-WmiObject win32_networkadapter | select netconnectionid, name, InterfaceIndex, netconnectionstatus
Write-Host ""

# By filtering on "netconnectionstatus" of "2", only connected adapters are shown
# Some values of flag include:
#	netconnectionstatus		Meaning
#			0				Disconnected
#			1				Connecting
#			2				Connected
#			3				Disconnecting (etc)
Write-Host "### Connected adapters (Get-WmiObject with filter) ###"
Get-WmiObject win32_networkadapter -filter "netconnectionstatus = 2" |
	select netconnectionid, name, InterfaceIndex, netconnectionstatus
Write-Host ""

# Pause before exiting...
pause