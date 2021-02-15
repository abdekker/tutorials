# Creates a list of the installed Windows Systems Apps (these come pre-installed with Windows 8+)

# Update your username in the line below (and delete WindowsApps.txt, if necessary)
Get-AppxPackage -User USERNAME > C:\Tmp\WindowsApps.txt

# Review the list of installed applications
# To uninstall an application run this command
# $ Remove-AppxPackage PackageFullName
# eg. Remove-AppxPackage Microsoft.Windows.OOBENetworkCaptivePortal
