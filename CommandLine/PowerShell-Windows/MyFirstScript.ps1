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
    Set-ExecutionPolicy RemoteSigned
and finally re-open PS. #>
Write-Host "Hello, World!"