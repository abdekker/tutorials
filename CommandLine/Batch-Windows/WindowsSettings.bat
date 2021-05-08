@echo off
rem Open one of the Windows Settings pages
setlocal EnableDelayedExpansion

rem Count arguments and place them in an array
set argCount=0
for %%x in (%*) do (
   set /a argCount += 1
   set "argVec[!argCount!]=%%~x"
)

rem Any parameters and can we act on them?
set argProcessed=0
if %argCount% GTR 0 (
    rem One or more arguments were supplied...use the first one only
	if !argVec[1]! EQU printers (call :ShowPrinters)
	if !argVec[1]! EQU batterysaver-settings (call :ShowBatterySaverSettings)
	if !argVec[1]! EQU powersleep (call :ShowPowerSleep)

	if !argProcessed!==0 (echo No parameters recognised...try sending "printers")
) else (
    echo No parameters passed...try sending "printers"
)

rem All done, bye!
goto :EOF

:ShowPrinters
rem Show Windows Settings > Devices > Printers & scanners
set /a argProcessed += 1
start ms-settings:printers
exit /b 0

:ShowBatterySaverSettings
rem Show Windows Settings > System > Battery
set /a argProcessed += 1
start ms-settings:batterysaver-settings
exit /b 0

:ShowPowerSleep
rem Show Windows Settings > System > Power & sleep
set /a argProcessed += 1
start ms-settings:powersleep
exit /b 0

rem Possible pages (taken from https://winaero.com/ms-settings-commands-in-windows-10/)

::	PAGE												COMMAND (URI)
::	Settings home
::		Settings home page								ms-settings:

::	System
::		Display											ms-settings:display
::			Night light settings						ms-settings:nightlight
::			Advanced scaling settings					ms-settings:display-advanced
::			Connect to a wireless display				ms-settings-connectabledevices:devicediscovery
::			Graphics settings							ms-settings:display-advancedgraphics
::			Display orientation							ms-settings:screenrotation

::		Sound (build 17063+)							ms-settings:sound
::			Manage sound devices						ms-settings:sound-devices
::			App volume and device preferences			ms-settings:apps-volume

::		Notifications & actions							ms-settings:notifications

::		Focus assist (build 17074+)						ms-settings:quiethours OR ms-settings:quietmomentshome
::			During these hours							ms-settings:quietmomentsscheduled
::			Duplicating my display						ms-settings:quietmomentspresentation
::			Playing a game full screen					ms-settings:quietmomentsgame

::		Power & sleep									ms-settings:powersleep

::		Battery	ms-settings:batterysaver
::			Apps affecting your battery life			ms-settings:batterysaver-usagedetails
::			Battery Saver settings						ms-settings:batterysaver-settings

::		Storage											ms-settings:storagesense
::			Configure Storage Sense or run it now		ms-settings:storagepolicies
::			Change where new content is saved			ms-settings:savelocations

::		Tablet mode										ms-settings:tabletmode
::		Multitasking									ms-settings:multitasking
::		Projecting to this PC							ms-settings:project
::		Shared experiences								ms-settings:crossdevice
::		Clipboard (build 17666+)						ms-settings:clipboard
::		Remote Desktop									ms-settings:remotedesktop
::		Device Encryption (where available)				ms-settings:deviceencryption
::		About											ms-settings:about

::	Devices
::		Bluetooth & other devices						ms-settings:bluetooth OR ms-settings:connecteddevices
::		Printers & scanners								ms-settings:printers
::		Mouse											ms-settings:mousetouchpad
::		Touchpad										ms-settings:devices-touchpad
::		Typing											ms-settings:typing
::			Hardware keyboard - Text suggestions		ms-settings:devicestyping-hwkbtextsuggestions
::		Wheel (where available)							ms-settings:wheel
::		Pen & Windows Ink								ms-settings:pen
::		AutoPlay										ms-settings:autoplay
::		USB												ms-settings:usb

::	Phone
::		Phone (build 16251+)							ms-settings:mobile-devices
::		Add a phone										ms-settings:mobile-devices-addphone
::		Your Phone (opens app)							ms-settings:mobile-devices-addphone-direct

::	Network & Internet
::		Network & Internet								ms-settings:network
::		Status											ms-settings:network-status
::			Show available networks						ms-availablenetworks:
::		Cellular & SIM									ms-settings:network-cellular
::		Wi-Fi											ms-settings:network-wifi
::			Show available networks						ms-availablenetworks:
::			Manage known networks						ms-settings:network-wifisettings
::		Wi-Fi Calling									ms-settings:network-wificalling
::		Ethernet										ms-settings:network-ethernet
::		Dial-up											ms-settings:network-dialup
::		DirectAccess (where available)					ms-settings:network-directaccess
::		VPN												ms-settings:network-vpn
::		Airplane mode									ms-settings:network-airplanemode OR ms-settings:proximity
::		Mobile hotspot									ms-settings:network-mobilehotspot
::		NFC												ms-settings:nfctransactions
::		Data usage										ms-settings:datausage
::		Proxy											ms-settings:network-proxy

::	Personalization
::		Personalization									ms-settings:personalization
::		Background										ms-settings:personalization-background
::		Colors											ms-settings:personalization-colors OR ms-settings:colors
::		Lock screen										ms-settings:lockscreen
::		Themes											ms-settings:themes
::		Fonts (build 17083+)							ms-settings:fonts
::		Start											ms-settings:personalization-start
::			Choose which folders appear on Start		ms-settings:personalization-start-places
::		Taskbar											ms-settings:taskbar

::	Apps
::		Apps & features									ms-settings:appsfeatures OR ms-settings:appsfeatures-app
::			Manage optional features					ms-settings:optionalfeatures
::		Default apps									ms-settings:defaultapps
::			Offline maps								ms-settings:maps
::		Download maps									ms-settings:maps-downloadmaps
::		Apps for websites								ms-settings:appsforwebsites
::		Video playback (build 16215+)					ms-settings:videoplayback
::		Startup (build 17017+)							ms-settings:startupapps

::	Accounts
::		Your info										ms-settings:yourinfo
::		Email & accounts								ms-settings:emailandaccounts
::		Sign-in options									ms-settings:signinoptions
::			Windows Hello face setup					ms-settings:signinoptions-launchfaceenrollment
::			Windows Hello fingerprint setup				ms-settings:signinoptions-launchfingerprintenrollment
::			Security Key setup							ms-settings:signinoptions-launchsecuritykeyenrollment
::			Dynamic Lock								ms-settings:signinoptions-dynamiclock
::		Access work or school							ms-settings:workplace
::		Family & other people							ms-settings:otherusers OR ms-settings:family-group
::			Set up a kiosk								ms-settings:assignedaccess
::		Sync your settings								ms-settings:sync

::	Time & language
::		Date & time										ms-settings:dateandtime
::		Region											ms-settings:regionformatting
::		Japan IME settings (where available)			ms-settings:regionlanguage-jpnime
::		Pinyin IME settings (where available)			ms-settings:regionlanguage-chsime-pinyin
::		Wubi IME settings (where available)				ms-settings:regionlanguage-chsime-wubi
::		Korea IME settings (where available)			ms-settings:regionlanguage-korime
::		Language										ms-settings:regionlanguage OR ms-settings:regionlanguage-languageoptions
::			Windows Display language					ms-settings:regionlanguage-setdisplaylanguage
::			Add Display language						ms-settings:regionlanguage-adddisplaylanguage
::		Keyboard (removed in build 17083+)				ms-settings:keyboard
::		Speech											ms-settings:speech

::	Gaming
::		Game bar										ms-settings:gaming-gamebar
::		Captures										ms-settings:gaming-gamedvr
::		Broadcasting									ms-settings:gaming-broadcasting
::		Game Mode										ms-settings:gaming-gamemode
::		TruePlay (removed in build 1809+)				ms-settings:gaming-trueplay
::		Xbox Networking (build 16226+)					ms-settings:gaming-xboxnetworking

::	Extras
::	Extras (with Settings app extensions)				ms-settings:extras

::	Ease of Access
::		Display (build 17025+)							ms-settings:easeofaccess-display
::		Mouse Pointer (build 17040+)					ms-settings:easeofaccess-cursorandpointersize OR ms-settings:easeofaccess-MousePointer
::		Text Cursor										ms-settings:easeofaccess-cursor
::		Magnifier										ms-settings:easeofaccess-magnifier
::		Color Filters (build 17025+)					ms-settings:easeofaccess-colorfilter
::			Adaptive Color Filters Link					ms-settings:easeofaccess-colorfilter-adaptivecolorlink
::			Night Light Link							ms-settings:easeofaccess-colorfilter-bluelightlink
::		High Contrast									ms-settings:easeofaccess-highcontrast
::		Narrator										ms-settings:easeofaccess-narrator
::			Start Narratorafter sign-in for me			ms-settings:easeofaccess-narrator-isautostartenabled
::		Audio (build 17035+)							ms-settings:easeofaccess-audio
::		Closed captions									ms-settings:easeofaccess-closedcaptioning
::		Speech (build 17035+)							ms-settings:easeofaccess-speechrecognition
::		Keyboard										ms-settings:easeofaccess-keyboard
::		Mouse											ms-settings:easeofaccess-mouse
::		Eye Control (build 17035+)						ms-settings:easeofaccess-eyecontrol
::		Other options (removed in version 1809+)		ms-settings:easeofaccess-otheroptions

::	Search (version 1903+)
::		Permissions & history							ms-settings:search-permissions
::		Searching Windows								ms-settings:cortana-windowssearch
::		More details									ms-settings:search-moredetails

::	Cortana (build 16188+)
::		Cortana											ms-settings:cortana
::		Talk to Cortana									ms-settings:cortana-talktocortana
::		Permissions										ms-settings:cortana-permissions
::		More details									ms-settings:cortana-moredetails

::	Privacy
::		General											ms-settings:privacy
::		Speech											ms-settings:privacy-speech
::		Inking & typing personalization					ms-settings:privacy-speechtyping
::		Diagnostics & feedback							ms-settings:privacy-feedback
::			View Diagnostic Data						ms-settings:privacy-feedback-telemetryviewergroup
::		Activity history (build 17040+)					ms-settings:privacy-activityhistory
::		Location										ms-settings:privacy-location
::		Camera											ms-settings:privacy-webcam
::		Microphone										ms-settings:privacy-microphone
::		Voice activation								ms-settings:privacy-voiceactivation
::		Notifications									ms-settings:privacy-notifications
::		Account info									ms-settings:privacy-accountinfo
::		Contacts										ms-settings:privacy-contacts
::		Calendar										ms-settings:privacy-calendar
::		Phone calls (removed in build 1809+)			ms-settings:privacy-phonecalls
::		Call history									ms-settings:privacy-callhistory
::		Email											ms-settings:privacy-email
::		Eye tracker (requires eyetracker hardware)		ms-settings:privacy-eyetracker
::		Tasks											ms-settings:privacy-tasks
::		Messaging										ms-settings:privacy-messaging
::		Radios											ms-settings:privacy-radios
::		Other devices									ms-settings:privacy-customdevices
::		Background apps									ms-settings:privacy-backgroundapps
::		App diagnostics									ms-settings:privacy-appdiagnostics
::		Automatic file downloads						ms-settings:privacy-automaticfiledownloads
::		Documents										ms-settings:privacy-documents
::		Pictures										ms-settings:privacy-pictures
::		Videos											ms-settings:privacy-documents
::		File system										ms-settings:privacy-broadfilesystemaccess

::	Update & security
::		Windows Update									ms-settings:windowsupdate
::			Check for updates							ms-settings:windowsupdate-action
::			View update history							ms-settings:windowsupdate-history
::			Restart options								ms-settings:windowsupdate-restartoptions
::			Advanced options							ms-settings:windowsupdate-options
::			Change active hours							ms-settings:windowsupdate-activehours
::			Optional updates							ms-settings:windowsupdate-optionalupdates OR ms-settings:windowsupdate-seekerondemand
::		Delivery Optimization							ms-settings:delivery-optimization
::		Windows Security / Windows Defender				ms-settings:windowsdefender
::			Open Windows Security						windowsdefender:
::		Backup											ms-settings:backup
::		Troubleshoot									ms-settings:troubleshoot
::		Recovery										ms-settings:recovery
::		Activation										ms-settings:activation
::		Find My Device									ms-settings:findmydevice
::		For developers									ms-settings:developers
::		Windows Insider Program							ms-settings:windowsinsider OR ms-settings:windowsinsider-optin

::	Mixed reality
::		Mixed reality									ms-settings:holographic
::		Audio and speech								ms-settings:holographic-audio
::		Environment										ms-settings:privacy-holographic-environment
::		Headset display									ms-settings:holographic-headset
::		Uninstall										ms-settings:holographic-management

::	Surface Hub
::		Accounts										ms-settings:surfacehub-accounts
::		Team Conferencing								ms-settings:surfacehub-calling
::		Team device management							ms-settings:surfacehub-devicemanagenent
::		Session cleanup									ms-settings:surfacehub-sessioncleanup
::		Welcome screen									ms-settings:surfacehub-welcome
