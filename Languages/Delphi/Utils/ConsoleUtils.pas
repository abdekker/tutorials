{ Utilities designed for console applications, where $APPTYPE=CONSOLE is defined }
unit ConsoleUtils;

interface

uses
  SysUtils;
  
const
  DUMMY_INTERFACE_CONSTANT = 0;
  
type
  PDummyInterfacePointer = ^Integer;
  
function IsConsoleKeyPressed() : Boolean;
  
implementation

uses
  Windows;
  
const
  DUMMY_IMPLEMENTATION_CONSTANT = 0;
  
type
  PDummyImplementationPointer = ^Integer;

function IsConsoleKeyPressed() : Boolean;
var
	lpNumberOfEvents: DWORD;
	lpBuffer: TInputRecord;
	lpNumberOfEventsRead: DWORD;
	nStdHandle: THandle;
begin
	// Detects whether a key has been pressed. Adapted from:
	// https://stackoverflow.com/questions/5845080

	// To use:
	//		while (not IsConsoleKeyPressed()) do;
	Result := False;

	// Get the console handle
	nStdHandle := GetStdHandle(STD_INPUT_HANDLE);
	lpNumberOfEvents:=0;

	{ Each console has an input buffer that contains a queue of input event records. When a
	console's window has the keyboard focus, each input event (such as a single keystroke, a mouse
	movement, or a mouse-button click) is formatted as an input record and placed in the console's
	input buffer. }

	// Get the number of events
	GetNumberOfConsoleInputEvents(nStdHandle, lpNumberOfEvents);
	if (lpNumberOfEvents <> 0) then
		 begin
		// Retrieve the event
		PeekConsoleInput(nStdHandle, lpBuffer, 1, lpNumberOfEventsRead);
		if (lpNumberOfEventsRead <> 0) then
			begin
			// Is this a Keyboard event?
			if (lpBuffer.EventType = KEY_EVENT) then
				begin
				// Yes! Was the key pressed?
				if lpBuffer.Event.KeyEvent.bKeyDown then
					Result := True
				else
					FlushConsoleInputBuffer(nStdHandle);
				end
			else
				FlushConsoleInputBuffer(nStdHandle);
			end;
		end;
end;

end.
