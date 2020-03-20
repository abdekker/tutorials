{ General system utilities designed for Windows applications }
unit SystemUtils;

interface

uses
  Windows, StdCtrls;

const
  DUMMY_INTERFACE_CONSTANT = 0;

type
  PDummyInterfacePointer = ^Integer;

// Public methods

// System
procedure SaveToClipboard(const cstrText: String);

implementation

uses
  Clipbrd;

// Start: Public methods
// System
procedure SaveToClipboard(const cstrText: String);
begin
	// Save some text to the clipboard. Usually only used for development purposes.
	Clipboard.AsText := cstrText;
end;
// End: Private methods

end.
