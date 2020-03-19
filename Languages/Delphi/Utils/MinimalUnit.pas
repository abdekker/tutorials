{ Example of a minimal Delphi unit }
unit MinimalUnit;
{$I CoreOptions.inc}

interface
  // Public section (types, constants, methods that the unit offers to the outside world)

const
  PUBLIC_CONST = 7;		// Public => available to any unit

type
  // TRandomNumber is public (so gets defined globally)
  TRandomNumber = Integer;

  // A list of procedure/function signatures makes them useable from outside of the unit
  function getRandomNumber() : TRandomNumber;

implementation
  // Private section (implementation of public methods and optional internal data/methods)

// See the "initialization" section for a note about this line
//uses SysUtils;

const
  PRIVATE_CONST = 3;	// Private => available within this unit only
  
type
  // TSomethingElse is private (so can only be used within this unit only)
  TSomethingElse = Integer;

var
  // Private variables are only modifiable from inside this unit
  myRandomNumber: TRandomNumber;

// Implementation of functions listed in the "interface" section
function getRandomNumber(): TRandomNumber;
begin
	getRandomNumber := myRandomNumber;
end;

// "initialization" is executed when this unit is loaded/included (optional)
initialization
begin
	// Say hello in DEBUG mode
	{$IFDEF DBG} WriteLn('DBG: Hello from MinimalUnit'); {$ENDIF}

	// Choose a random number
	myRandomNumber :=
		{$IF Defined(DBG)} PRIVATE_CONST {$ELSEIF Defined(NDBG)} PUBLIC_CONST {$ELSE} 11 {$IFEND};

	// Uncomment to see what "myRandomNumber" is (requires "uses SysUtils" be uncommented as well)
	//WriteLn(Format('Random number is: %d', [myRandomNumber]));
end;

// "finalization" is executed when the programs ends (optional; if included, must be preceded by
// an "initialization" section)
finalization
begin
	// Say goodbye in DEBUG mode
	{$IFDEF DBG} WriteLn('DBG: Goodbye from MinimalUnit'); {$ENDIF}
end;

end.