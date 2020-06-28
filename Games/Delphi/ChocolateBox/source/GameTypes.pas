unit GameTypes;
{$I ..\..\..\Languages\Delphi\Utils\CoreOptions.inc}

{ Use this file to define constants and structures used across several modules }

interface

uses
  Windows;

const
  // Graphics constants
  GRID_SIZE_DEFAULT		= 3;
  GRID_SIZE_MAX			= 8;

type
  // Enumerations
  TGameBackground = (
	// Background image or solid colour
	eBackgroundNone = 0,
	eBackgroundFirst,
	eBackgroundImg1 = eBackgroundFirst,
	eBackgroundImg2,
	eBackgroundSolidColour,
	eBackgroundLast = eBackgroundSolidColour);

  TGameIconSet = (
	// Icons to use for the main grid
	eIconSetNone = 0,
	eIconSetFirst,
	eIconSetStd_64x64 = eIconSetFirst,
	eIconSetStd_128x128,
	eIconSetFruitSalad_64x64,
	eIconSetFruitSalad_128x128,
	eIconSetFuturama_128x128,
	eIconSetLast = eIconSetFuturama_128x128);

implementation

end.
