{ Console application to show some of the data types, including ranges, in Delphi 7 }
program DataTypes_D7;
{$I ..\..\..\Languages\Delphi\Utils\CoreOptions.inc}

// Note: APPTYPE is "GUI" by default; only needs to be set explicitly for console applications }
{$APPTYPE CONSOLE}

uses
  Windows, Math, SysUtils,
  ConsoleUtils in '..\..\..\Languages\Delphi\Utils\ConsoleUtils.pas',
  MinimalUnit  in '..\..\..\Languages\Delphi\Utils\MinimalUnit.pas';

var
	tab: Char;

procedure ShowTypesInteger();
var
	// Note: Local variables are not required, but demonstrate syntax or can be used for debugging
	shortMin, shortMax: ShortInt;
	byteMin, byteMax: BYTE;
	smallMin, smallMax: SmallInt;
	wordMin, wordMax: WORD;
	integerMin, integerMax: Integer;
	longIntMin, longIntMax: LongInt;
	cardinalMin, cardinalMax: Cardinal;
	longWordMin, longWordMax: LongWord;
	dwordMin, dwordMax: DWORD;
	int64Min, int64Max: Int64;
begin
	// Integers
	WriteLn('### Integer ###');
	WriteLn(Format('%s%s%sSize (bytes)%sMin%s%s%sMax', [tab, tab, tab, tab, tab, tab, tab]));

		begin
		// ShortInt, equivalent to C++ "__int8" (subrange type in Delphi)
		shortMin := Low(ShortInt);
		shortMax := High(ShortInt);
		WriteLn(Format('ShortInt (__int8)%s%d%s%s%d%s%s%s%d', [
			tab, SizeOf(ShortInt),
			tab, tab, shortMin,
			tab, tab, tab, shortMax]));
		end;

		begin
		// Byte, equivalent to C++ "unsigned __int8" and Delphi "Char" (subrange type in Delphi)
		byteMin := Low(BYTE);
		byteMax := High(BYTE);
		WriteLn(Format('Byte%s%s%s%d%s%s%d%s%s%s%d', [
			tab, tab, tab, SizeOf(BYTE),
			tab, tab, byteMin,
			tab, tab, tab, byteMax]));
		end;

		begin
		// SmallInt, equivalent to C++ "__int16" (subrange type in Delphi)
		smallMin := Low(SmallInt);
		smallMax := High(SmallInt);
		WriteLn(Format('SmallInt (__int16)%s%d%s%s%d%s%s%s%d', [
			tab, SizeOf(SmallInt),
			tab, tab, smallMin,
			tab, tab, tab, smallMax]));
		end;

		begin
		// Word, equivalent to C++ "unsigned __int16" (subrange type in Delphi)
		wordMin := Low(WORD);
		wordMax := High(WORD);
		WriteLn(Format('Word%s%s%s%d%s%s%d%s%s%s%d', [
			tab, tab, tab, SizeOf(WORD),
			tab, tab, wordMin,
			tab, tab, tab, wordMax]));
		end;

		begin
		// Integer, equivalent to C++ "__int32"
		integerMin := Low(Integer);
		integerMax := High(Integer);
		WriteLn(Format('Integer (__int32)%s%d%s%s%d%s%s%d', [
			tab, SizeOf(Integer),
			tab, tab, integerMin,
			tab, tab, integerMax]));
		end;

		begin
		// LongInt, appears to be an alias for "Integer" (see above)
		// Note: 4 bytes on 32-bit platforms and 64-bit Windows, but 8 bytes on 64-bit iOS
		longIntMin := Low(LongInt);
		longIntMax := High(LongInt);
		WriteLn(Format('LongInt%s%s%s%d%s%s%d%s%s%d', [
			tab, tab, tab, SizeOf(LongInt),
			tab, tab, longIntMin,
			tab, tab, longIntMax]));
		end;

		begin
		// Cardinal, equivalent to C++ "unsigned __int32"
		cardinalMin := Low(Cardinal);
		cardinalMax := High(Cardinal);
		WriteLn(Format('Cardinal (__int32)%s%d%s%s%d%s%s%s%s', [
			tab, SizeOf(Cardinal),
			tab, tab, cardinalMin,
			tab, tab, tab, IntToStr(cardinalMax)]));
		end;

		begin
		// LongWord, appears to be an alias for "Cardinal" (see above)
		// Note: 4 bytes on 32-bit platforms and 64-bit Windows, but 8 bytes on 64-bit iOS
		longWordMin := Low(LongWord);
		longWordMax := High(LongWord);
		WriteLn(Format('LongWord%s%s%d%s%s%d%s%s%s%s', [
			tab, tab, SizeOf(LongWord),
			tab, tab, longWordMin,
			tab, tab, tab, IntToStr(longWordMax)]));
		end;

		begin
		// Dword, appears to be an alias for "Cardinal" (see above)
		dwordMin := Low(DWORD);
		dwordMax := High(DWORD);
		WriteLn(Format('DWORD%s%s%s%d%s%s%d%s%s%s%s', [
			tab, tab, tab, SizeOf(DWORD),
			tab, tab, dwordMin,
			tab, tab, tab, IntToStr(dwordMax)]));
		end;

		begin
		// Int64, equivalent to C++ "__int64"
		int64Min := Low(Int64);
		int64Max := High(Int64);
		WriteLn(Format('Int64 (__int64)%s%s%d%s%s%d%s%s', [
			tab, tab, SizeOf(Int64),
			tab, tab, int64Min,
			tab, IntToStr(int64Max)]));
		end;

		begin
		// UInt64, equivalent to C++ "unsigned __int32"

		// Note,1: UInt64 range is 0..184467440737095516145 (from VS 2019)

		// Note,2: Delphi 7 does not have a "IntToStr" overload for unsigned Int64. To convert
		// UInt64 variables to a string, first cast to a signed Int64. Any values larger than the
		// max Int64 (9223372036854775807) will not be displayed correctly.

		// Note,3: Defining a variable as "Low(UInt64)" (or "High") causes compiler problems with
		// Delphi 7 so we use hard-coded min/max values here
		WriteLn(Format('UInt64%s%s%s%d%s%s0%s%s%s184467440737095516145', [
			tab, tab, tab, SizeOf(UInt64),
			tab, tab, tab,
			tab, tab]));
		end;

		begin
		// Pointer; 4 bytes on 32-bit and 8 bytes on 64-bit systems
		WriteLn(Format('Pointer%s%s%s%d', [tab, tab, tab, SizeOf(Pointer)]));
		end;

	// Other integer types not supported in Delphi 7 include:
	// * FixedInt, FixedUInt (4 bytes)
	// * NativeInt, NativeUInt (platform-dependent: 4 bytes in Delphi/32 and 8 bytes in Delphi/64)
	WriteLn('');
end;

procedure ShowTypesFloating();
var
	singleZero, singleMin, singleMax: Single;
	doubleZero, doubleMin, doubleMax: Double;
	extendedZero, extendedMin, extendedMax: Extended;
	currencyZero, currencyMin, currencyMax: Currency;
begin
	// Floating point numbers
	// Note: "MinSingle" and other constants are from the Math unit
	WriteLn('### Floating point ###');
	WriteLn(Format('%s%s%sSize (bytes)%sZero%s%s%sMin%s%sMax', [
		tab, tab, tab, tab, tab, tab, tab, tab, tab]));

		begin
		// Single, equivalent to C++ "float" (7 significant digits, exponent -38 to 38)

		// Note: SysUtils::FloatToStrF supports several formats
		//	* The following formats are noteworthy:
		//		- ffGeneral (converts to a fixed for scientific format number as required)
		//		- ffExponent (scientific format)
		//		- ffFixed (fixed format)
		//		- ffNumber (number format with thousand separators)
		//	* Precision should 7 (or less) for Single, 15 for Double and 18 for Extended types
		//	* For the "ffExponent" format, the Digits parameter is the minimum number of
		//		digits in the exponent (between 0 and 4)
		singleZero := MinSingle;
		singleMin := -1*MaxSingle;
		singleMax := MaxSingle;
		WriteLn(Format('Single (float)%s%s%d%s%s%s%s%s%s%s%s', [
			tab, tab, SizeOf(Single),
			tab, tab, FloatToStrF(singleZero, ffExponent, 7, 0),
			tab, tab, FloatToStrF(singleMin, ffExponent, 3, 0),
			tab, FloatToStrF(singleMax, ffExponent, 3, 0)]));
		end;

		begin
		// Double, equivalent to C++ "double" (15 significant digits, exponent -308 to 308)
		doubleZero := MinDouble;
		doubleMin := -1*MaxDouble;
		doubleMax := MaxDouble;
		WriteLn(Format('Double (double)%s%s%d%s%s%s%s%s%s%s', [
			tab, tab, SizeOf(Double),
			tab, tab, FloatToStrF(doubleZero, ffExponent, 15, 0),
			tab, FloatToStrF(doubleMin, ffExponent, 3, 0),
			tab, FloatToStrF(doubleMax, ffExponent, 3, 0)]));
		end;

		begin
		// Extended, specific to Delphi (19 significant digits, exponent -4932 to 4932)

		// 10 bytes on 32-bit and 8 bytes on 64-bit platforms. Intended to have greater precision
		// than "Double" (at least on 32-bit platforms). Less portable and should generally avoid.
		extendedZero := MinExtended;
		extendedMin := -1*MaxExtended;
		extendedMax := MaxExtended;
		WriteLn(Format('Extended%s%s%d%s%s%s%s%s%s%s%s', [
			tab, tab, SizeOf(Extended),
			tab, tab, FloatToStrF(extendedZero, ffExponent, 3, 0),
			tab, tab, FloatToStrF(extendedMin, ffExponent, 3, 0),
			tab, FloatToStrF(extendedMax, ffExponent, 3, 0)]));
		end;

		begin
		// Currency, specific to Delphi (50 significant digits, 4 fixed decimal places)

		// Intended for financial calculations to minimise rounding errors. Stored as a scaled
		// Int64 with at least 4 significant digits. When mixed with other floats, values are
		// automatically divided or multipled by 10000.
		currencyZero := 0;
		extendedMin := (Low(Int64) / 10000.0);
		currencyMin := extendedMin;
		extendedMax := (High(Int64) / 10000.0);
		currencyMax := extendedMax;
		WriteLn(Format('Currency%s%s%d%s%s%s%s%s%s%s%s', [
			tab, tab, SizeOf(Currency),
			tab, tab, CurrToStrF(currencyZero, ffFixed, 4),
			tab, tab, CurrToStr(currencyMin),
			tab, CurrToStr(currencyMax)]));
		end;

	// Other floating types not demonstrated include:
	// * Comp (which stands for "Computational"); 8 bytes; native to Intell architecture and
	//		maintained for backward compatibility. Avoid, and use Int64 instead.
	//		Min = MinComp = -9.223372036854775807e+18
	//		Max = MaxComp = 9.223372036854775807e+18
	// * Real; 8 bytes; maps to "Double"
	// * Real48; 6 bytes; obsolete Delphi-specific which should be avoided
	WriteLn('');
end;

procedure ShowTypesBoolean();
var
	boolMin, boolMax: Boolean;
	byteBoolMin, byteBoolMax: ByteBool;
	wordBoolMin, wordBoolMax: WordBool;
	longBoolMin, longBoolMax: LongBool;
begin
	// Boolean types
	WriteLn('### Boolean ###');
	WriteLn(Format('%s%s%sSize (bytes)%sMin%s%sMax', [tab, tab, tab, tab, tab, tab]));

	//		Boolean type		Compare			Ord(False)	Ord(True)	Succ(False)		Pred(True)
	// Boolean 					False < True		0			1		True			False
	// xxxBool (ByteBool, etc)	False <> True		0		  <> 0		True			False

	// "Boolean" is the preferred type. Others are used for compatibility with other languages and
	// operating system libraries.

		begin
		// Boolean, equivalent to C++ "bool"
		boolMin := False;
		boolMax := True;
		WriteLn(Format('Boolean (bool)%s%s%d%s%s%d%s%s%d', [
			tab, tab, SizeOf(Boolean),
			tab, tab, Ord(boolMin),
			tab, tab, Ord(boolMax)]));
		end;

		begin
		// ByteBool
		byteBoolMin := False;
		byteBoolMax := True;
		WriteLn(Format('ByteBool%s%s%d%s%s%d%s%s%d', [
			tab, tab, SizeOf(ByteBool),
			tab, tab, Ord(byteBoolMin),
			tab, tab, Ord(byteBoolMax)]));
		end;

		begin
		// WordBool
		wordBoolMin := False;
		wordBoolMax := True;
		WriteLn(Format('WordBool%s%s%d%s%s%d%s%s%d', [
			tab, tab, SizeOf(WordBool),
			tab, tab, Ord(wordBoolMin),
			tab, tab, Ord(wordBoolMax)]));
		end;

		begin
		// LongBool
		longBoolMin := False;
		longBoolMax := True;
		WriteLn(Format('LongBool%s%s%d%s%s%d%s%s%d', [
			tab, tab, SizeOf(LongBool),
			tab, tab, Ord(longBoolMin),
			tab, tab, Ord(longBoolMax)]));
		end;

	WriteLn('');
end;

procedure ShowTypesChar();
var
	charMin, charMax: Char;
	ansiCharMin, ansiCharMax: AnsiChar;
	wideCharMin, wideCharMax: WideChar;
begin
	// Text types
	WriteLn('### Character ###');
	WriteLn(Format('%s%s%sSize (bytes)%sMin%s%sMax', [tab, tab, tab, tab, tab, tab]));

		begin
		// Char, equivalent to C++ "char" (subrange type in Delphi, range is #0..#255)
		charMin := Low(Char);
		charMax := High(Char);
		WriteLn(Format('Char (char)%s%s%d%s%s%d%s%s%d', [
			tab, tab, SizeOf(Char),
			tab, tab, Ord(charMin),
			tab, tab, Ord(charMax)]));
		end;

		begin
		// AnsiChar, alias for "Char" (see above)
		// Note: Not supported in Delphi mobile compilers (use "array of Byte" or "Char" instead)
		ansiCharMin := Low(AnsiChar);
		ansiCharMax := High(AnsiChar);
		WriteLn(Format('AnsiChar%s%s%d%s%s%d%s%s%d', [
			tab, tab, SizeOf(AnsiChar),
			tab, tab, Ord(ansiCharMin),
			tab, tab, Ord(ansiCharMax)]));
		end;

		begin
		// WideChar, equivalent to C++ "wchar_t" or "char16_t"
		wideCharMin := Low(WideChar);
		wideCharMax := High(WideChar);
		WriteLn(Format('WideChar (wchar_t)%s%d%s%s%d%s%s%d', [
			tab, SizeOf(WideChar),
			tab, tab, Ord(wideCharMin),
			tab, tab, Ord(wideCharMax)]));
		end;

	WriteLn('');
end;

procedure ShowTypesString();
var
	shortStringExample: ShortString;
	stringExample: String;
	wideStringExample: WideString;
begin
	// Text types
	WriteLn('### String (all examples are "Hello world") ###');
	WriteLn(Format('%s%s%sSize (bytes)%sExample length', [tab, tab, tab, tab]));

		begin
		// ShortString, alias for String[255]
		shortStringExample := 'Hello world';
		WriteLn(Format('ShortString%s%s%d%s%s%d', [
			tab, tab, SizeOf(ShortString),
			tab, tab, Length(shortStringExample)]));
		end;

		begin
		// String. Occupies 4 bytes on 32-bit and 8 bytes on 64-bit platforms which is a pointer to
		// dynamically-allocated string.
		stringExample := 'Hello world';
		WriteLn(Format('String%s%s%s%d%s%s%d', [
			tab, tab, tab, SizeOf(String),
			tab, tab, Length(stringExample)]));
		end;

		begin
		// WideString (see String)
		wideStringExample := 'Hello world';
		WriteLn(Format('WideString%s%s%d%s%s%d', [
			tab, tab, SizeOf(WideString),
			tab, tab, Length(wideStringExample)]));
		end;

	// Other string types not supported in Delphi 7 include:
	// * UnicodeString
	WriteLn('');
end;

begin
	// Welcome message
	WriteLn('Data types in Delphi 7');
	WriteLn('');

	// Define a TAB character
	tab := Chr(9);

	// Show information about data types. Some references for Delphi data types:
	// * http://docwiki.embarcadero.com/RADStudio/Rio/en/ _
	//		64-bit_Windows_Data_Types_Compared_to_32-bit_Windows_Data_Types
	// * http://www.delphibasics.co.uk/Article.asp?Name=DataTypes
	// * http://www.festra.com/eng/ref-numbers.htm
	ShowTypesInteger();
	ShowTypesFloating();
	ShowTypesBoolean();
	ShowTypesChar();
	ShowTypesString();

	WriteLn('Finished...press any key to exit');
	while (not IsConsoleKeyPressed()) do;
end.
