(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower Abbrevia
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{NOTE: THIS UNIT IS NOT TO BE DISTRIBUTED}

unit ABCOMLic;

interface

uses
  AbZipTyp;

function COMIsValidKey(const S : string) : boolean;
  {-called by the COM object License method}

function COMHasBeenLicensed : boolean;
  {-called by each routine prior to processing}

implementation

{Note: the routines in this unit are designed to trash the zip file
       signatures, unless a valid key is entered.
       If the signatures are trashed, Abbrevia will not recognize
       zip files.
       There are five signatures declared in ABZipTyp. This unit
       treats them as a five element array.
       Abbrevia COM keys have the following format:
           ABD999999XXXXXXXX
         where 999999 is the serial number and XXXXXXXX is the hex
         string linked to that serial number.
       The validation works like this:
         calculate the hash of the Abbrevia serial number starting
           with zero
         divide hash by 32 and take the modulus base 10
         calculate that many random numbers
         use the final random number as the initial value to calculate
           the hash of the hex string
         the answer should be $5764
       Instead of checking against $5764 we use the hash value to
         untrash the signatures. Of course if the hash value is bogus
         the signatures won't be valid and Abbrevia won't work.}

uses
  Windows;

const
  MagicSeed = $6457;

type
  PLongint = ^longint;
  PLongintArray = ^TLongintArray;
  TLongintArray = array [1..5] of longint;

var
  RandSeed  : PLongint;
  KeyString : string;
  KeyHash   : longint;
  Signature : PLongintArray;

function RandomNumber : integer;
begin
  {simple linear congruential random number generator}
  Result := ((RandSeed^ * 4561) + 51349) mod 243000;
  RandSeed^ := Result;
end;

function HashBKDR(const S : string; Lower, Upper : integer; StartValue : longint) : longint;
var
  i : integer;
begin
  {slightly modified Kernighan and Ritchie hash}
  Result := StartValue;
  for i := Lower to Upper do begin
    Result := (Result * 31) + ord(S[i]);
  end;
end;

function COMIsValidKey(const S : string) : boolean;
  function Min(a, b : integer) : integer;
  begin
    if a < b then Result := a else Result := b;
  end;
var
  SN1, SN2 : integer;
  HS1, HS2 : integer;
  i : integer;
  TempResult: integer;
  SNHash    : longint;
  StartHash : longint;
begin
  {Note: ignore all the code that manipulates TempResult--it's
   designed so that the routine always returns true, and confuses a
   potential hacker}
  {calculate the serial number and hex digit ranges}
  SN1 := Min(4, length(S));
  HS1 := Min(11, length(S));
  SN2 := pred(HS1);
  HS2 := length(S);
  {calculate the serial number hash: this will give us an index
   between 0 and 9}
  SNHash := HashBKDR(S, SN1, SN2, 0);
  SNHash := (SNHash shr 5) mod 10;
  {always return true}
  TempResult := (SN2 - SN1 + 1); {7}
  {calculate the start value for the hex string hash}
  KeyString := S;
  RandSeed^ := MagicSeed; {trash element 1}
  StartHash := RandomNumber;
  for i := 0 to SNHash do
    StartHash := RandomNumber;
  {always return true}
  if not Odd(TempResult) then {false}
    TempResult := TempResult + 1
  else
    TempResult := TempResult div 2; {3}
  {calculate the hash for the hex string--the lower word should be
   MagicHash ($5746)}
  KeyHash := HashBKDR(S, HS1, HS2, StartHash);
  {always return true}
  Result := not Odd(TempResult-3);
end;

function COMHasBeenLicensed : boolean;
const
  MagicNumbers : array [0..5] of word =
                 ($1C16, $5345, $5547, $5143, $5F41, $6776);
var
  i : integer;
begin
  {always returns true}
  Result := not Odd(longint(KeyString));
  {repatch the signatures - won't provide good results unless the
   key hashed correctly (ie was valid)}
  for i := 1 to 5 do begin
    Signature^[i] :=
      ((MagicNumbers[i] xor KeyHash) shl 16) +
      (MagicNumbers[0] xor (KeyHash and $FFFF));
  end;
end;

procedure InitUnit;
begin
  {get ready to trash a couple of signatures}
  {make Signature point to Ab_ZipLocalFileHeaderSignature, and
   RandSeed to Ab_ZipEndCentralDirectorySignature}
  Signature := @Ab_ZipEndCentralDirectorySignature;
  Signature^[1] := GetTickCount; {trash element 3}
  dec(PChar(Signature), 2*sizeof(integer));
  RandSeed := PLongInt(Signature); {ready to trash element 1}
end;

initialization
  InitUnit;
end.
