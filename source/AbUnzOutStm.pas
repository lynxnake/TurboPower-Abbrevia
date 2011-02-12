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
 * Craig Peterson
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* ABBREVIA: AbUnzOutStm.pas 3.05                        *}
{*********************************************************}
{* ABBREVIA: UnZip output stream;  progress and CRC32    *}
{*********************************************************}

{$I AbDefine.inc}

unit AbUnzOutStm;

interface

uses
  SysUtils, Classes, AbArcTyp;

type
  TAbUnzipOutputStream = class( TStream )
  private
    FBytesWritten : Int64;
    FCRC32 : LongInt;
    FCurrentProgress : Byte;
    FStream : TStream;
    FUncompressedSize : Int64;
    FOnProgress : TAbProgressEvent;

    function GetCRC32 : LongInt;

  public
    constructor Create(aStream : TStream);

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;

    property CRC32 : LongInt
      read GetCRC32;
    property Stream : TStream
      read FStream
      write FStream;
    property UncompressedSize : Int64
      read FUncompressedSize
      write FUncompressedSize;
    property OnProgress : TAbProgressEvent
      read FOnProgress
      write FOnProgress;
  end;


implementation

uses
  AbExcept, AbUtils;

{ TAbUnzipOutputStream implementation ====================================== }

{ -------------------------------------------------------------------------- }
constructor TAbUnzipOutputStream.Create(aStream: TStream);
begin
  inherited Create;
  FStream := aStream;
  FCRC32 := -1;
end;
{ -------------------------------------------------------------------------- }
function TAbUnzipOutputStream.Read(var Buffer; Count: Integer): Longint;
begin
  raise EAbException.Create('TAbUnzipOutputStream.Read not supported');
end;
{ -------------------------------------------------------------------------- }
function TAbUnzipOutputStream.Write(const Buffer; Count: Longint): Longint;
var
  Abort : Boolean;
  NewProgress : Byte;
begin
  Result := FStream.Write(Buffer, Count);

  AbUpdateCRC( FCRC32, Buffer, Count );

  Inc( FBytesWritten, Result );
  if Assigned( FOnProgress ) then begin
    Abort := False;
    NewProgress := AbPercentage(FBytesWritten, FUncompressedSize);
    if (NewProgress <> FCurrentProgress) then begin
      FOnProgress( NewProgress, Abort );
      FCurrentProgress := NewProgress;
    end;
    if Abort then
      raise EAbUserAbort.Create;
  end;
end;
{ -------------------------------------------------------------------------- }
function TAbUnzipOutputStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := FStream.Seek(Offset, Origin);
end;
{ -------------------------------------------------------------------------- }
function TAbUnzipOutputStream.GetCRC32: LongInt;
begin
  Result := not FCRC32;
end;

end.

