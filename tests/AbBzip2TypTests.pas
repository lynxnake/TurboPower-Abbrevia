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

{$I AbDefine.inc}

unit AbBzip2TypTests;

interface

uses
  Classes, AbArcTypTests, AbArcTyp;

type
  TAbBzip2ArchiveTests = class(TAbArchiveTests)
  protected
    class function ArchiveClass: TAbArchiveClass; override;
    class function ArchiveExt: string; override;
  published
    procedure TestVerifyBzip2;
  end;

  TAbBzippedTarArchiveTests = class(TAbArchiveMultiFileTests)
  protected
    class function ArchiveClass: TAbArchiveClass; override;
    class function ArchiveExt: string; override;
    function CreateArchive(const aFileName : string; aMode : Word): TAbArchive;
      override;
    function CreateArchive(aStream : TStream; const aArchiveName : string): TAbArchive;
      override;
  end;

implementation

uses
  SysUtils, TestFrameWork, AbBzip2Typ, AbUtils;

{----------------------------------------------------------------------------}
{ TAbBzip2ArchiveTests }
{----------------------------------------------------------------------------}
class function TAbBzip2ArchiveTests.ArchiveClass: TAbArchiveClass;
begin
  Result := TAbBzip2Archive;
end;
{ -------------------------------------------------------------------------- }
class function TAbBzip2ArchiveTests.ArchiveExt: string;
begin
  Result := '.bz2';
end;
{ -------------------------------------------------------------------------- }
procedure TAbBzip2ArchiveTests.TestVerifyBzip2;
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(MPLDir + 'MPL.bz2', fmOpenRead or fmShareDenyNone);
  try
    Check(VerifyBzip2(FS) = atBzip2, 'VerifyBzip2 failed on valid BZ2');
  finally
    FS.Free;
  end;
  FS := TFileStream.Create(CanterburyDir + 'Canterbury.tbz', fmOpenRead or fmShareDenyNone);
  try
    Check(VerifyBzip2(FS) = atBzippedTar, 'VerifyBzip2 failed on .TAR.BZ2');
  finally
    FS.Free;
  end;
  FS := TFileStream.Create(MPLDir + 'MPL.cab', fmOpenRead or fmShareDenyNone);
  try
    Check(VerifyBzip2(FS) = atUnknown, 'VerifyBzip2 succeeded on CAB');
  finally
    FS.Free;
  end;
end;

{----------------------------------------------------------------------------}
{ TAbBzippedTarArchiveTests }
{----------------------------------------------------------------------------}
class function TAbBzippedTarArchiveTests.ArchiveClass: TAbArchiveClass;
begin
  Result := TAbBzip2Archive;
end;
{----------------------------------------------------------------------------}
class function TAbBzippedTarArchiveTests.ArchiveExt: string;
begin
  Result := '.tbz';
end;
{----------------------------------------------------------------------------}
function TAbBzippedTarArchiveTests.CreateArchive(const aFileName : string;
  aMode : Word): TAbArchive;
begin
  Result := inherited CreateArchive(aFileName, aMode);
  TAbBzip2Archive(Result).TarAutoHandle := True;
  TAbBzip2Archive(Result).IsBzippedTar := True;
end;
{----------------------------------------------------------------------------}
function TAbBzippedTarArchiveTests.CreateArchive(aStream : TStream;
  const aArchiveName : string): TAbArchive;
begin
  Result := inherited CreateArchive(aStream, aArchiveName);
  TAbBzip2Archive(Result).TarAutoHandle := True;
  TAbBzip2Archive(Result).IsBzippedTar := True;
end;
{----------------------------------------------------------------------------}

initialization

  TestFramework.RegisterTest('Abbrevia.Low-Level Compression Test Suite',
    TAbBzip2ArchiveTests.Suite);

end.
