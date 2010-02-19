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

 unit AbBzip2TypTests;

{$I AbDefine.inc}

interface

uses
  SysUtils, Classes, TestFrameWork, AbTestFrameWork, AbBzip2Typ;

type
  TAbBzip2ArchiveTests = class(TAbTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure TestVerifyBzip2;
    procedure TestExtractToStream;
    procedure TestExtractCanterbury;
    procedure TestCompressCanterbury;
  end;

implementation

uses
  AbUtils;

{----------------------------------------------------------------------------}
{ TAbBzip2ArchiveTests }
{----------------------------------------------------------------------------}
procedure TAbBzip2ArchiveTests.SetUp;
begin
  inherited;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBzip2ArchiveTests.TearDown;
begin
  inherited;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBzip2ArchiveTests.TestVerifyBzip2;
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(TestFileDir + 'MPL.BZ2', fmOpenRead or fmShareDenyNone);
  try
    Check(VerifyBzip2(FS) = atBzip2, 'VerifyBzip2 failed on valid BZ2');
  finally
    FS.Free;
  end;
  FS := TFileStream.Create(TestFileDir + 'Canterbury' + PathDelim + 'Canterbury.tbz', fmOpenRead or fmShareDenyNone);
  try
    Check(VerifyBzip2(FS) = atBzippedTar, 'VerifyBzip2 failed on .TAR.BZ2');
  finally
    FS.Free;
  end;
  FS := TFileStream.Create(TestFileDir + 'MPL.CAB', fmOpenRead or fmShareDenyNone);
  try
    Check(VerifyBzip2(FS) = atUnknown, 'VerifyBzip2 succeeded on CAB');
  finally
    FS.Free;
  end;
end;
{----------------------------------------------------------------------------}
procedure TAbBzip2ArchiveTests.TestExtractToStream;
var
  Bz2: TAbBzip2Archive;
  Stream: TMemoryStream;
begin
  Bz2 := TAbBzip2Archive.Create(TestFileDir + 'MPL.BZ2', fmOpenRead);
  try
    Bz2.Load;
    Stream := TMemoryStream.Create;
    try
      Bz2.ExtractToStream(Bz2[0].FileName, Stream);
      CheckFileMatchesStream(TestFileDir + 'MPL-1_1.txt', Stream);
    finally
      Stream.Free;
    end;
  finally
    Bz2.Free;
  end;
end;
{----------------------------------------------------------------------------}
procedure TAbBzip2ArchiveTests.TestExtractCanterbury;
var
  SourceDir: string;
  Bz2: TAbBzip2Archive;
begin
  SourceDir := TestFileDir + 'Canterbury' + PathDelim;
  Bz2 := TAbBzip2Archive.Create(SourceDir + 'Canterbury.tbz', fmOpenRead);
  try
    Bz2.TarAutoHandle := True;
    Bz2.IsBzippedTar := True;
    Bz2.BaseDirectory := TestTempDir;
    Bz2.Load;
    Bz2.ExtractFiles('*');
  finally
    Bz2.Free;
  end;
  CheckDirMatch(SourceDir + 'source', TestTempDir, False);
end;
{----------------------------------------------------------------------------}
procedure TAbBzip2ArchiveTests.TestCompressCanterbury;
var
  SourceDir: string;
  Bz2: TAbBzip2Archive;
begin
  SourceDir := TestFileDir + 'Canterbury' + PathDelim + 'source';
  Bz2 := TAbBzip2Archive.Create(TestTempDir + 'Test.tbz', fmCreate);
  try
    Bz2.TarAutoHandle := True;
    Bz2.IsBzippedTar := True;
    Bz2.BaseDirectory := SourceDir;
    Bz2.AddFiles('*', faAnyFile - faDirectory);
    Bz2.Save;
  finally
    Bz2.Free;
  end;
  Bz2 := TAbBzip2Archive.Create(TestTempDir + 'Test.tbz', fmOpenRead);
  try
    Bz2.TarAutoHandle := True;
    Bz2.IsBzippedTar := True;
    CreateDir(TestTempDir + 'test');
    Bz2.BaseDirectory := TestTempDir + 'test';
    Bz2.Load;
    Bz2.ExtractFiles('*');
  finally
    Bz2.Free;
  end;
  CheckDirMatch(SourceDir, TestTempDir + 'test', False);
end;
{----------------------------------------------------------------------------}

initialization

  TestFramework.RegisterTest('Abbrevia.Low-Level Compression Test Suite',
    TAbBzip2ArchiveTests.Suite);

end.
