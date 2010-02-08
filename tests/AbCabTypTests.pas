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

 unit AbCabTypTests;

{$I AbDefine.inc}

interface

uses
  Windows, SysUtils, Classes, TestFrameWork, AbTestFrameWork, AbCabTyp;

type
  TAbCabArchiveTests = class(TAbTestCase)
  private
    procedure TestCompressDir(const aSourceDir: string);

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure TestVerifyCab;
    procedure TestExtract;
    procedure TestExtractToStream;
    procedure TestCompressCanterbury;
    {$IFDEF UNICODE}
    procedure TestCompressUnicode;
    {$ENDIF}
  end;

implementation

uses
  AbUtils;

{----------------------------------------------------------------------------}
{ TAbCabArchiveTests }
{----------------------------------------------------------------------------}
procedure TAbCabArchiveTests.SetUp;
begin
  inherited;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCabArchiveTests.TearDown;
begin
  inherited;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCabArchiveTests.TestVerifyCab;
begin
  Check(VerifyCab(TestFileDir + 'MPL.CAB') = atCab, 'VerifyCab failed on valid CAB');
  Check(VerifyCab(TestFileDir + 'MPL.ZIP') = atUnknown, 'VerifyCab succeeded on ZIP');
end;
{----------------------------------------------------------------------------}
procedure TAbCabArchiveTests.TestExtract;
var
  Cab: TAbCabArchive;
begin
  Cab := TAbCabArchive.Create(TestFileDir + 'MPL.CAB', fmOpenRead);
  try
    Cab.Load;
    Cab.ExtractAt(0, TestTempDir + 'MPL-1_1.TXT');
    CheckFilesMatch(TestFileDir + 'MPL-1_1.TXT', TestTempDir + 'MPL-1_1.TXT', '');
  finally
    Cab.Free;
  end;
end;
{----------------------------------------------------------------------------}
procedure TAbCabArchiveTests.TestExtractToStream;
var
  Cab: TAbCabArchive;
  Stream: TMemoryStream;
begin
  Cab := TAbCabArchive.Create(TestFileDir + 'MPL.CAB', fmOpenRead);
  try
    Cab.Load;
    Stream := TMemoryStream.Create;
    try
      Cab.ExtractToStream('MPL-1_1.TXT', Stream);
      CheckFileMatchesStream(TestFileDir + 'MPL-1_1.TXT', Stream);
    finally
      Stream.Free;
    end;
  finally
    Cab.Free;
  end;
end;
{----------------------------------------------------------------------------}
procedure TAbCabArchiveTests.TestCompressDir(const aSourceDir: string);
var
  Cab: TAbCabArchive;
  FileName: string;
begin
  FileName := TestTempDir + 'test.cab';
  Cab := TAbCabArchive.Create(FileName, fmCreate);
  try
    Cab.BaseDirectory := aSourceDir;
    Cab.Load; // TODO: This shouldn't be necessary
    Cab.AddFiles('*', faAnyFile);
    Cab.Save;
  finally
    Cab.Free;
  end;
  Cab := TAbCabArchive.Create(FileName, fmOpenRead);
  try
    CreateDir(TestTempDir + 'test');
    Cab.BaseDirectory := TestTempDir + 'test';
    Cab.Load;
    Cab.ExtractFiles('*');
  finally
    Cab.Free;
  end;
  CheckDirMatch(aSourceDir, TestTempDir + 'test', False);
end;
{----------------------------------------------------------------------------}
procedure TAbCabArchiveTests.TestCompressCanterbury;
begin
  TestCompressDir(TestFileDir + 'Canterbury' + PathDelim + 'source');
end;
{----------------------------------------------------------------------------}
{$IFDEF UNICODE}
procedure TAbCabArchiveTests.TestCompressUnicode;
begin
  TestCompressDir(TestFileDir + 'Unicode' + PathDelim + 'source');
end;
{$ENDIF}
{----------------------------------------------------------------------------}

initialization

  TestFramework.RegisterTest('Abbrevia.Low-Level Compression Test Suite',
    TAbCabArchiveTests.Suite);

end.
