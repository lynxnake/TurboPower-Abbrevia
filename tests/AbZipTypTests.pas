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

 unit AbZipTypTests;

{$I AbDefine.inc}

interface

uses
  SysUtils, Classes, TestFrameWork, AbTestFrameWork, AbArcTyp, AbZipTyp;

type
  TAbZipArchiveTests = class(TAbTestCase)
  private
    class function DecompressSuite(const aDir: string): ITestSuite;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  public
    class function Suite: ITestSuite; override;

  published
    procedure TestVerifyZip;
  end;

  TAbZipDecompressTest = class(TAbTestCase)
  private
    FFileName: string;
    procedure ExtractHelper(Sender : TObject; Item : TAbArchiveItem;
      const NewName : string);
  public
    constructor Create(const aFileName: string); reintroduce;
  published
    procedure TestDecompress;
  end;

  TAbZipCompressTest = class(TAbTestCase)
  private
    FMethod: TAbZipSupportedMethod;
    FPassword: AnsiString;
    FSourceDir: string;
    procedure ExtractHelper(Sender : TObject; Item : TAbArchiveItem;
      const NewName : string);
    procedure InsertHelper(Sender : TObject; Item : TAbArchiveItem;
      OutStream : TStream);
  public
    constructor Create(const aTestName, aSourceDir: string;
      aMethod: TAbZipSupportedMethod; const aPassword: AnsiString); reintroduce;
  published
    procedure TestCompress;
  end;

implementation

uses
  AbUtils, AbUnzPrc, AbZipPrc;

{----------------------------------------------------------------------------}
{ TAbZipArchiveTests }
{----------------------------------------------------------------------------}
class function TAbZipArchiveTests.DecompressSuite(const aDir: string): ITestSuite;
var
  SR: TSearchRec;
  Dir: string;
begin
  Dir := ExcludeTrailingPathDelimiter(aDir);
  Result := TTestSuite.Create('Decompress ' + ExtractFileName(Dir));
  if FindFirst(aDir + PathDelim + '*.zip', faAnyFile, SR) = 0 then
    try
      repeat
        Result.AddTest(TAbZipDecompressTest.Create(Dir + PathDelim + SR.Name));
      until FindNext(SR) <> 0;
    finally
      FindClose(SR);
    end;
end;
{ -------------------------------------------------------------------------- }
class function TAbZipArchiveTests.Suite: ITestSuite;
var
  CompressSuite: ITestSuite;
begin
  Result := inherited Suite;
  {$IF DEFINED(UNICODE) OR NOT DEFINED(MSWINDOWS)}
  // Test compression/decompression of Unicode filenames
  Result.AddSuite(DecompressSuite(TestFileDir + 'Unicode'));
  Result.AddTest(
    TAbZipCompressTest.Create('Compress Unicode',
      TestFileDir + 'Unicode' + PathDelim + 'source', smStored, ''));
  {$IFEND}

  // Test compression/decompression of Canterbury corpus
  Result.AddSuite(DecompressSuite(CanterburyDir));
  CompressSuite := TTestSuite.Create('Compress Canterbury');
  CompressSuite.AddTest(
    TAbZipCompressTest.Create('Store', CanterburySourceDir, smStored, ''));
  CompressSuite.AddTest(
    TAbZipCompressTest.Create('Deflate', CanterburySourceDir, smDeflated, ''));
  CompressSuite.AddTest(
    TAbZipCompressTest.Create('StoreP', CanterburySourceDir, smStored, 'password'));
  CompressSuite.AddTest(
    TAbZipCompressTest.Create('DeflateP', CanterburySourceDir, smDeflated, 'password'));
  Result.AddSuite(CompressSuite);
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipArchiveTests.SetUp;
begin
  inherited;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipArchiveTests.TearDown;
begin
  inherited;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipArchiveTests.TestVerifyZip;
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(TestFileDir + 'MPL.ZIP', fmOpenRead or fmShareDenyNone);
  try
    Check(VerifyZip(FS) = atZip, 'VerifyZip failed on valid ZIP');
  finally
    FS.Free;
  end;
  FS := TFileStream.Create(TestFileDir + 'MPL.CAB', fmOpenRead or fmShareDenyNone);
  try
    Check(VerifyZip(FS) = atUnknown, 'VerifyZip succeeded on CAB');
  finally
    FS.Free;
  end;
end;

{----------------------------------------------------------------------------}
{ TAbZipDecompressTest }
{----------------------------------------------------------------------------}
constructor TAbZipDecompressTest.Create(const aFileName: string);
begin
  inherited Create('TestDecompress');
  FTestName := ChangeFileExt(ExtractFileName(aFileName), '');
  FFileName := aFileName;
end;
{----------------------------------------------------------------------------}
procedure TAbZipDecompressTest.ExtractHelper(Sender : TObject;
  Item : TAbArchiveItem; const NewName : string);
begin
  AbUnzip(Sender, TAbZipItem(Item), NewName);
end;
{----------------------------------------------------------------------------}
procedure TAbZipDecompressTest.TestDecompress;
var
  Zip: TAbZipArchive;
begin
  Zip := TAbZipArchive.Create(FFileName, fmOpenRead or fmShareDenyNone);
  try
    Zip.ExtractHelper := ExtractHelper;
    Zip.BaseDirectory := TestTempDir;
    Zip.Password := 'password';
    Zip.Load;
    Zip.ExtractFiles('*');
  finally
    Zip.Free;
  end;
  CheckDirMatch(ExtractFilePath(FFileName) + 'source', TestTempDir, False);
end;

{----------------------------------------------------------------------------}
{ TAbZipCompressTest }
{----------------------------------------------------------------------------}
constructor TAbZipCompressTest.Create(const aTestName, aSourceDir: string;
  aMethod: TAbZipSupportedMethod; const aPassword: AnsiString);
begin
  inherited Create('TestCompress');
  FTestName := aTestName;
  FSourceDir := aSourceDir;
  FMethod := aMethod;
  FPassword := aPassword;
end;
{----------------------------------------------------------------------------}
procedure TAbZipCompressTest.ExtractHelper(Sender : TObject;
  Item : TAbArchiveItem; const NewName : string);
begin
  AbUnzip(Sender, TAbZipItem(Item), NewName);
end;
{----------------------------------------------------------------------------}
procedure TAbZipCompressTest.InsertHelper(Sender : TObject;
  Item : TAbArchiveItem; OutStream : TStream);
begin
  AbZip(TAbZipArchive(Sender), TAbZipItem(Item), OutStream);
end;
{----------------------------------------------------------------------------}
procedure TAbZipCompressTest.TestCompress;
var
  Zip: TAbZipArchive;
  TargetDir: string;
begin
  Zip := TAbZipArchive.Create(TestTempDir + 'test.zip', fmCreate);
  try
    Zip.CompressionMethodToUse := FMethod;
    Zip.InsertHelper := InsertHelper;
    Zip.BaseDirectory := FSourceDir;
    Zip.Password := FPassword;
    Zip.AddFiles('*', faAnyFile);
    Zip.Save;
  finally
    Zip.Free;
  end;
  TargetDir := TestTempDir + 'test';
  CreateDir(TargetDir);
  Zip := TAbZipArchive.Create(TestTempDir + 'test.zip', fmOpenRead or fmShareDenyNone);
  try
    Zip.ExtractHelper := ExtractHelper;
    Zip.BaseDirectory := TargetDir;
    Zip.Password := FPassword;
    Zip.Load;
    Zip.ExtractFiles('*');
  finally
    Zip.Free;
  end;
  CheckDirMatch(FSourceDir, TargetDir, False);
end;
{----------------------------------------------------------------------------}

initialization

  TestFramework.RegisterTest('Abbrevia.Low-Level Compression Test Suite',
    TAbZipArchiveTests.Suite);

end.
