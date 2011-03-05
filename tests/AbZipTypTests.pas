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
  Classes, TestFrameWork, AbTestFrameWork, AbArcTypTests, AbArcTyp, AbUtils, AbZipTyp;

type
  TAbZipArchiveTests = class(TAbArchiveTests)
  private
    class function DecompressSuite(const aDir: string): ITestSuite;

  protected
    class function ArchiveClass: TAbArchiveClass; override;
    class function ArchiveExt: string; override;
    class function ArchiveType: TAbArchiveType; override;
    class function VerifyArchive(aStream: TStream): TAbArchiveType; override;

  public
    class function Suite: ITestSuite; override;
  end;

  TAbZipDecompressTest = class(TAbTestCase)
  private
    FFileName: string;
    procedure ItemFailure(Sender : TObject; Item : TAbArchiveItem;
      ProcessType : TAbProcessType; ErrorClass : TAbErrorClass;
      ErrorCode : Integer);
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
  public
    constructor Create(const aTestName, aSourceDir: string;
      aMethod: TAbZipSupportedMethod; const aPassword: AnsiString); reintroduce;
  published
    procedure TestCompress;
  end;

implementation

uses
  SysUtils, AbConst, AbUnzPrc, AbZipPrc;

{----------------------------------------------------------------------------}
{ TAbZipArchive with Extract/Insert helpers }
{----------------------------------------------------------------------------}

type
  TAbZipArchive = class(AbZipTyp.TAbZipArchive)
  private
    procedure DoExtractHelper(Sender : TObject; Item : TAbArchiveItem;
      const NewName : string);
    procedure DoExtractToStreamHelper(Sender : TObject; Item : TAbArchiveItem;
      OutStream : TStream);
    procedure DoInsertHelper(Sender : TObject; Item : TAbArchiveItem;
      OutStream : TStream);
    procedure DoInsertFromStreamHelper(Sender : TObject; Item : TAbArchiveItem;
      OutStream, InStream : TStream);
  public {methods}
    constructor CreateFromStream( aStream : TStream; const ArchiveName : string );
      override;
  end;

constructor TAbZipArchive.CreateFromStream( aStream : TStream;
  const ArchiveName : string );
begin
  inherited;
  ExtractHelper := DoExtractHelper;
  ExtractToStreamHelper := DoExtractToStreamHelper;
  InsertHelper := DoInsertHelper;
  InsertFromStreamHelper := DoInsertFromStreamHelper;
end;
{----------------------------------------------------------------------------}
procedure TAbZipArchive.DoExtractHelper(Sender : TObject;
  Item : TAbArchiveItem; const NewName : string);
begin
  AbUnzip(Sender, TAbZipItem(Item), NewName);
end;
{----------------------------------------------------------------------------}
procedure TAbZipArchive.DoExtractToStreamHelper(Sender : TObject;
  Item : TAbArchiveItem; OutStream : TStream);
begin
  AbUnzipToStream(Sender, TAbZipItem(Item), OutStream);
end;
{----------------------------------------------------------------------------}
procedure TAbZipArchive.DoInsertHelper(Sender : TObject;
  Item : TAbArchiveItem; OutStream : TStream);
begin
  AbZip(TAbZipArchive(Sender), TAbZipItem(Item), OutStream);
end;
{----------------------------------------------------------------------------}
procedure TAbZipArchive.DoInsertFromStreamHelper(Sender : TObject;
  Item : TAbArchiveItem; OutStream, InStream : TStream);
begin
  AbZipFromStream(TAbZipArchive(Sender), TAbZipItem(Item), OutStream, InStream);
end;

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
  {$IFDEF UnzipWavPackSupport}
  TestCase: TAbZipDecompressTest;
  {$ENDIF}
begin
  Result := inherited Suite;
  // Test compression/decompression of Unicode filenames
  Result.AddSuite(DecompressSuite(TestFileDir + 'Unicode'));
  Result.AddTest(
    TAbZipCompressTest.Create('Compress Unicode',
      TestFileDir + 'Unicode' + PathDelim + 'source', smStored, ''));

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

  {$IFDEF UnzipWavPackSupport}
  // Test decompressiong of .wav files
  TestCase := TAbZipDecompressTest.Create(
    TestFileDir + 'WavPack' + PathDelim + 'wavpack.zip');
  TestCase.FTestName := 'Decompress WavPack';
  Result.AddTest(TestCase);
  {$ENDIF}
end;
{ -------------------------------------------------------------------------- }
class function TAbZipArchiveTests.ArchiveClass: TAbArchiveClass;
begin
  Result := TAbZipArchive;
end;
{ -------------------------------------------------------------------------- }
class function TAbZipArchiveTests.ArchiveExt: string;
begin
  Result := '.zip';
end;
{ -------------------------------------------------------------------------- }
class function TAbZipArchiveTests.ArchiveType: TAbArchiveType;
begin
  Result := atZip;
end;
{ -------------------------------------------------------------------------- }
class function TAbZipArchiveTests.VerifyArchive(aStream: TStream): TAbArchiveType;
begin
  Result := VerifyZip(aStream);
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
procedure TAbZipDecompressTest.ItemFailure(Sender : TObject;
  Item : TAbArchiveItem; ProcessType : TAbProcessType;
  ErrorClass : TAbErrorClass; ErrorCode : Integer);
begin
  if ErrorClass = ecAbbrevia then
    Fail('Extract failed: ' + AbStrRes(ErrorCode));
end;
{----------------------------------------------------------------------------}
procedure TAbZipDecompressTest.TestDecompress;
var
  Zip: TAbZipArchive;
begin
  Zip := TAbZipArchive.Create(FFileName, fmOpenRead or fmShareDenyNone);
  try
    Zip.OnProcessItemFailure := ItemFailure;
    Zip.BaseDirectory := TestTempDir;
    Zip.Password := 'password';
    Zip.Load;
    Zip.ExtractFiles('*');
  finally
    Zip.Free;
  end;
  CheckDirMatch(ExtractFilePath(FFileName) + 'source', TestTempDir);
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
procedure TAbZipCompressTest.TestCompress;
var
  Zip: TAbZipArchive;
  TargetDir: string;
begin
  Zip := TAbZipArchive.Create(TestTempDir + 'test.zip', fmCreate);
  try
    Zip.CompressionMethodToUse := FMethod;
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
    Zip.BaseDirectory := TargetDir;
    Zip.Password := FPassword;
    Zip.Load;
    Zip.ExtractFiles('*');
  finally
    Zip.Free;
  end;
  CheckDirMatch(FSourceDir, TargetDir);
end;
{----------------------------------------------------------------------------}

initialization

  TestFramework.RegisterTest('Abbrevia.Low-Level Compression Test Suite',
    TAbZipArchiveTests.Suite);

end.
