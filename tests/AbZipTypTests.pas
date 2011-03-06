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
  TAbZipArchiveTests = class(TAbArchiveMultiFileTests)
  private
    class function DecompressSuite(const aDir: string): ITestSuite;

  protected
    class procedure AddCanterburyTests(aSuite: ITestSuite); override;
    class function ArchiveClass: TAbArchiveClass; override;
    class function ArchiveExt: string; override;
    class function ArchiveType: TAbArchiveType; override;
    class function VerifyArchive(aStream: TStream): TAbArchiveType; override;

  public
    class function Suite: ITestSuite; override;
  end;

  TAbZipDecompressTest = class(TAbArchiveDecompressTest)
  protected
    function CreateArchive(const aFileName: string; aMode : Word): TAbArchive;
      override;
  end;

  TAbZipCompressTest = class(TAbArchiveCompressTest)
  private
    FMethod: TAbZipSupportedMethod;
    FPassword: AnsiString;
  protected
    function CreateArchive(const aFileName: string; aMode : Word): TAbArchive;
      override;
  public
    constructor Create(aParent: TAbArchiveTestsClass;
      const aTestName, aSourceDir: string; aMethod: TAbZipSupportedMethod;
      const aPassword: AnsiString); reintroduce;
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
        Result.AddTest(TAbZipDecompressTest.Create(Self,
          ChangeFileExt(SR.Name, ''), Dir + PathDelim + SR.Name));
      until FindNext(SR) <> 0;
    finally
      FindClose(SR);
    end;
end;
{ -------------------------------------------------------------------------- }
class function TAbZipArchiveTests.Suite: ITestSuite;
begin
  Result := inherited Suite;
  // Test decompression of Unicode filenames
  Result.AddSuite(DecompressSuite(TestFileDir + 'Unicode'));

  {$IFDEF UnzipWavPackSupport}
  // Test decompressiong of .wav files
  Result.AddTest(
    TAbZipDecompressTest.Create(Self, 'Decompress WavPack',
      TestFileDir + 'WavPack' + PathDelim + 'wavpack.zip'));
  {$ENDIF}
end;
{ -------------------------------------------------------------------------- }
class procedure TAbZipArchiveTests.AddCanterburyTests(aSuite: ITestSuite);
var
  CompressSuite: ITestSuite;
begin
  aSuite.AddSuite(DecompressSuite(CanterburyDir));

  CompressSuite := TTestSuite.Create('Compress Canterbury');
  CompressSuite.AddTest(
    TAbZipCompressTest.Create(Self, 'Store', CanterburySourceDir, smStored, ''));
  CompressSuite.AddTest(
    TAbZipCompressTest.Create(Self, 'Deflate', CanterburySourceDir, smDeflated, ''));
  CompressSuite.AddTest(
    TAbZipCompressTest.Create(Self, 'StoreP', CanterburySourceDir, smStored, 'password'));
  CompressSuite.AddTest(
    TAbZipCompressTest.Create(Self, 'DeflateP', CanterburySourceDir, smDeflated, 'password'));
  aSuite.AddSuite(CompressSuite);
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
function TAbZipDecompressTest.CreateArchive(const aFileName: string;
  aMode : Word): TAbArchive;
begin
  Result := inherited CreateArchive(aFileName, aMode);
  (Result as TAbZipArchive).Password := 'password';
end;

{----------------------------------------------------------------------------}
{ TAbZipCompressTest }
{----------------------------------------------------------------------------}
constructor TAbZipCompressTest.Create(aParent: TAbArchiveTestsClass;
  const aTestName, aSourceDir: string; aMethod: TAbZipSupportedMethod;
  const aPassword: AnsiString);
begin
  inherited Create(aParent, aTestName, aSourceDir);
  FMethod := aMethod;
  FPassword := aPassword;
end;
{----------------------------------------------------------------------------}
function TAbZipCompressTest.CreateArchive(const aFileName: string;
  aMode : Word): TAbArchive;
begin
  Result := inherited CreateArchive(aFileName, aMode);
  (Result as TAbZipArchive).CompressionMethodToUse := FMethod;
  (Result as TAbZipArchive).Password := FPassword;
end;
{----------------------------------------------------------------------------}

initialization

  TestFramework.RegisterTest('Abbrevia.Low-Level Compression Test Suite',
    TAbZipArchiveTests.Suite);

end.
