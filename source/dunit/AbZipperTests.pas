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
 * Robert Love
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)
unit AbZipperTests;
{$I AbDefine.inc}
interface

uses
  TestFrameWork, abTestFrameWork, AbZipper, AbUnZper, SysUtils,
  Classes, abMeter, abArcTyp, abUtils;

type

  TAbZipperTests = class(TabCompTestCase)
  private
    Component : TAbZipper;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDefaultStreaming;
    procedure TestComponentLinks;
    procedure BasicZipFile;
    procedure CreateAndTestBasicZipFile;
    procedure CreateAndTestBasicZipFile2;
    procedure CreateAndTestBasicZipFile3;
    procedure CreateAndTestBasicZipFile4;
    procedure BasicGZipTarFile;
    procedure BasicGZipTarFile2;
    procedure TestBasicForceTypeZip;
    procedure TestBasicForceTypeGZipTar;
    procedure CreatePasswordProtectedAddedByStream;
    procedure GZipInputStreamClearTest;
  end;

implementation

{ TAbZipperTests }

procedure TAbZipperTests.BasicGZipTarFile;
var
 TestFileName : String;
begin
 // This test only insure that the Gzip Tar file is created without raising errors
 // it is not designed to test the data in the resulting zip file.
 TestFileName := TestTempDir + 'basic.tgz';
 Component.FileName := TestFileName;
 Component.BaseDirectory := GetTestFileDir;
 Component.AddFiles('*.*',faAnyFile);
 Component.Save;
 CheckFileExists(TestFileName);
 DeleteFile(TestFileName);
end;

procedure TAbZipperTests.BasicGZipTarFile2;
var
 TestFileName : string;
begin
 TestFileName := TestTempDir + 'abasic.tar.gz';
 Component.FileName := TestFileName;
 Component.BaseDirectory := GetTestFileDir;
 Component.AddFiles('*.*',faAnyFile);
 Component.Save;
 CheckFileExists(TestFileName);
 DeleteFile(TestFileName);
end;

procedure TAbZipperTests.BasicZipFile;
var
 TestFileName : String;
begin
 // This test only insure that the zip file is created without raising errors
 // it is not designed to test the data in the resulting zip file.
 TestFileName := TestTempDir + 'basic.zip';
 Component.FileName := TestFileName;
 Component.BaseDirectory := GetTestFileDir;
 Component.AddFiles('*.*',faAnyFile);
 Component.Save;
 CheckFileExists(TestFileName);
 DeleteFile(TestFileName);
end;

procedure TAbZipperTests.CreateAndTestBasicZipFile;
var
 TestFileName : String;
 ExtractDir : String;
 AbUnZip : TAbUnZipper;
begin
// Test with Setting BaseDirectory and not specifying AutoSave  
 TestFileName := TestTempDir + 'basic.zip';
 if FileExists(TestFileName) then
   DeleteFile(TestFileName);
 Component.FileName := TestFileName;
 Component.BaseDirectory := GetTestFileDir;
 Component.AddFiles('*.*',faAnyFile);
 Component.Save;
 Component.FileName := '';
 CheckFileExists(TestFileName);

 AbUnZip := TAbUnZipper.Create(nil);
 try
   AbUnZip.FileName := TestFileName;
   // Clean out old Directory and create a new one.
   Extractdir := TestTempDir + 'extracttest\';
   if DirExists(ExtractDir) then
      DelTree(ExtractDir);
   CreateDir(ExtractDir);
   // Extract Files.
   AbUnZip.BaseDirectory := ExtractDir;
   AbUnZip.ExtractFiles('*.*');
   // Compare Extracted Files
   CheckDirMatch(TestFileDir,ExtractDir,False);
 finally
  AbUnZip.Free;
 end;
 DeleteFile(TestFileName);
end;

procedure TAbZipperTests.CreateAndTestBasicZipFile2;
var
 TestFileName : String;
 ExtractDir : String;
 AbUnZip : TAbUnZipper;
begin
// Test AutoSave and not setting BaseDirectory specify full path AddFiles

 TestFileName := TestTempDir + 'basic.zip';
 if FileExists(TestFileName) then
   DeleteFile(TestFileName);
 Component.AutoSave := True;
 Component.FileName := TestFileName;
 Component.AddFiles(GetTestFileDir + '*.*',faAnyFile);
 Component.FileName := '';
 CheckFileExists(TestFileName);

 AbUnZip := TAbUnZipper.Create(nil);
 try
   AbUnZip.FileName := TestFileName;
   // Clean out old Directory and create a new one.
   Extractdir := TestTempDir + 'extracttest\';
   if DirExists(ExtractDir) then
      DelTree(ExtractDir);
   CreateDir(ExtractDir);
   // Extract Files.
   AbUnZip.BaseDirectory := ExtractDir;
   AbUnZip.ExtractFiles('*.*');
   // Compare Extracted Files
   CheckDirMatch(TestFileDir,ExtractDir,False);
 finally
  AbUnZip.Free;
 end;
 DeleteFile(TestFileName);
end;

procedure TAbZipperTests.CreateAndTestBasicZipFile3;
var
 TestFileName : String;
 ExtractDir : String;
 AbUnZip : TAbUnZipper;
begin
// Test AutoSave setting Base Directory.
 TestFileName := TestTempDir + 'basic.zip';
 if FileExists(TestFileName) then
   DeleteFile(TestFileName);
 Component.AutoSave := True;
 Component.BaseDirectory := TestFileDir;
 Component.FileName := TestFileName;
 Component.AddFiles('*.*',faAnyFile);
 Component.FileName := '';
 CheckFileExists(TestFileName);

 AbUnZip := TAbUnZipper.Create(nil);
 try
   AbUnZip.FileName := TestFileName;
   // Clean out old Directory and create a new one.
   Extractdir := TestTempDir + 'extracttest\';
   if DirExists(ExtractDir) then
      DelTree(ExtractDir);
   CreateDir(ExtractDir);
   // Extract Files.
   AbUnZip.BaseDirectory := ExtractDir;
   AbUnZip.ExtractFiles('*.*');
   // Compare Extracted Files
   CheckDirMatch(TestFileDir,ExtractDir,False);
 finally
  AbUnZip.Free;
 end;
 DeleteFile(TestFileName);

end;

procedure TAbZipperTests.CreateAndTestBasicZipFile4;
var
 TestFileName : String;
 ExtractDir : String;
 AbUnZip : TAbUnZipper;
begin
// Test AutoSave setting BaseDirectory and specifing full path to AddFiles
 TestFileName := TestTempDir + 'basic.zip';
 if FileExists(TestFileName) then
   DeleteFile(TestFileName);
 Component.AutoSave := True;
 Component.BaseDirectory := TestTempDir;
 Component.FileName := TestFileName;
 Component.AddFiles(GetTestFileDir + '*.*',faAnyFile);
 Component.FileName := '';
 CheckFileExists(TestFileName);

 AbUnZip := TAbUnZipper.Create(nil);
 try
   AbUnZip.FileName := TestFileName;
   // Clean out old Directory and create a new one.
   Extractdir := TestTempDir + 'extracttest\';
   if DirExists(ExtractDir) then
      DelTree(ExtractDir);
   CreateDir(ExtractDir);
   // Extract Files.
   AbUnZip.BaseDirectory := ExtractDir;
   AbUnZip.ExtractFiles('*.*');
   // Compare Extracted Files
   CheckDirMatch(TestFileDir,ExtractDir,False);
 finally
  AbUnZip.Free;
 end;
 DeleteFile(TestFileName);
end;

procedure TAbZipperTests.SetUp;
begin
  inherited;
  Component := TAbZipper.Create(TestForm);
end;

procedure TAbZipperTests.TearDown;
begin
  inherited;

end;

procedure TAbZipperTests.TestBasicForceTypeGZipTar;
var
 TestFileName : string;
begin
 TestFileName := TestTempDir + 'basicGzipTar';
 Component.ArchiveType := atGzippedTar;
 Component.ForceType := True;
 Component.FileName := TestFileName;
 Component.BaseDirectory := GetTestFileDir;
 Component.AddFiles('*.*',faAnyFile);
 Component.Save;
 CheckFileExists(TestFileName);
 DeleteFile(TestFileName);
end;

procedure TAbZipperTests.TestBasicForceTypeZip;
var
 TestFileName : string;
begin
 TestFileName := TestTempDir + 'basic';
 Component.ArchiveType := atZip;
 Component.ForceType := True;
 Component.FileName := TestFileName;
 Component.BaseDirectory := GetTestFileDir;
 Component.AddFiles('*.*',faAnyFile);
 Component.Save;
 CheckFileExists(TestFileName);
 DeleteFile(TestFileName);
end;

procedure TAbZipperTests.TestComponentLinks;
var
  MLink1,MLink2,MLink3 : TAbVCLMeterLink;
begin
  MLink1 := TAbVCLMeterLink.Create(TestForm);
  MLink2 := TAbVCLMeterLink.Create(TestForm);
  MLink3 := TAbVCLMeterLink.Create(TestForm);
  Component.ArchiveProgressMeter := MLink1;
  Component.ItemProgressMeter := MLink2;
  Component.ArchiveSaveProgressMeter := MLink3;
  MLink1.Free;
  MLink2.Free;
  MLink3.Free;
  Check(Component.ArchiveProgressMeter = nil,'Notification does not work for TAbZipper.ArchiveProgressMeter');
  Check(Component.ItemProgressMeter = nil,'Notification does not work for TAbZipper.ItemProgressMeter');
  Check(Component.ArchiveSaveProgressMeter = nil,'Notification does not work for TAbZipper.ArchiveSaveProgressMeter');
end;

procedure TAbZipperTests.TestDefaultStreaming;
var
CompStr : STring;
CompTest : TAbZipper;
begin
  RegisterClass(TAbZipper);
  CompStr  := StreamComponent(Component);
  CompTest := (UnStreamComponent(CompStr) as TAbZipper);
  CompareComponentProps(Component,CompTest);
  UnRegisterClass(TAbZipper);
end;

procedure TAbZipperTests.CreatePasswordProtectedAddedByStream;
var
//oAbZipper : TAbZipper;
oFileStream : TFileStream;
sZipFile : String;
begin
//  oAbZipper := TAbZipper.Create(nil);
//  try
  // Remove the path from the zip
  Component.StoreOptions := [soStripPath];

  sZipFile := TestTempDir +  'PWStream.zip';

  // Create the directory if it doesn't exist
  {$IFDEF DELPHI5} // ForceDirectories not part of Delphi 4 so assume created by hand
   Check(ForceDirectories(TestTempDir),'Unable to create Test Temp directory.');
  {$ELSE}
   Check(DirExists(TestTempDir),'Test Temp Directory needs to be created.');
  {$ENDIF}


  // File we need to zip
  Component.FileName := sZipFile;

  // Password protect the source file
//  Component.Password := 'password';

  oFileStream := TFileStream.Create(TestFileDir +'MPL-1_1.txt', fmOpenRead or fmShareDenyNone);
  try
  // Add file to the zip file
   Component.AddFromStream('file.ext', oFileStream);
   Component.Save;

  finally
  // Free memory
  oFileStream.Free;
  end; {finally}
  CheckFileExists(sZipFile);
  

end;

procedure TAbZipperTests.GZipInputStreamClearTest;
//[ 820489 ] CloseArchive does not close input stream
var
 fs,fs2 : TFileStream;
 filename : string;
 extractFilename :string;
 unzip : TAbUnZipper;
begin
 fs := TFileStream.Create(TestFileDir + 'MPL-1_1.txt',fmOpenRead);
 try
 filename := TestTempDir + 'clearinputstr.gz';
 if FileExists(filename) then
   DeleteFile(fileName);
 Component.ForceType := true;
 Component.ArchiveType := atGzip;
 Component.FileName := FileName;
 Fs.Position := 0;
 Component.AddFromStream('', fs);
 Component.Save;
 Component.CloseArchive;
 if FileExists(filename) then
   DeleteFile(fileName);
 Component.ForceType := true;
 Component.ArchiveType := atGzip;
 Component.FileName := FileName;
 Fs.Position := 0;
 Component.AddFromStream('', fs);
 Component.Save;
 Component.CloseArchive;
 //Check Archive for match
 unzip := TAbUnZipper.Create(nil);
 try
   extractFilename := TestTempDir + 'extractmpl.txt';
   unzip.FileName := filename;
   unzip.ExtractAt(0,extractFileName);
 finally
  unzip.free;
 end;
 fs2 := TFileStream.Create(extractFilename,fmOpenRead);
 try
   CheckStreamMatch(fs,fs2,'Extracted file does not match original');
 finally
   fs2.free;
 end;

 finally
  fs.free;
 end;


end;


initialization

  TestFramework.RegisterTest('Abbrevia.Component Level Test Suite',
    TAbZipperTests.Suite);

end.

