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

unit AbUnzperTests;
{$I AbDefine.inc}
{ The following define is unit specific it will build the files used
  in the tests here. Due to the potential problems of compress having
  a problem and uncompress not, I did not want to confuse the tests by
  having these created every time, but only when needed under a stable
  code base... Otherwise it would more difficult to determine which
  side the problem was on!  WATCH out for hardcoding of paths, if you
  use this compiler define}
{.$DEFINE BUILDTESTS}
interface

uses
  TestFrameWork,abTestFrameWork,AbUnzper,SysUtils,Classes,abMeter;

type

  TAbUnZipperTests = class(TabCompTestCase)
  private
    Component : TAbUnZipper;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDefaultStreaming;
    procedure TestComponentLinks;
    procedure TestBasicUnzip;
    procedure TestBasicUnGzip;
    procedure TestBasicUnGzipTar;
    {$IFDEF BUILDTESTS}
    procedure CreateTestFiles;
    {$ENDIF}
  end;

implementation
{$IFDEF BUILDTESTS}
  uses AbZipper;
{$ENDIF}

{ TAbUnZipperTests }
{$IFDEF BUILDTESTS}
procedure TAbUnZipperTests.CreateTestFiles;
var
 Zip : TAbZipper;
begin
 //Cheap and easy way to keep the code around that I use to build the tests
 //Hard Coded to Abbrevia path to keep things easy as this is one time only code
 Zip := TAbZipper.Create(nil);
 try
    Zip.FileName := TestFileDir + 'MPL.ZIP';
    Zip.AddFiles('C:\TP\ABBREVIA\MPL-1_1.TXT',faAnyFile);
    Zip.Save;
 finally
   Zip.Free;
 end;
 Zip := TAbZipper.Create(nil);
 try
    Zip.FileName := TestFileDir + 'MPL.GZ';
    Zip.AddFiles('C:\TP\ABBREVIA\MPL-1_1.TXT',faAnyFile);
    Zip.Save;
 finally
   Zip.Free;
 end;
 Zip := TAbZipper.Create(nil);
 try
    Zip.FileName := TestFileDir + 'MPL.TGZ';
    Zip.AddFiles('C:\TP\ABBREVIA\MPL-1_1.TXT',faAnyFile);
    Zip.Save;
 finally
   Zip.Free;
 end;
end;
{$ENDIF}

procedure TAbUnZipperTests.SetUp;
begin
  inherited;
  Component := TAbUnzipper.Create(TestForm);
end;

procedure TAbUnZipperTests.TearDown;
begin
  inherited;

end;


procedure TAbUnZipperTests.TestBasicUnGzip;
var
 TestFileName : string;
begin
  TestFileName :=  TestTempDir + 'MPL-1_1.txt';
  if FileExists(TestFileName) then
     DeleteFile(TestFileName);
  Component.BaseDirectory := TestTempDir;
  Component.FileName := TestFileDir + 'mpl.gz';
  Component.ExtractFiles('*.*');
  Check(FileExists(TestFileName),'Unzip Test File not Found');
  DeleteFile(TestFileName)
end;

procedure TAbUnZipperTests.TestBasicUnGzipTar;
var
 TestFileName : string;
begin
  TestFileName :=  TestTempDir + 'MPL-1_1.txt';
  if FileExists(TestFileName) then
     DeleteFile(TestFileName);
  Component.BaseDirectory := TestTempDir;
  Component.FileName := TestFileDir + 'mpl.tgz';
  Component.ExtractFiles('*.*');
  Check(FileExists(TestFileName),'Unzip Test File not Found');
  DeleteFile(TestFileName)
end;

procedure TAbUnZipperTests.TestBasicUnzip;
var
 TestFileName : string;
begin
  TestFileName :=  TestTempDir + 'MPL-1_1.txt';
  if FileExists(TestFileName) then
     DeleteFile(TestFileName);
  Component.BaseDirectory := TestTempDir;
  Component.FileName := TestFileDir + 'mpl.zip';
  Component.ExtractFiles('*.*');
  Check(FileExists(TestFileName),'Unzip Test File not Found');
  DeleteFile(TestFileName);
end;

procedure TAbUnZipperTests.TestComponentLinks;
var
  MLink1,MLink2 : TAbVCLMeterLink;
begin
  MLink1 := TAbVCLMeterLink.Create(TestForm);
  MLink2 := TAbVCLMeterLink.Create(TestForm);
  Component.ArchiveProgressMeter := MLink1;
  Component.ItemProgressMeter := MLink2;
  MLink1.Free;
  MLink2.Free;
  Check(Component.ArchiveProgressMeter = nil,'Notification does not work for TabUnZipper.ArchiveProgressMeter');
  Check(Component.ItemProgressMeter = nil,'Notification does not work for TabUnZipper.ItemProgressMeter');
end;

procedure TAbUnZipperTests.TestDefaultStreaming;
var
CompStr : STring;
CompTest : TAbUnZipper;
begin
  RegisterClass(TAbUnZipper);
  CompStr  := StreamComponent(Component);
  CompTest := (UnStreamComponent(CompStr) as TAbUnZipper);
  CompareComponentProps(Component,CompTest);
  UnRegisterClass(TAbUnZipper);
end;

initialization

  TestFramework.RegisterTest('Abbrevia.Component Level Test Suite',
    TAbUnZipperTests.Suite);
 
end.

 