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

interface

uses
  TestFrameWork,abTestFrameWork,AbZipper,SysUtils,Classes,abMeter;

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
  end;

implementation

{ TAbZipperTests }

procedure TAbZipperTests.SetUp;
begin
  inherited;
  Component := TAbZipper.Create(TestForm);
end;

procedure TAbZipperTests.TearDown;
begin
  inherited;

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

initialization

  TestFramework.RegisterTest('Abbrevia.Component Level Test Suite',
    TAbZipperTests.Suite);
 
end.

 