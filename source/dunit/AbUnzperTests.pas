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
  end;

implementation

{ TAbUnZipperTests }

procedure TAbUnZipperTests.SetUp;
begin
  inherited;
  Component := TAbUnzipper.Create(TestForm);
end;

procedure TAbUnZipperTests.TearDown;
begin
  inherited;

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

 