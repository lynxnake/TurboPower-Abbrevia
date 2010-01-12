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

 unit AbArcTypTests;

{$I AbDefine.inc}

interface

uses
  SysUtils, Classes, TestFrameWork, AbTestFrameWork, AbArcTyp;

type
  TAbArchiveTests = class(TAbTestCase)
  published
    procedure TestGenerateHash;
  end;

implementation

{----------------------------------------------------------------------------}
{ TAbArchiveTests }
{----------------------------------------------------------------------------}
procedure TAbArchiveTests.TestGenerateHash;
  {- Test issue 1196468, range check error in TAbArchiveList.GenerateHash }
var
  ItemList: TAbArchiveList;
begin
  ItemList := TAbArchiveList.Create(True);
  try
    Check(ItemList.Find('dvd9g06s4_050503103802_n_meas.log') = -1);
  finally
    ItemList.Free;
  end;
end;
{----------------------------------------------------------------------------}

initialization

  TestFramework.RegisterTest('Abbrevia.Low-Level Compression Test Suite',
    TAbArchiveTests.Suite);

end.
