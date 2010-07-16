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
  Classes, AbArcTyp, AbArcTypTests, AbTestFrameWork;

type
  TAbCabArchiveTests = class(TAbArchiveMultiFileTests)
  protected
    class function ArchiveClass: TAbArchiveClass; override;
    class function ArchiveExt: string; override;
  published
    procedure TestVerifyCab;
  end;

implementation

uses
  SysUtils, TestFrameWork, AbCabTyp, AbUtils;

{----------------------------------------------------------------------------}
{ TAbCabArchiveTests }
{----------------------------------------------------------------------------}
class function TAbCabArchiveTests.ArchiveClass: TAbArchiveClass;
begin
  Result := TAbCabArchive;
end;
{ -------------------------------------------------------------------------- }
class function TAbCabArchiveTests.ArchiveExt: string;
begin
  Result := '.cab';
end;
{ -------------------------------------------------------------------------- }
procedure TAbCabArchiveTests.TestVerifyCab;
begin
  Check(VerifyCab(MPLDir + 'MPL.cab') = atCab, 'VerifyCab failed on valid CAB');
  Check(VerifyCab(MPLDir + 'MPL.zip') = atUnknown, 'VerifyCab succeeded on ZIP');
end;
{----------------------------------------------------------------------------}

initialization

  TestFramework.RegisterTest('Abbrevia.Low-Level Compression Test Suite',
    TAbCabArchiveTests.Suite);

end.
