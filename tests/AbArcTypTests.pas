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
{$IF CompilerVersion >= 17.0}
  {$DEFINE HasEnumerators}    { Test for..in support in D2005+ }
{$IFEND}

interface

uses
  Classes, AbTestFrameWork, AbArcTyp, AbUtils;

type
  TAbArchiveListTests = class(TAbTestCase)
  published
    {$IFDEF HasEnumerators}
    procedure TestEnumerator;
    {$ENDIF}
    procedure TestGenerateHash;
  end;

  TAbArchiveClass = class of TAbArchive;

  TAbArchiveTests = class(TAbTestCase)
  private
    procedure TestExtractFile(const aArchiveFile, aSourceFile: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    function CreateArchive(const aFileName : string; aMode : Word): TAbArchive;
      overload; virtual;
    function CreateArchive(aStream : TStream; const aArchiveName : string): TAbArchive;
      overload; virtual;
    class function ArchiveClass: TAbArchiveClass; virtual; abstract;
    class function ArchiveExt: string; virtual; abstract;
    class function ArchiveType: TAbArchiveType; virtual; abstract;
    class function VerifyArchive(aStream: TStream): TAbArchiveType; virtual; abstract;
  published
    procedure TestExtract;
    procedure TestExtractToStream;
    procedure TestAdd;
    procedure TestAddFromStream;
    procedure TestVerify;
  end;

  TAbArchiveMultiFileTests = class(TAbArchiveTests)
  private
    procedure TestCompressDir(const aSourceDir: string);
  published
    procedure TestCompressCanterbury;
    {$IFDEF UNICODE}
    procedure TestCompressUnicode;
    {$ENDIF}
  end;

implementation

uses
  SysUtils, TestFrameWork;

{----------------------------------------------------------------------------}
{ TAbArchiveListTests }
{----------------------------------------------------------------------------}
{$IFDEF HasEnumerators}
procedure TAbArchiveListTests.TestEnumerator;
var
  ItemList: TAbArchiveList;
  Item: TAbArchiveItem;
  i: Integer;
begin
  ItemList := TAbArchiveList.Create(True);
  try
    for i := 0 to 9 do begin
      Item := TAbArchiveItem.Create;
      Item.FileName := IntToStr(i);
      ItemList.Add(Item);
    end;
    i := 0;
    for Item in ItemList do begin
      CheckEquals(IntToStr(i), Item.FileName);
      Inc(i);
    end;
    CheckEquals(10, i);
  finally
    ItemList.Free;
  end;
end;
{$ENDIF}
{----------------------------------------------------------------------------}
procedure TAbArchiveListTests.TestGenerateHash;
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
{ TAbArchiveTests }
{----------------------------------------------------------------------------}
function TAbArchiveTests.CreateArchive(const aFileName : string;
  aMode : Word): TAbArchive;
begin
  Result := ArchiveClass.Create(aFileName, aMode);
end;
{ -------------------------------------------------------------------------- }
function TAbArchiveTests.CreateArchive(aStream : TStream;
  const aArchiveName : string): TAbArchive;
begin
  Result := ArchiveClass.CreateFromStream(aStream, aArchiveName);
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchiveTests.SetUp;
begin
  inherited;
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchiveTests.TearDown;
begin
  inherited;
end;
{----------------------------------------------------------------------------}
procedure TAbArchiveTests.TestExtractFile(const aArchiveFile, aSourceFile: string);
var
  Arc: TAbArchive;
begin
  Arc := CreateArchive(aArchiveFile, fmOpenRead);
  try
    Arc.Load;
    Arc.ExtractAt(0, TestTempDir + ExtractFileName(aSourceFile));
    CheckFilesMatch(aSourceFile, TestTempDir + ExtractFileName(aSourceFile), '');
  finally
    Arc.Free;
  end;
end;
{----------------------------------------------------------------------------}
procedure TAbArchiveTests.TestExtract;
begin
  TestExtractFile(MPLDir + 'MPL' + ArchiveExt, MPLDir + 'MPL-1_1.txt');
end;
{----------------------------------------------------------------------------}
procedure TAbArchiveTests.TestExtractToStream;
var
  Arc: TAbArchive;
  Stream: TMemoryStream;
begin
  Arc := CreateArchive(MPLDir + 'MPL' + ArchiveExt, fmOpenRead);
  try
    Arc.Load;
    Stream := TMemoryStream.Create;
    try
      // Bzip2 doesn't store the original filename, so don't hardcode a path
      Arc.ExtractToStream(Arc.ItemList[0].FileName, Stream);
      CheckFileMatchesStream(MPLDir + 'MPL-1_1.txt', Stream);
    finally
      Stream.Free;
    end;
  finally
    Arc.Free;
  end;
end;
{----------------------------------------------------------------------------}
procedure TAbArchiveTests.TestAdd;
var
  Arc: TAbArchive;
begin
  Arc := CreateArchive(TestTempDir + 'test' + ArchiveExt, fmCreate);
  try
    Arc.Load;
    Arc.BaseDirectory := MPLDir;
    Arc.AddFiles('MPL-1_1.txt', faAnyFile);
    Arc.Save;
  finally
    Arc.Free;
  end;
  TestExtractFile(TestTempDir + 'test' + ArchiveExt, MPLDir + 'MPL-1_1.txt');
end;
{----------------------------------------------------------------------------}
procedure TAbArchiveTests.TestAddFromStream;
var
  Arc: TAbArchive;
  MemStream, FileStream: TStream;
begin
  Arc := CreateArchive(TestTempDir + 'test' + ArchiveExt, fmCreate);
  try
    Arc.Load;
    // Copy to a memory stream to test adding something other than a TFileStream
    MemStream := TMemoryStream.Create;
    try
      FileStream := TFileStream.Create(MPLDir + 'MPL-1_1.txt', fmOpenRead or fmShareDenyWrite);
      try
        MemStream.CopyFrom(FileStream, 0);
      finally
        FileStream.Free;
      end;
      MemStream.Position := 0;
      Arc.AddFromStream('MPL-1_1.txt', MemStream);
      Arc.Save;
    finally
      MemStream.Free;
    end;
  finally
    Arc.Free;
  end;
  TestExtractFile(TestTempDir + 'test' + ArchiveExt, MPLDir + 'MPL-1_1.txt');
end;
{----------------------------------------------------------------------------}
procedure TAbArchiveTests.TestVerify;
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(MPLDir + 'MPL' + ArchiveExt, fmOpenRead or fmShareDenyNone);
  try
    Check(VerifyArchive(FS) = ArchiveType, 'Verify failed on valid archive');
  finally
    FS.Free;
  end;
  FS := TFileStream.Create(MPLDir + 'MPL-1_1.txt', fmOpenRead or fmShareDenyNone);
  try
    Check(VerifyArchive(FS) = atUnknown, 'Verify succeeded on invalid archive');
  finally
    FS.Free;
  end;
end;


{----------------------------------------------------------------------------}
{ TAbArchiveMultiFileTests }
{----------------------------------------------------------------------------}
procedure TAbArchiveMultiFileTests.TestCompressDir(const aSourceDir: string);
var
  Arc: TAbArchive;
  FileName: string;
begin
  FileName := TestTempDir + 'test' + ArchiveExt;
  Arc := CreateArchive(FileName, fmCreate);
  try
    Arc.BaseDirectory := aSourceDir;
    Arc.Load; // TODO: This shouldn't be necessary
    Arc.AddFiles('*', faAnyFile);
    Arc.Save;
  finally
    Arc.Free;
  end;
  Arc := CreateArchive(FileName, fmOpenRead);
  try
    CreateDir(TestTempDir + 'test');
    Arc.BaseDirectory := TestTempDir + 'test';
    Arc.Load;
    Arc.ExtractFiles('*');
  finally
    Arc.Free;
  end;
  CheckDirMatch(aSourceDir, TestTempDir + 'test', False);
end;
{----------------------------------------------------------------------------}
procedure TAbArchiveMultiFileTests.TestCompressCanterbury;
begin
  TestCompressDir(CanterburySourceDir);
end;
{----------------------------------------------------------------------------}
{$IFDEF UNICODE}
procedure TAbArchiveMultiFileTests.TestCompressUnicode;
begin
  TestCompressDir(TestFileDir + 'Unicode' + PathDelim + 'source');
end;
{$ENDIF}
{----------------------------------------------------------------------------}

initialization

  TestFramework.RegisterTest('Abbrevia.Low-Level Compression Test Suite',
    TAbArchiveListTests.Suite);

end.
