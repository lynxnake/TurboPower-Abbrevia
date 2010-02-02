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

unit AbTestFramework;

{$I AbDefine.inc}

interface

uses
  TestFramework,
  Variants,
{$IFDEF LINUX}
  QControls, QForms,
{$ELSE}
  Windows, Controls, Forms,
{$ENDIF}
{$IFDEF WINZIPTESTS}
  SyncObjs,
{$ENDIF}
  Classes, TypInfo;

{$IFDEF WINZIPTESTS}
{ Hard Coded as I could not find install location in the Registry to extract and
  make dynamic if this proves to be a problem, we will have to have test
  configuration file that specifies the winzip command line utility path. }
const
  UnWinZip = 'C:\Program Files\WinZip\wzunzip.exe';
{$ENDIF}

type
{$IFNDEF VERSION6}
  TIntegerSet = set of 0..SizeOf(Integer) * 8 - 1;
{$ENDIF}

  TAbTestCase = class(TTestCase)
  protected
  {$IFDEF WINZIPTESTS}
    FSpawnComplete  : TSimpleEvent;
    procedure SpawnErrorEvent(Sender : TObject; Error : Word);
    procedure SpawnCompletedEvent(Sender : TObject);
    procedure SpawnTimeOutEvent(Sender : TObject);
    procedure ExecuteAndWait(ExeName,Param : string; TimeOut : Integer = 0); // Exception if failure
  {$ENDIF}
    function GetTestFileDir : string;
    function GetTestTempDir : string;
    function GetWindowsDir : string;
    procedure CheckFilesMatch(const AFileName1, aFileName2 : string;
      const aMsg : string = '');
    procedure CheckFileMatchesStream(const aFileName : string; aStream : TStream;
      const aMsg : string = '');
    procedure CheckStreamMatch(aStream1, aStream2 : TStream; const aMsg : string = '');
    procedure CheckFileExists(aFileName : string);
    procedure CheckDirExists(aFileName : string);
    procedure CreateDummyFile(aFileName : string; aSize : Integer);
    procedure SetUp; override;
    procedure TearDown; override;
    function FilesInDirectory(const aDir : string) : TStringList;
    procedure CheckDirMatch(aDir1, aDir2 : string; IgnoreMissingFiles : Boolean = True);
    // Call this routine with GREAT Caution!!!!
    procedure DelTree(aDir : string);

  public
    property TestFileDir : string
      read GetTestFileDir;
    property TestTempDir : string
      read GetTestTempDir;
  end;

  TAbCompTestCase = class(TAbTestCase)
  protected
    FTestForm : TForm;
    IgnoreProp : TStringList;
    procedure SetUp; override;
    procedure TearDown; override;

    // RTTI function so that they match from version to version of delphi.
    function AbGetPropList(TypeInfo : PTypeInfo; out PropList : PPropList) : Integer;
    function AbGetPropValue(Instance : TObject; const PropName : string; PreferStrings : Boolean = True) : Variant;

    function StreamComponent(aComp : TComponent) : string;
    function UnStreamComponent(const aCompStr : string; Instance : TComponent = nil) : TComponent;

    procedure CompareComponentProps(aComp1, aComp2 : TPersistent); virtual;

    procedure TestComponentLink(AComponent: TComponent;
      const APropName: string; APropClass: TComponentClass);

  public
    procedure ShowForm; virtual;
    property TestForm : TForm read FTestForm;
  end;

implementation

uses
{$IFDEF WINZIPTESTS}
  // Systool Unit change abdefine.inc if you don't have systools or don't want
  // to run winzip compatability tests
  stSpawn,
{$ENDIF}
  Math, SysUtils,
  AbUtils;

var
  ExePath : string;

{ TAbTestCase }

procedure TAbTestCase.CheckDirMatch(aDir1, aDir2 : string;
  IgnoreMissingFiles: Boolean);
var
  d1,d2 : TStringList;
  I : Integer;
begin
  d1 := FilesInDirectory(aDir1);
  d2 := FilesInDirectory(aDir2);
  try
    Check(IgnoreMissingFiles or (d1.count = d2.count),
      'Number of files in directories do not match');
    for I := 0 to d1.Count - 1 do
      if d2.IndexOf(d1[I]) = - 1 then
        Check(IgnoreMissingFiles, d1[I] + ' is missing in directory')
      else
        CheckFilesMatch(AbAddBackSlash(aDir1) + d1[i], AbAddBackSlash(aDir2) + d1[i], d1[i] + ' does not match');
  finally
    d1.Free;
    d2.Free;
  end;
end;

procedure TAbTestCase.CheckFileExists(aFileName : string);
begin
  Check(FileExists(aFileName), 'Unable to locate file: ' + aFileName);
end;

procedure TAbTestCase.CheckDirExists(aFileName : string);
begin
  Check(DirectoryExists(aFileName), 'Unable to locate directory: ' + aFileName);
end;

procedure TAbTestCase.CheckFilesMatch(const aFileName1, aFileName2: string;
  const aMsg: string = '');
var
  Stream1, Stream2 : TStream;
begin
  Stream1 := TFileStream.Create(aFileName1, fmOpenRead or fmShareDenyWrite);
  try
    Stream2 := TFileStream.Create(AFileName2, fmOpenRead or fmShareDenyWrite);
    try
      CheckStreamMatch(Stream1, Stream2, aMsg);
    finally
      Stream2.Free;
    end;
  finally
    Stream1.Free;
  end;
end;

procedure TAbTestCase.CheckFileMatchesStream(const aFileName : string;
  aStream : TStream; const aMsg : string = '');
var
  FileStream : TStream;
begin
  FileStream := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite);
  try
    CheckStreamMatch(FileStream, aStream, aMsg);
  finally
    FileStream.Free;
  end;
end;


procedure TAbTestCase.CheckStreamMatch(aStream1, aStream2 : TStream;
  const aMsg: string = '');
var
  I, BytesRead, BufSize : Integer;
  Buf1, Buf2 : array of Byte;
begin
  CheckEquals(aStream1.Size, aStream2.Size, aMsg);
  if aStream1.Size = 0 then
    Exit;
  aStream1.Seek(0, soFromBeginning);
  aStream2.Seek(0, soFromBeginning);
  BufSize := Min(aStream1.Size, 32768);
  SetLength(Buf1, BufSize);
  SetLength(Buf2, BufSize);
  while True do begin
    BytesRead := aStream1.Read(Buf1[0], BufSize);
    Check(BytesRead = aStream2.Read(Buf2[0], BufSize), 'Bytes read mismatch');
    if BytesRead = 0 then
      Exit;
    if not CompareMem(Pointer(Buf1), Pointer(Buf2), BytesRead) then
      for i := 0 to BytesRead - 1 do
        if Buf1[i] <> Buf2[i] then
          FailEquals(IntToHex(Buf1[i], 2), IntToHex(Buf2[i], 2), 'Bytes do not match: ' + aMsg);
  end;
end;

procedure TAbTestCase.CreateDummyFile(aFileName : string; aSize : Integer);
var
  fs : TFileStream;
  bf : pointer;
begin
  fs := TFileStream.Create(aFileName, fmCreate);
  try
    GetMem(bf, aSize + 1);
    try
      // Fill with dummy data might be better in the future to fill less compressable
      // data.
      FillChar(bf^, aSize, 26);
      Fs.Write(bf^, aSize);
    finally
      FreeMem(bf, aSize + 1);
    end;
  finally
    FS.Free;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbTestCase.DelTree(aDir : string);
var
  SR : TSearchRec;
  Directories, Files : TStringList; // FindFirst/FindNext locks directories
  I : Integer;
begin
  // Force Slash
  if aDir[Length(aDir)] <> PathDelim then
    aDir := aDir + PathDelim;
  // If a File is found
  if FindFirst(aDir + '*', faAnyFile, SR) = 0 then begin
    Directories := TStringList.Create;
    Files := TStringList.Create;
    try
      repeat
        if (SR.Attr and faDirectory > 0) then begin
          if (SR.Name <> '.') and (SR.Name <> '..') then
            Directories.Add(aDir + SR.Name)
        end
        else
          Files.Add(aDir + SR.Name);
      until FindNext(SR) <> 0;
      // Close search to free locks on files/directories
      FindClose(SR);
      // Delete all files in directory
      for I := 0 to Files.Count - 1 do
        if not DeleteFile(Files[I]) then
          raise Exception.CreateFmt('Unable to delete "%s"', [Files[I]]);
      // Recursivly call DelTree to get rid of subdirectories
      for I := 0 to Directories.Count -1 do
        DelTree(Directories[I]);
      // Finally remove the directory
      if not RemoveDir(aDir) then
        raise Exception.CreateFmt('Unable to delete "%s"', [aDir]);
    finally
      Directories.Free;
      Files.Free;
    end;
  end; { If File Found with FindFirst }
end;
{ -------------------------------------------------------------------------- }
{$IFDEF WINZIPTESTS}
procedure TAbTestCase.ExecuteAndWait(ExeName, Param: String;TimeOut : Integer);
var
  Spawn : TStSpawnApplication;
  WR    : TWaitResult;
begin
   // Make sure Application trying to execute is found
   CheckFileExists(ExeName);
   Spawn := TStSpawnApplication.Create(nil);
   try
     Spawn.FileName := ExeName;
     Spawn.RunParameters := Param;
     Spawn.NotifyWhenDone := True;
     Spawn.TimeOut := TimeOut;
     Spawn.OnSpawnError := SpawnErrorEvent;
     Spawn.OnCompleted := SpawnCompletedEvent;
     Spawn.OnTimeOut := SpawnTimeOutEvent;
     Spawn.Execute;
     WR := FSpawnComplete.WaitFor(1000);
     While WR <> wrSignaled do
       begin
          Application.ProcessMessages;
          Check(NOT (WR = wrAbandoned), 'Event has been Abandoonded');
          Check(NOT (WR = wrError),'Event has Errored out');
          WR := FSpawnComplete.WaitFor(1000);
       end;
   finally
      Spawn.Free;
   end;
end;
{$ENDIF}

function TAbTestCase.FilesInDirectory(const aDir : string) : TStringList;
var
  SR : TSearchRec;
begin
  Check(DirectoryExists(aDir), 'Directory Requested does not exist : ' + aDir);
  Result := TStringList.Create;
  Result.Sorted := True;
  if FindFirst(AbAddBackSlash(aDir) + '*', faAnyFile, SR) = 0 then begin
    repeat
      if SR.Attr and faDirectory = 0 then // Don't include sub directories
        Result.Add(SR.Name);
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;
end;

function TAbTestCase.GetTestFileDir: string;
begin
  // May want to place in ini file in the future but this will do for now
  Result := ExePath + 'testfiles' + PathDelim;
end;

function TAbTestCase.GetTestTempDir: string;
begin
  Result := GetTestFileDir + 'temp' + PathDelim;
end;

function TAbTestCase.GetWindowsDir: string;
{$IFDEF MSWINDOWS}
var
  aDirBuf : Array[0..MAX_PATH] of Char;
{$ENDIF}
begin
// Windows Directory is used to find
 {$IFDEF LINUX}
   result := '/etc/'
 {$ELSE}
   GetWindowsDirectory(aDirBuf,SizeOf(aDirBuf));
   result := AbAddBackSlash(string(aDirBuf));
 {$ENDIF}
end;

procedure TAbTestCase.SetUp;
begin
  inherited;
  if not DirectoryExists(TestTempDir) then
    CreateDir(TestTempDir);
  {$IFDEF WINZIPTESTS}
    FSpawnComplete := TSimpleEvent.Create;
  {$ENDIF}
end;

{$IFDEF WINZIPTESTS}
procedure TAbTestCase.SpawnCompletedEvent(Sender: TObject);
begin
  FSpawnComplete.SetEvent;
end;

procedure TAbTestCase.SpawnErrorEvent(Sender: TObject; Error: Word);
begin
 FSpawnComplete.SetEvent;
 Fail('Error: ' + IntToSTr(Error) + ' occured launching WinZip');
end;

procedure TAbTestCase.SpawnTimeOutEvent(Sender: TObject);
begin
 FSpawnComplete.SetEvent;
 Fail('Timeout occured launching WinZip');
end;
{$ENDIF}

procedure TAbTestCase.TearDown;
begin
  inherited;
  {$IFDEF WINZIPTESTS}
    FSpawnComplete.Free;
  {$ENDIF}
end;

{ TabCompTestCase }

procedure TAbCompTestCase.SetUp;
begin
  inherited;
  FTestForm := TForm.Create(nil);
  IgnoreProp := TStringList.Create;
end;

procedure TAbCompTestCase.ShowForm;
begin
  FTestForm.ShowModal;
end;

function TAbCompTestCase.StreamComponent(aComp : TComponent) : string;
// The Following was cut and paste out of the Delphi Help File.
var
  BinStream : TMemoryStream;
  StrStream : TStringStream;
begin
  BinStream := TMemoryStream.Create;
  try
    StrStream := TStringStream.Create('');
    try
      BinStream.WriteComponent(aComp);
      BinStream.Seek(0, soFromBeginning);
      ObjectBinaryToText(BinStream, StrStream);
      StrStream.Seek(0, soFromBeginning);
      Result := StrStream.DataString;
    finally
      StrStream.Free;
    end;
  finally
    BinStream.Free
  end;
end;

procedure TAbCompTestCase.TearDown;
begin
  IgnoreProp.free;
  FTestForm.Release; // This allows Message Handling to finish
  inherited;
end;


procedure TAbCompTestCase.CompareComponentProps(aComp1,aComp2 : TPersistent);
var
 PList1   : PPropList;
 PI1,PI2  : PPropInfo;
 PListCnt1 : Integer;
 PList2   : PPropList;
 PListCnt2 : Integer;
 I : Integer;
 SubComp1,SubComp2 : TObject;
 PName1, PName2 : string;
begin
//NOTE: I wrote the following in Delphi 7.   I have had reports that this won't
//      compile in Delphi 5 (Which I don't have access too)    Anyway this could
//      be rewritten to work with Delphi 5 if someone wants to.

  // Check all published properties to see if same.
  PListCnt1 := AbGetPropList(PTypeInfo(aComp1.ClassInfo),PList1);
  PListCnt2 := AbGetPropList(PTypeInfo(aComp2.ClassInfo),PList2);
  // The following should not fail but it here just in case!
  Check(PListCnt1 = PListCnt2,aComp1.ClassName + ' Streaming is really Screwed up!');
  For I := 0 to PListCnt1 -1 do
   begin
     PName1 := string(PList1^[I]^.Name);
     PName2 := string(PList2^[I]^.Name);
     if IgnoreProp.IndexOf(PName1) = -1 then
      begin
         if not(PList1^[I]^.PropType^.Kind = tkClass) then
           Check(AbGetPropValue(aComp1, PName1) = AbGetPropValue(aComp2, PName2), 'Stream Problem with ' + aComp1.ClassName + '.' + PName1)
         else
           begin
              PI1 := GetPropInfo(aComp1.ClassInfo, PName1);
              if Assigned(PI1) then
                SubComp1 := TObject(GetOrdProp(aComp1,PI1))
              else
                SubComp1 := nil;
              PI2 := GetPropInfo(aComp2.ClassInfo,PName1);
              if Assigned(PI2) then
                   SubComp2 := TObject(GetOrdProp(aComp2,PI2))
                else
                   SubComp2 := nil;
              Check((Assigned(SubComp1) and Assigned(SubComp2)) or ((not Assigned(SubComp1)) and (Not Assigned(SubComp1))),'Stream Problem with ' +aComp1.ClassName + '.' + PName2);
              if Assigned(SubComp1) and (SubComp1 is TPersistent) and (SubComp1 is TPersistent) then
              CompareComponentProps(SubComp1 as TPersistent, SubComp2 as TPersistent);
           end;
      end;
   end;

end;

function TAbCompTestCase.UnStreamComponent(const aCompStr : string;
                                           Instance : TComponent) : TComponent;
// The Following was Cut and Paste from the Delphi Help file
var
  StrStream:TStringStream;
  BinStream: TMemoryStream;
  ErrStream  : TFileStream; 
//  CReader : TReader;
begin
  result := nil;
  StrStream := TStringStream.Create(aCompStr);
  try
    BinStream := TMemoryStream.Create;
    try
      try
      ObjectTextToBinary(StrStream, BinStream);
      except
        on E : EParserError do
          begin
             ErrStream := TFileStream.Create('parse.err',fmCreate);
             StrStream.Seek(0,soFromBeginning);
             ErrStream.CopyFrom(StrStream,StrStream.Size);
             ErrStream.Free;
             Fail('Check parse.err ' + E.Message,nil);
             raise;
          end
          else Raise;
      end;
      BinStream.Seek(0, soFromBeginning);
      result := BinStream.ReadComponent(Instance);
    finally
      BinStream.Free;
    end;
  finally
    StrStream.Free;
  end;
end;


function TAbCompTestCase.AbGetPropList(TypeInfo: PTypeInfo;
  out PropList: PPropList): Integer;
begin
  Result := GetTypeData(TypeInfo)^.PropCount;
  if Result > 0 then
  begin
    GetMem(PropList, Result * SizeOf(Pointer));
    GetPropInfos(TypeInfo, PropList);
  end;
end;

function TAbCompTestCase.AbGetPropValue(Instance: TObject;
  const PropName: string; PreferStrings: Boolean): Variant;
var
  PropInfo: PPropInfo;
  S: TIntegerSet;
  TInfo: PTypeInfo;
  I: Integer;
begin
  // assume failure
  Result := Null;

  // get the prop info
  PropInfo := GetPropInfo(Instance.ClassInfo, PropName);
  if PropInfo = nil then
    Raise Exception.Create('Property "' + PropName + '" was not found.')
  else
  begin
    // return the right type
    case PropInfo^.PropType^^.Kind of
      tkInteger, tkChar, tkWChar, tkClass:
        Result := GetOrdProp(Instance, PropInfo);
      tkEnumeration:
        if PreferStrings then
          Result := GetEnumName(PropInfo^.PropType^, GetOrdProp(Instance, PropInfo))
        else if GetTypeData(PropInfo^.PropType^)^.BaseType^ = TypeInfo(Boolean) then
          Result := Boolean(GetOrdProp(Instance, PropInfo))
        else
          Result := GetOrdProp(Instance, PropInfo);
      tkSet:
        if PreferStrings then
          begin
            Result := '';
            Integer(S) := GetOrdProp(Instance, PropInfo);
            TInfo := GetTypeData(PropInfo^.PropType^)^.CompType^;
            for I := 0 to SizeOf(Integer) * 8 - 1 do
            if I in S then
            begin
              if Result <> '' then
                Result := Result + ',';
              Result := Result + GetEnumName(TInfo, I);
            end;
          end
        else
          Result := GetOrdProp(Instance, PropInfo);
      tkFloat:
        Result := GetFloatProp(Instance, PropInfo);
      tkMethod:
        Result := PropInfo^.PropType^.Name;
      tkString, tkLString:
        Result := GetStrProp(Instance, PropInfo);
      tkWString:
       {$IFDEF VERSION6}
        Result := GetWideStrProp(Instance, PropInfo);
       {$ELSE}
        Result := ''; //No simple way to get this in Prior Delphi versions... and we don't use WideStrings (yet)
       {$ENDIF}
      tkVariant:
        Result := GetVariantProp(Instance, PropInfo);
      tkInt64:
          {$IFDEF VERSION6}
             Result := GetInt64Prop(Instance, PropInfo);
          {$ELSE}
             Result := '';
          {$ENDIF}
          tkDynArray:
                DynArrayToVariant(Result, Pointer(GetOrdProp(Instance, PropInfo)), PropInfo^.PropType^);
      {$IFDEF CONDITIONALEXPRESSIONS}
        {$IF DECLARED(tkUString)}
          tkUString:
            Result := GetUnicodeStrProp(Instance, PropInfo);
        {$IFEND}
      {$ENDIF}
    else
      raise Exception.Create('Invalid Property Type: ' + string(PropInfo.PropType^^.Name));
    end;
  end;
end;

procedure TAbCompTestCase.TestComponentLink(AComponent: TComponent;
  const APropName: string; APropClass: TComponentClass);
  {- Create a component of the given type, assign it to the property, then
     free the object and test that the property has been nilled. }
var
  PropObject: TComponent;
begin
  PropObject := APropClass.Create(TestForm);
  SetObjectProp(AComponent, APropName, PropObject);
  Check(GetObjectProp(AComponent, APropName) = PropObject,
    Format('SetObjectProp failed for %s.%s', [AComponent.ClassName, APropName]));
  PropObject.Free;
  CheckNull(GetObjectProp(AComponent, APropName),
    Format('Notification does not work for %s.%s', [AComponent.ClassName, APropName]));
end;

initialization
  // Cache on startup;  on Linux ParamStr(0) may not be fully qualified, and
  // the tests change the working directory.
  ExePath := ExtractFilePath(ExpandFileName(ParamStr(0)))

end.
