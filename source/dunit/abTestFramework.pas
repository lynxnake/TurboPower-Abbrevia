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

unit abTestFramework;

interface
{$I AbDefine.inc}

uses TestFramework, SysUtils, Classes, TypInfo, {$IFDEF VERSION6} Variants, {$ENDIF} {$IFDEF LINUX} QForms,QControls {$ELSE}Forms,Controls{$ENDIF};

type

{$IFNDEF VERSION6}
  TIntegerSet = set of 0..SizeOf(Integer) * 8 - 1;
{$ENDIF}


 TabTestCase = class(TTestCase)
   protected
    function GetTestFileDir : string;
    procedure CheckStreamMatch(aStream1,aStream2 : TStream;Msg : String);
   public
     property TestFileDir : String read GetTestFileDir;
   end;

 TabCompTestCase = class(TabTestCase)
   protected
     FTestForm : TForm;
     FComponentClass : TComponentClass;
     IgnoreProp : TStringList;
     procedure SetUp; override;
     procedure TearDown; override;

     // RTTI function so that they match from version to version of delphi.
     function AbGetPropList(TypeInfo: PTypeInfo; out PropList: PPropList): Integer;
     function AbGetPropValue(Instance: TObject; const PropName: string; PreferStrings: Boolean = True): Variant;


     function StreamComponent(aComp : TComponent) : String;
     function UnStreamComponent(aCompStr : String;Instance : TComponent = nil) : TComponent; overload;

     procedure CompareComponentProps(aComp1,aComp2 : TPersistent); virtual;
   public
    property TestForm : TForm read FTestForm;
    procedure ShowForm; virtual;
   published
   end;


implementation


{ TabTestCase }

procedure TabTestCase.CheckStreamMatch(aStream1, aStream2: TStream;
  Msg: String);
var
 I : Integer;
 b1,b2 : Byte;
begin
 aStream1.Seek(0,soFromBeginning);
 aStream2.Seek(0,soFromBeginning);
 if aStream1.Size <> aStream2.Size then
    Fail(Msg,CallerAddr);
 for I := 0 to aStream1.Size -1 do
   begin
     aStream1.Read(b1,1);
     aStream2.Read(b2,1);
     if (b1 <> b2) then
       Fail(Msg,CallerAddr);
   end;
end;

function TabTestCase.GetTestFileDir: string;
begin
// May want to place in ini file in the future but this will do for now
{$IFDEF LINUX}
// I don't think this will work with linux so may need to change
 result := ExtractFilePath(ParamStr(0)) + 'TestFiles/';
{$ELSE}
 result := ExtractFilePath(ParamStr(0)) + 'TestFiles\';
{$ENDIF}
end;

{ TabCompTestCase }

procedure TabCompTestCase.SetUp;
begin
  inherited;
  FTestForm := TForm.Create(nil);
  IgnoreProp := TStringList.create;  
end;

procedure TabCompTestCase.ShowForm;
begin
  FTestForm.ShowModal;
end;

function TabCompTestCase.StreamComponent(aComp: TComponent): String;
// The Following was cut and paste out of the Delphi Help File.
var
  BinStream:TMemoryStream;
  StrStream: TStringStream;
  s: string;
begin
  BinStream := TMemoryStream.Create;
  try
    StrStream := TStringStream.Create(s);
    try
      BinStream.WriteComponent(aComp);
      BinStream.Seek(0, soFromBeginning);
      ObjectBinaryToText(BinStream, StrStream);
      StrStream.Seek(0, soFromBeginning);
      Result:= StrStream.DataString;
    finally
      StrStream.Free;
    end;
  finally
    BinStream.Free
  end;
end;

procedure TabCompTestCase.TearDown;
begin
  IgnoreProp.free;
  FTestForm.Release; // This allows Message Handling to finish
  inherited;
end;


procedure TabCompTestCase.CompareComponentProps(aComp1,aComp2 : TPersistent);
var
 PList1   : PPropList;
 PI1,PI2  : PPropInfo;
 PListCnt1 : Integer;
 PList2   : PPropList;
 PListCnt2 : Integer;
 I : Integer;
 SubComp1,SubComp2 : TObject;
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
     if IgnoreProp.IndexOf(PList1^[I]^.Name) = -1 then
      begin
         if not(PList1^[I]^.PropType^.Kind = tkClass) then
           Check(AbGetPropValue(aComp1,PList1^[I]^.Name) = AbGetPropValue(aComp2,PList2^[I]^.Name), 'Stream Problem with ' +aComp1.ClassName + '.' + PList2^[I]^.Name)
         else
           begin
              PI1 := GetPropInfo(aComp1.ClassInfo,PList1^[I]^.Name);
              if Assigned(PI1) then
                SubComp1 := TObject(GetOrdProp(aComp1,PI1))
              else
                SubComp1 := nil;
              PI2 := GetPropInfo(aComp2.ClassInfo,PList1^[I]^.Name);
              if Assigned(PI2) then
                   SubComp2 := TObject(GetOrdProp(aComp2,PI2))
                else
                   SubComp2 := nil;
              Check((Assigned(SubComp1) and Assigned(SubComp2)) or ((not Assigned(SubComp1)) and (Not Assigned(SubComp1))),'Stream Problem with ' +aComp1.ClassName + '.' + PList2^[I]^.Name);
              if Assigned(SubComp1) and (SubComp1 is TPersistent) and (SubComp1 is TPersistent) then
              CompareComponentProps(SubComp1 as TPersistent, SubComp2 as TPersistent);
           end;
      end;
   end;

end;

function TabCompTestCase.UnStreamComponent(aCompStr: String;Instance : TComponent): TComponent;
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


function TabCompTestCase.AbGetPropList(TypeInfo: PTypeInfo;
  out PropList: PPropList): Integer;
begin
  Result := GetTypeData(TypeInfo)^.PropCount;
  if Result > 0 then
  begin
    GetMem(PropList, Result * SizeOf(Pointer));
    GetPropInfos(TypeInfo, PropList);
  end;
end;

function TabCompTestCase.AbGetPropValue(Instance: TObject;
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
    else
      raise Exception.Create('Invalid Property Type: ' + PropInfo.PropType^^.Name);
    end;
  end;
end;

end.
 