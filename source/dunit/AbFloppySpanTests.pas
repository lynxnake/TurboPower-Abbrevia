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

unit AbFloppySpanTests;
{$I AbDefine.inc}
// The winzip test require Systools (http://sf.net/projects/tpsystools), stSpawn.pas
// Inactivate Define to remove dependance on systools
{$DEFINE WINZIPTESTS}
interface

uses
// Note: The Floppy Span tests are designed to be platform specific

  Windows, Forms, Dialogs,Controls,SysUtils, Classes, TestFrameWork,
  abTestFramework, AbZipper, AbUnzper{$IFDEF WINZIPTESTS},SyncObjs{$ENDIF} ;

type
  TAbFloppySpanTests = class(TabTestCase)
  protected
    WinDir : String;

    {$IFDEF WINZIPTESTS}
      FSpawnComplete : TSimpleEvent;
      procedure SpawnErrorEvent(Sender : TObject; Error : Word);
      procedure SpawnCompletedEvent(Sender : TObject);
      procedure SpawnTimeOutEvent(Sender : TObject);
    {$ENDIF}
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CreateBasicSpan;
    procedure VerifyBasicSpan;
    {$IFDEF WINZIPTESTS}
    procedure WinzipExtractTest;
    {$ENDIF}
  end;

implementation

uses ShellAPI {$IFDEF WINZIPTESTS}, stSpawn {$ENDIF} ;




{ TAbFloppySpanTests }
procedure TAbFloppySpanTests.SetUp;
begin
  inherited;
   // Get directory windows is installed to
   SetLength(WinDir,MAX_PATH);
   SetLength(WinDir,GetWindowsDirectory(pchar(WinDir), MAX_PATH));
   WinDir := IncludeTrailingBackslash(WinDir);
  {$IFDEF WINZIPTESTS}
    FSpawnComplete := TSimpleEvent.Create;
  {$ENDIF}
end;


procedure TAbFloppySpanTests.VerifyBasicSpan;
var
  UnZip : TAbUnZipper;
  mStream : TMemoryStream;
  fStream : TFileStream;
  I : Integer;
  TestFile : string;
begin
   // Create a Basic Span by zipping all .EXE Files in C:\WINDOWS\
   // On My(Robert Love) Machine this takes two disks to do.

   if MessageDlg('This test requires the TESTSPAN.ZIP created in the '+#13+#10+'"CreateBasicSpan" test.  Please Insert Disk #1'+#13+#10+''+#13+#10+'Pressing Cancel will terminate this test.', mtInformation, [mbOK,mbCancel], 0) = mrCancel then
    Fail('Test Aborted');

   UnZip := TAbUnZipper.create(nil);
   mStream := TMemoryStream.Create;
   try
   UnZip.BaseDirectory := GetTestTempDir;
   CheckFileExists('A:\SPANTEST.ZIP');
   UnZip.FileName := 'A:\SPANTEST.ZIP';
   Check(Unzip.Count > 0,'Archive A:\SPANTEST.ZIP is empty');
   For I := 0 to Unzip.Count -1 do
    begin
      testFile := WinDir + ExtractFileName(UnZip.Items[I].FileName);
      // Make sure file exist to compare to.
      CheckFileExists(TestFile);
      // Extract File in Span to Memory
      UnZip.ExtractToStream(UnZip.Items[I].FileName,mStream);
      // Open the Existing File in Read Only Mode.
      fStream := TFileStream.Create(TestFile,fmOpenRead);
      try
      // Make sure memory Stream of File and Actual File Match, Byte for Byte (This takes some time to complete)
      CheckStreamMatch(mStream,fStream,'Test File: ' + TestFile + ' did not Match Archive');
      finally
        fStream.Free;
      end;
      mStream.SetSize(0);
    end;
   finally
     UnZip.Free;
     mStream.Free;
   end;

end;

procedure TAbFloppySpanTests.CreateBasicSpan;
var
  Zip : TAbZipper;
begin
   // UnZips the Basic Span that was created by zipping all .EXE Files in C:\WINDOWS\
   // Compares each file byte by byte to original.
   if MessageDlg('Insert Disk #1 to create A:\TESTSPAN.ZIP'+#13+#10+''+#13+#10+'Pressing Cancel Abort Test', mtWarning, [mbOK,mbCancel], 0) = mrCancel then
     Fail('User Aborted Test'); 
   Zip := TAbZipper.create(nil);
   try
   Zip.BaseDirectory := WinDir;
   Zip.FileName := 'A:\SPANTEST.ZIP';
   Zip.AddFiles('*.EXE',faAnyFile);
   Zip.Save;
   finally
     Zip.Free;
   end;
end;
{$IFDEF WINZIPTESTS}
procedure TAbFloppySpanTests.WinzipExtractTest;
var
  UnWinZip : String;
  ExtractTo : String;
  FileList : TStringList;
  Spawn : TStSpawnApplication;
  SR : TSearchRec;
  I : integer;
  FS1,FS2 : TFileStream;
  WR : TWaitResult;
begin
 // This test will use the Winzip command line utility to determine if the
 // file created in CreateBasicSpan can be extracted.
 // This is a good comptability routine.

    if MessageDlg('This test requires the TESTSPAN.ZIP created in the '+#13+#10+'"CreateBasicSpan" test. '+ #13#10 + 'It also requires WinZIP Command Line Utility. '+#13#10#13#10 +'Please Insert LAST Disk of span set.'+#13+#10+''+#13+#10+'Pressing Cancel will terminate this test.', mtInformation, [mbOK,mbCancel], 0) = mrCancel then
      Fail('Test Aborted');
// Hard Coded as I could not find install location in the Registry to extract and make dynamic
// if this proves to be a problem, we will have to have test configuration file that specifies
// the winzip command line utility path.
   UnWinZip := 'C:\Program Files\WinZip\wzunzip.exe';
   CheckFileExists(UnWinZip);

   Spawn := TStSpawnApplication.Create(nil);
   try
     Spawn.FileName := UnWinZip;
     ExtractTo := TestTempDir + 'WZSpan\';
     DelTree(ExtractTo);
     Check(DirectoryExists(extractTo),'DelTree() was just called it Directory should be deleted');
     CreateDir(ExtractTo);
     Spawn.RunParameters := 'A:\SPANTEST.ZIP ' + ExtractTo;
     Spawn.NotifyWhenDone := True;
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
       
     // Files have now been extracted Time to test.
     FileList := TStringList.Create;
     try
       // Find all the files in the extact Directory
       if FindFirst(ExtractTo + '*.*',faAnyFile,SR) = 0 then
        begin
          repeat
          if not (SR.Attr = faDirectory) then
             FileList.Add(SR.Name);
          until FindNext(SR) <> 0;
        end;
        FindClose(SR);
     // Make Sure Files where Extracted
     Check(Filelist.Count > 0,'Unable to find any extract files');
     For I := 0 to FileList.Count -1 do
      begin
        FS1 := TFileStream.Create(WinDir + ExtractFileName(FileList.Strings[I]),fmOpenRead);
        FS2 := TFileStream.Create(ExtractTo + ExtractFileName(FileList.Strings[I]),fmOpenRead);
        try
          // Make sure Files Match
          CheckStreamMatch(FS1,FS2,FileList.Strings[I] + 'Did not match Master File');
        finally
          FS1.Free;
          FS2.Free;
        end;
      end;
    finally
      FileList.free;
    end;
   finally
     Spawn.Free;
   end;
end;


procedure TAbFloppySpanTests.SpawnCompletedEvent(Sender: TObject);
begin
  FSpawnComplete.SetEvent;
end;

procedure TAbFloppySpanTests.SpawnErrorEvent(Sender: TObject; Error: Word);
begin
 FSpawnComplete.SetEvent;
 Fail('Error: ' + IntToSTr(Error) + ' occured launching WinZip');
end;

procedure TAbFloppySpanTests.SpawnTimeOutEvent(Sender: TObject);
begin
  FSpawnComplete.SetEvent;
 Fail('Timeout occured launching WinZip');
end;
{$ENDIF}


procedure TAbFloppySpanTests.TearDown;
begin
  inherited;
  {$IFDEF WINZIPTESTS}
    FSpawnComplete.Free;
  {$ENDIF}
end;

initialization

  TestFramework.RegisterTest('Abbrevia.Floppy Spanning Suite',
    TAbFloppySpanTests.Suite);

end.

