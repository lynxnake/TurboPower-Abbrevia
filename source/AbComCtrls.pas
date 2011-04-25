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
 * The Initial Developer of the Original Code is Craig Peterson
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Craig Peterson <capeterson@users.sourceforge.net>
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* ABBREVIA: AbComCtrls.pas                              *}
{*********************************************************}
{* ABBREVIA: Listview and treeview components that work  *}
{*   with an archive component.  The treeview can have a *}
{*   listview associated, in which case the listview will*}
{*   only show items in the selected folder.             *}
{*********************************************************}

unit AbComCtrls;

interface

{$I AbDefine.inc}

uses
  Windows, Messages, SysUtils, Classes, Controls, ComCtrls, Graphics, AbBrowse;

type
  TAbTreeView = class;

  TAbViewColumn =
    (vcName, vcFileType, vcLastModified, vcSize, vcRatio,
     vcPacked, vcCRC, vcAttributes, vcEncrypted, vcMethod, vcPath);
  TAbViewColumns = set of TAbViewColumn;

{ ===== TAbListView ========================================================= }
  TAbListView = class(TListView)
  protected {private}
    FArchive : TAbBaseBrowser;
    FColumns : TAbViewColumns;
    FHeaderImages: TImageList;
    FPath : string;
    FTreeView : TAbTreeView;

  protected {methods}
    procedure CreateWnd;
      override;
    procedure DoChange(Sender : TObject);
      virtual;
    procedure Notification(aComponent : TComponent; aOperation : TOperation);
      override;
    procedure SetArchive(aValue : TAbBaseBrowser);
    procedure SetColumns(aValue : TAbViewColumns);
    procedure SetPath(aValue : string);
    procedure UpdateView;

  protected {properties}
    property HeaderImages : TImageList
      read FHeaderImages;
    property TreeView : TAbTreeView
      read FTreeView
      write FTreeView;

  public {methods}
    constructor Create(aOwner: TComponent);
      override;

  published
    property Archive : TAbBaseBrowser
      read FArchive
      write SetArchive;
    property ColumnsX : TAbViewColumns
      read FColumns
      write SetColumns;
    property Path : string
      read FPath
      write SetPath;
  end;

{ ===== TAbTreeView ========================================================= }
  TAbTreeView = class(TTreeView)
  protected {private}
    FArchive: TAbBaseBrowser;
    FExpandedImageIndex: Integer;
    FFolderImageIndex: Integer;
    FListView: TAbListView;
    FZipImageIndex: Integer;

  protected {methods}
    procedure Change(aNode: TTreeNode);
      override;
    procedure DoChange(Sender : TObject);
      virtual;
    procedure DoLoad(Sender : TObject);
      virtual;
    procedure GetSelectedIndex(aNode: TTreeNode);
      override;
    procedure Notification(aComponent : TComponent; aOperation : TOperation);
      override;
    procedure SetArchive(aValue: TAbBaseBrowser);
    procedure SetListView(aValue: TAbListView);

  public {methods}
    constructor Create(aOwner: TComponent);
      override;

  published {properties}
    property Archive: TAbBaseBrowser
      read FArchive
      write SetArchive;
    property ListView: TAbListView
      read FListView
      write SetListView;
  end;

implementation

{$R AbComCtrls.res}

uses
  CommCtrl, Contnrs, ShellAPI, StrUtils, AbArcTyp, AbResString, AbUtils;

function AbNormalizeFilename(const aFilename: string): string;
var
  i: Integer;
begin
  Result := aFilename;
  for i := 1 to Length(Result) do
    if IsDelimiter('\/', Result, i) then
      Result[i] := PathDelim;
  if IsDelimiter(PathDelim, Result, Length(Result)) then
    SetLength(Result, Length(Result) - 1);
end;
{ -------------------------------------------------------------------------- }
function SortProc(aItem1, aItem2: TListItem; alParam: Integer): Integer;
  stdcall;
begin
  if TAbArchiveItem(aItem1.Data).IsDirectory <>
     TAbArchiveItem(aItem2.Data).IsDirectory then
    if TAbArchiveItem(aItem1.Data).IsDirectory then
      Result := -1
    else
      Result := 1
  else
    Result := CompareText(aItem1.Caption, aItem2.Caption)
end;

{ ===== TAbListView ========================================================= }
constructor TAbListView.Create(aOwner: TComponent);
var
  Bmp : TBitmap;
  sfi: SHFILEINFO;
begin
  inherited;
  // Load header image into an image list;  the header's hbm property
  // doesn't support transparency
  FHeaderImages := TImageList.Create(Self);
  Bmp := TBitmap.Create;
  try
    Bmp.LoadFromResourceName(0, 'AbComCtrls_Lock');
    FHeaderImages.AddMasked(Bmp, clBlack)
  finally
    Bmp.Free;
  end;
  // Load system image lists
  LargeImages := TImageList.Create(Self);
  LargeImages.ShareImages := True;
  LargeImages.Handle := SHGetFileInfo('', 0, sfi, SizeOf(sfi),
    SHGFI_LARGEICON or SHGFI_SYSICONINDEX);
  SmallImages := TImageList.Create(Self);
  SmallImages.ShareImages := True;
  SmallImages.Handle := SHGetFileInfo('', 0, sfi, SizeOf(sfi),
    SHGFI_SMALLICON or SHGFI_SYSICONINDEX);

  ColumnsX := [vcName, vcFileType, vcLastModified, vcSize, vcRatio,
     vcPacked, vcCRC, vcAttributes, vcEncrypted, vcMethod, vcPath];
end;
{ -------------------------------------------------------------------------- }
procedure TAbListView.CreateWnd;
begin
  inherited;
  Header_SetImageList(ListView_GetHeader(Handle), FHeaderImages.Handle);
end;
{ -------------------------------------------------------------------------- }
procedure TAbListView.DoChange(Sender: TObject);
begin
  UpdateView;
  if (Sender = FArchive) and Assigned(FTreeView) then
    FTreeView.DoChange(Self);
end;
{ -------------------------------------------------------------------------- }
procedure TAbListView.Notification(aComponent: TComponent;
  aOperation: TOperation);
begin
  inherited;
  if aOperation = opRemove then
    if aComponent = FArchive then begin
      FArchive := nil;
      Clear;
    end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbListView.SetArchive(aValue: TAbBaseBrowser);
begin
  if Assigned(FArchive) then
    FArchive.RemoveFreeNotification(Self);
  FArchive := aValue;
  if Assigned(FArchive) then
    FArchive.FreeNotification(Self);
  UpdateView;
end;
{ -------------------------------------------------------------------------- }
procedure TAbListView.SetColumns(aValue : TAbViewColumns);
var
  Col: TAbViewColumn;
  Column: TListColumn;
begin
  if aValue <> FColumns then begin
    inherited Columns.Clear;
    FColumns := aValue;
    for Col := Low(Col) to High(Col) do begin
      if not (Col in FColumns) then
        Continue;
      Column := inherited Columns.Add;
      case Col of
        vcName: Column.Caption := AbItemNameHeadingS;
        vcFileType: Column.Caption := AbFileTypeHeadingS;
        vcLastModified: Column.Caption := AbLastModifiedHeadingS;
        vcSize: Column.Caption := AbFileSizeHeadingS;
        vcRatio: Column.Caption := AbRatioHeadingS;
        vcPacked: Column.Caption := AbPackedHeadingS;
        vcCRC: Column.Caption := AbCRCHeadingS;
        vcAttributes: Column.Caption := AbFileAttrHeadingS;
        vcEncrypted: Column.ImageIndex := 0;
        vcMethod: Column.Caption := AbMethodHeadingS;
        vcPath: Column.Caption := AbPathHeadingS;
      end;
      Column.Tag := Ord(Col);
      if Col in [vcSize, vcRatio, vcPacked] then
        Column.Alignment := taRightJustify;
//    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
//    property MaxWidth: TWidth read FMaxWidth write SetMaxWidth default 0;
//    property MinWidth: TWidth read FMinWidth write SetMinWidth default 0;
//    property Width: TWidth read GetWidth write SetWidth stored IsWidthStored default 50;
    end;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbListView.SetPath(aValue: string);
begin
  FPath := aValue;
  UpdateView;
end;
{ -------------------------------------------------------------------------- }
procedure TAbListView.UpdateView;
var
  DOSAttr: Integer;
  i: Integer;
  Item: TListItem;
  Filename: string;
  sfi: SHFILEINFO;
begin
  Items.BeginUpdate;
  try
    Items.Clear;
    if Assigned(FArchive) then begin
      for i := 0 to FArchive.Count - 1 do
        if FArchive[i].Action <> aaDelete then begin
          Filename := AbNormalizeFilename(FArchive[i].FileName);
          if Path <> ExtractFileDir(Filename) then
            Continue;
          Item := Items.Add;
          Item.Caption := ExtractFileName(Filename);
          Item.Data := FArchive[i];
          if FArchive[i].IsDirectory then
            DOSAttr := FILE_ATTRIBUTE_DIRECTORY
          else
            DOSAttr := FILE_ATTRIBUTE_NORMAL;
          SHGetFileInfo(PChar(Item.Caption), DOSAttr, sfi, sizeof(sfi),
            SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES);
          Item.ImageIndex := sfi.iIcon;
        end;
      CustomSort(TLVCompare(@SortProc), 0);
    end;
  finally
    Items.EndUpdate;
  end;
end;


{ ===== TAbTreeView ========================================================= }
constructor TAbTreeView.Create(aOwner: TComponent);
var
  sfi: SHFILEINFO;
begin
  inherited;
  Images := TImageList.Create(Self);
  Images.ShareImages := True;
  Images.Handle := SHGetFileInfo('', 0, sfi, SizeOf(sfi),
    SHGFI_SMALLICON or SHGFI_SYSICONINDEX);
  SHGetFileInfo(nil, FILE_ATTRIBUTE_DIRECTORY, sfi, sizeof(sfi),
    SHGFI_OPENICON or SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES);
  FExpandedImageIndex := sfi.iIcon;
  SHGetFileInfo(nil, FILE_ATTRIBUTE_DIRECTORY, sfi, sizeof(sfi),
    SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES);
  FFolderImageIndex := sfi.iIcon;
  SHGetFileInfo('.zip', FILE_ATTRIBUTE_NORMAL, sfi, sizeof(sfi),
    SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES);
  FZipImageIndex := sfi.iIcon;
end;
{ -------------------------------------------------------------------------- }
procedure TAbTreeView.Change(aNode: TTreeNode);
var
  Path: string;
begin
  inherited;
  if aNode.Selected and Assigned(ListView) then begin
    Path := '';
    if aNode <> Items.GetFirstNode then begin
      Path := aNode.Text;
      aNode := aNode.Parent;
      while aNode <> Items.GetFirstNode do begin
        Path := aNode.Text + PathDelim + Path;
        aNode := aNode.Parent;
      end;
    end;
    ListView.Path := Path;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbTreeView.DoChange(Sender: TObject);
var
  Nodes: TStringList;
  ZipNode: TTreeNode;

  function GetNode(const aFilename: string): TTreeNode;
  var
    i: Integer;
  begin
    if aFilename = '' then
      Result := ZipNode
    else if Nodes.Find(aFilename, i) then
      Result := TTreeNode(Nodes.Objects[i])
    else begin
      Result := Items.AddChild(GetNode(ExtractFileDir(aFilename)),
                               ExtractFileName(aFilename));
      Result.ExpandedImageIndex := FExpandedImageIndex;
      Result.ImageIndex := FFolderImageIndex;
      Nodes.AddObject(aFilename, Result);
    end;
  end;

var
  i: Integer;
  Filename: string;
begin
  Items.BeginUpdate;
  try
    Items.Clear;
    if Assigned(FArchive) then begin
      Nodes := TStringList.Create;
      try
        Nodes.Sorted := True;
        if Archive.FArchive <> nil then
          Filename := ExtractFileName(Archive.FArchive.ArchiveName)
        else
          Filename := PathDelim;
        ZipNode := Items.AddChild(nil, Filename);
        ZipNode.ExpandedImageIndex := FZipImageIndex;
        ZipNode.ImageIndex := FZipImageIndex;
        for i := 0 to FArchive.Count - 1 do
          if FArchive[i].Action <> aaDelete then begin
            Filename := AbNormalizeFilename(FArchive[i].FileName);
            if not FArchive[i].IsDirectory then
              Filename := ExtractFileDir(Filename);
            GetNode(Filename);
          end;
      finally
        Nodes.Free;
      end;
      Items.AlphaSort(True);
    end;
  finally
    Items.EndUpdate;
  end;
  if (Sender = FArchive) and Assigned(FListView) then
    FListView.DoChange(Self);
end;
{ -------------------------------------------------------------------------- }
procedure TAbTreeView.DoLoad(Sender: TObject);
begin

end;
{ -------------------------------------------------------------------------- }
procedure TAbTreeView.GetSelectedIndex(aNode: TTreeNode);
begin
  if aNode.Expanded then
    aNode.SelectedIndex := aNode.ExpandedImageIndex
  else
    aNode.SelectedIndex := aNode.ImageIndex;
end;
{ -------------------------------------------------------------------------- }
procedure TAbTreeView.Notification(aComponent: TComponent;
  aOperation: TOperation);
begin
  inherited Notification(aComponent, aOperation);
  if aOperation = opRemove then begin
    if aComponent = FArchive then begin
      FArchive := nil;
      Items.Clear;
    end;
    if aComponent = FListView then
      FListView := nil;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbTreeView.SetArchive(aValue: TAbBaseBrowser);
begin
  if Assigned(FArchive) then begin
    FArchive.RemoveFreeNotification(Self);
    FArchive.OnChange := nil;
    FArchive.OnLoad := nil;
  end;
  FArchive := aValue;
  if Assigned(FArchive) then begin
    FArchive.FreeNotification(Self);
    FArchive.OnChange := DoChange;
    FArchive.OnLoad := DoLoad;
    DoChange(Self);
  end
  else
    Items.Clear;
end;
{ -------------------------------------------------------------------------- }
procedure TAbTreeView.SetListView(aValue: TAbListView);
begin
  if Assigned(FListView) then begin
    FListView.RemoveFreeNotification(Self);
    FListView.TreeView := nil;
  end;
  FListView := aValue;
  if Assigned(FListView) then begin
    FListView.FreeNotification(Self);
    FListView.TreeView := Self;
  end;
end;

end.
