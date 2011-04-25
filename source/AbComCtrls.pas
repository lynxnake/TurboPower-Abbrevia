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
 * Portions created by the Initial Developer are Copyright (C) 2011
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
//TODO: Finish up listview w/o treeview mode
//TODO: Clean up published properties
//TODO: Figure out how to show encrypted correctly
//TODO: Tighten up component cooperation
//TODO: Fix sorting by columns other than the name
//TODO: Add to AbbreviaVCL packages
unit AbComCtrls;

interface

{$I AbDefine.inc}

uses
  Windows, Messages, SysUtils, Classes, Controls, ComCtrls, Graphics, AbBrowse;

const
  AbTreeArchiveImage        = 0;
  AbTreeFolderImage         = 1;
  AbTreeFolderExpandedImage = 2;

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
    FListView: TAbListView;

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
  CommCtrl, Contnrs, ShellAPI, StrUtils, AbArcTyp, AbResString, AbUtils,
  AbZipTyp;

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
const
  ColWidths: array[TAbViewColumn] of Integer = (
    180{vcName}, 110{vcFileType}, 130{vcLastModified}, 80{vcSize}, 50{vcRatio},
    80{vcPacked}, 70{vcCRC}, 30{vcAttributes}, 28{vcEncrypted}, 60{vcMethod},
    300{vcPath});
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
      Column.Width := ColWidths[Col];
      Column.Tag := Ord(Col);
      if Col in [vcSize, vcRatio, vcPacked] then
        Column.Alignment := taRightJustify;
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
  Col: TAbViewColumn;
  CurItem: TAbArchiveItem;
  DOSAttr: Integer;
  i: Integer;
  Item: TListItem;
  Filename, ColText: string;
  ColImage: Integer;
  sfi: SHFILEINFO;
begin
  Items.BeginUpdate;
  try
    Items.Clear;
    if Assigned(FArchive) then begin
      for i := 0 to FArchive.Count - 1 do
        if FArchive[i].Action <> aaDelete then begin
          CurItem := FArchive[i];
          // Only include items that match the current path
          Filename := AbNormalizeFilename(CurItem.FileName);
          if Path <> ExtractFileDir(Filename) then
            Continue;
          // Get file type information from the shell
          if CurItem.IsDirectory then
            DOSAttr := FILE_ATTRIBUTE_DIRECTORY
          else
            DOSAttr := FILE_ATTRIBUTE_NORMAL;
          SHGetFileInfo(PChar(ExtractFileName(Filename)), DOSAttr, sfi, sizeof(sfi),
            SHGFI_TYPENAME or SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES);
          // Create new item
          Item := Items.Add;
          Item.Data := FArchive[i];
          // Fill in columns
          Item.Caption := ExtractFileName(Filename);
          Item.ImageIndex := sfi.iIcon;
          Item.SubItems.Clear;
          for Col := Succ(Low(Col)) to High(Col) do
            if Col in FColumns then begin
              ColText := '';
              ColImage := -1;
              case Col of
                vcFileType:
                  ColText := sfi.szTypeName;
                vcLastModified:
                  ColText := FormatDateTime(ShortDateFormat + ' ' + LongTimeFormat,
                    CurItem.LastModTimeAsDateTime);
                vcSize:
                  if not CurItem.IsDirectory then
                    ColText := FormatFloat('#,##0', CurItem.UncompressedSize);
                vcRatio:
                  if not CurItem.IsDirectory then
                    if CurItem.UncompressedSize > 0 then
                      ColText := Format('%d%%',
                        [CurItem.CompressedSize * 100 div CurItem.UncompressedSize])
                    else
                      ColText := '0%';
                vcPacked:
                  if not CurItem.IsDirectory then
                    ColText := FormatFloat('#,##0', CurItem.CompressedSize);
                vcCRC:
                  if not CurItem.IsDirectory then
                    ColText := IntToHex(CurItem.CRC32, 8);
                vcAttributes:
                  begin
                    {$WARN SYMBOL_PLATFORM OFF}
                    if (faReadOnly and CurItem.ExternalFileAttributes) = faReadOnly then
                      ColText := ColText + AbReadOnlyS;
                    if (faHidden and CurItem.ExternalFileAttributes) = faHidden then
                      ColText := ColText + AbHiddenS;
                    if (faSysFile and CurItem.ExternalFileAttributes) = faSysFile then
                      ColText := ColText + AbSystemS;
                    if (faArchive and CurItem.ExternalFileAttributes) = faArchive then
                      ColText := ColText + AbArchivedS;
                    {$WARN SYMBOL_PLATFORM ON}
                  end;
                vcEncrypted:
                  if CurItem.IsEncrypted then
                    ColText := '+';
                vcMethod:
                  if CurItem is TAbZipItem then
                    ColText := ZipCompressionMethodToString(TAbZipItem(CurItem).CompressionMethod);
                vcPath:
                  ColText := ExtractFileDir(FileName);
              end;
              Item.SubItems.Add(ColText);
              Item.SubItemImages[Item.SubItems.Count - 1] := ColImage;
            end;
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
  Bmp : TBitmap;
  Icon : TIcon;
  sfi: SHFILEINFO;
begin
  inherited;
  Images := TImageList.Create(Self);
  Bmp := TBitmap.Create;
  try
    Bmp.LoadFromResourceName(0, 'AbComCtrls_Zip');
    Images.AddMasked(Bmp, clBlack);
    Icon := TIcon.Create;
    try
      SHGetFileInfo('', FILE_ATTRIBUTE_DIRECTORY, sfi, sizeof(sfi),
        SHGFI_ICON or SHGFI_SMALLICON or SHGFI_USEFILEATTRIBUTES);
      Icon.Handle := sfi.hIcon;
      Bmp.PixelFormat := pf24bit;
      Bmp.Canvas.Brush.Color := clWindow;
      Bmp.Canvas.FillRect(Rect(0, 0, 16, 16));
      Bmp.Canvas.Draw(0, 0, Icon);
      Images.AddMasked(Bmp, clWindow);
      SHGetFileInfo('', FILE_ATTRIBUTE_DIRECTORY, sfi, sizeof(sfi),
        SHGFI_ICON or SHGFI_OPENICON or SHGFI_SMALLICON or SHGFI_USEFILEATTRIBUTES);
      Icon.Handle := sfi.hIcon;
      Bmp.Canvas.FillRect(Rect(0, 0, 16, 16));
      Bmp.Canvas.Draw(0, 0, Icon);
      Images.AddMasked(Bmp, clWindow);
    finally
      Icon.Free;
    end;
  finally
    Bmp.Free;
  end;
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
      Result.ExpandedImageIndex := AbTreeFolderExpandedImage;
      Result.ImageIndex := AbTreeFolderImage;
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
        ZipNode.ExpandedImageIndex := AbTreeArchiveImage;
        ZipNode.ImageIndex := AbTreeArchiveImage;
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
