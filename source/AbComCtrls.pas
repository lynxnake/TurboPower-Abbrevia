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
//TODO: Add to AbbreviaVCL packages
//TODO: Listview encrypted column
//TODO: Listview column sorting
//TODO: Listview support for implicit folders
//TODO: Tighten up component cooperation
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
  TAbCustomTreeView = class;

  TAbViewColumn =
    (vcName, vcFileType, vcLastModified, vcSize, vcRatio,
     vcPacked, vcCRC, vcAttributes, vcEncrypted, vcMethod, vcPath);
  TAbViewColumns = set of TAbViewColumn;

{ ===== TAbCustomListView =================================================== }
  TAbCustomListView = class(TCustomListView)
  protected {private}
    FArchive : TAbBaseBrowser;
    FFlatList: Boolean;
    FHeaderImages : TImageList;
    FPath : string;
    FTreeView : TAbCustomTreeView;
    FVisibleColumns : TAbViewColumns;

  protected {methods}
    procedure CreateWnd;
      override;
    procedure DblClick;
      override;
    procedure DoChange(Sender : TObject);
      virtual;
    procedure Notification(aComponent : TComponent; aOperation : TOperation);
      override;
    procedure SetArchive(aValue : TAbBaseBrowser);
    procedure SetFlatList(aValue : Boolean);
    procedure SetPath(aValue : string);
    procedure SetVisibleColumns(aValue : TAbViewColumns);
    procedure UpdateView;

  protected {properties}
    property HeaderImages : TImageList
      read FHeaderImages;
    property TreeView : TAbCustomTreeView
      read FTreeView
      write FTreeView;

  public {methods}
    constructor Create(aOwner: TComponent);
      override;
    destructor Destroy;
      override;

  public {properties}
    property Archive : TAbBaseBrowser
      read FArchive
      write SetArchive;
    // Show only items in the current path
    property FlatList : Boolean
      read FFlatList
      write SetFlatList;
    property Path : string
      read FPath
      write SetPath;
    property VisibleColumns : TAbViewColumns
      read FVisibleColumns
      write SetVisibleColumns;
  end;


{ ===== TAbListView ========================================================= }
  TAbListView = class(TAbCustomListView)
  published
    property Action;
    property Align;
    property AllocBy;
    property Anchors;
    property Archive;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind default bkNone;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property Checkboxes;
    property Color;
    property Columns;
    property ColumnClick;
    property Constraints;
    property Ctl3D;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FlatScrollBars;
    property FullDrag;
    property GridLines;
    property Groups;
    property HideSelection;
    property HotTrack;
    property HotTrackStyles;
    property HoverTime;
    property IconOptions;
    property Items;
    property LargeImages;
    property MultiSelect;
    property GroupHeaderImages;
    property GroupView default False;
    property ReadOnly default False;
    property RowSelect;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property Path;
    property PopupMenu;
    property ShowColumnHeaders;
    property ShowWorkAreas;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property ViewStyle;
    property Visible;
    property VisibleColumns;
    property OnClick;
    property OnColumnClick;
    property OnColumnDragged;
    property OnColumnRightClick;
    property OnContextPopup;
    property OnDblClick;
    property OnEdited;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnDragDrop;
    property OnDragOver;
    property OnInfoTip;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnSelectItem;
    property OnItemChecked;
    property OnStartDock;
    property OnStartDrag;
  end;


{ ===== TAbCustomTreeView =================================================== }
  TAbCustomTreeView = class(TTreeView)
  protected {private}
    FArchive: TAbBaseBrowser;
    FListView: TAbCustomListView;
    FPath: string;

  protected {methods}
    procedure Change(aNode: TTreeNode);
      override;
    procedure DoChange(Sender : TObject);
      virtual;
    procedure GetSelectedIndex(aNode: TTreeNode);
      override;
    procedure Notification(aComponent : TComponent; aOperation : TOperation);
      override;
    procedure SetArchive(aValue: TAbBaseBrowser);
    procedure SetListView(aValue: TAbCustomListView);
    procedure SetPath(const aValue: string);

  public {methods}
    constructor Create(aOwner: TComponent);
      override;

  public {properties}
    property Archive: TAbBaseBrowser
      read FArchive
      write SetArchive;
    property HideSelection
      default False;
    property ListView: TAbCustomListView
      read FListView
      write SetListView;
    property Path: string
      read FPath
      write SetPath;
  end;


{ ===== TAbTreeView ========================================================= }
  TAbTreeView = class(TAbCustomTreeView)
  published
    property Align;
    property Anchors;
    property Archive;
    property AutoExpand;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind default bkNone;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property ChangeDelay;
    property Color;
    property Ctl3D;
    property Constraints;
    property DoubleBuffered;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property HotTrack;
    property Indent;
    property Items;
    property ListView;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property Path;
    property PopupMenu;
    property ReadOnly;
    property RightClickSelect;
    property RowSelect;
    property ShowButtons;
    property ShowHint;
    property ShowLines;
    property ShowRoot;
    property TabOrder;
    property TabStop default True;
    property ToolTips;
    property Visible;
    property OnChanging;
    property OnClick;
    property OnCollapsed;
    property OnCollapsing;
    property OnContextPopup;
    property OnDblClick;
    property OnDeletion;
    property OnDragDrop;
    property OnDragOver;
    property OnEdited;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnExpanding;
    property OnExpanded;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
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
function SameEvent(const aEvent1, aEvent2: TNotifyEvent): Boolean;
begin
  Result := (TMethod(aEvent1).Code = TMethod(aEvent2).Code) and
    (TMethod(aEvent1).Data = TMethod(aEvent2).Data);
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

{ ===== TAbCustomListView =================================================== }
constructor TAbCustomListView.Create(aOwner: TComponent);
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
    Bmp.LoadFromResourceName(HInstance, 'AbComCtrls_Lock');
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

  VisibleColumns := [vcName, vcFileType, vcLastModified, vcSize, vcRatio,
     vcPacked, vcCRC, vcAttributes, vcEncrypted, vcMethod, vcPath];
end;
{ -------------------------------------------------------------------------- }
destructor TAbCustomListView.Destroy;
begin

  inherited;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomListView.CreateWnd;
begin
  inherited;
  Header_SetImageList(ListView_GetHeader(Handle), FHeaderImages.Handle);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomListView.DblClick;
begin
  inherited;
  if TAbArchiveItem(Selected.Data).IsDirectory then begin
    Path := Path + PathDelim + Selected.Caption;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomListView.DoChange(Sender: TObject);
begin
  UpdateView;
  if (Sender = FArchive) and Assigned(FTreeView) then
    FTreeView.DoChange(Self);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomListView.Notification(aComponent: TComponent;
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
procedure TAbCustomListView.SetArchive(aValue: TAbBaseBrowser);
begin
  if Assigned(FArchive) then begin
    FArchive.RemoveFreeNotification(Self);
    if SameEvent(FArchive.OnChange, DoChange) then
      if Assigned(TreeView) then
        FArchive.OnChange := TreeView.DoChange
      else
        FArchive.OnChange := nil;
  end;
  FArchive := aValue;
  if Assigned(FArchive) then begin
    FArchive.FreeNotification(Self);
    FArchive.OnChange := DoChange;
    DoChange(Self);
  end;
  UpdateView;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomListView.SetFlatList(aValue : Boolean);
begin
  if aValue <> FFlatList then begin
    FFlatList := aValue;
    UpdateView;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomListView.SetPath(aValue: string);
begin
  if aValue <> FPath then begin
    FPath := aValue;
    if Assigned(TreeView) then
      TreeView.Path := aValue;
    if not FlatList then
      UpdateView;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomListView.SetVisibleColumns(aValue : TAbViewColumns);
const
  ColWidths: array[TAbViewColumn] of Integer = (
    180{vcName}, 110{vcFileType}, 130{vcLastModified}, 80{vcSize}, 50{vcRatio},
    80{vcPacked}, 70{vcCRC}, 30{vcAttributes}, 28{vcEncrypted}, 60{vcMethod},
    300{vcPath});
var
  Col: TAbViewColumn;
  Column: TListColumn;
begin
  if aValue <> FVisibleColumns then begin
    Columns.Clear;
    FVisibleColumns := aValue;
    for Col := Low(Col) to High(Col) do begin
      if not (Col in FVisibleColumns) then
        Continue;
      Column := Columns.Add;
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
    UpdateView;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomListView.UpdateView;
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
  if (Items.Count = 0) and (FArchive = nil) then
    Exit;
  Items.BeginUpdate;
  try
    Items.Clear;
    if Assigned(FArchive) then begin
      for i := 0 to FArchive.Count - 1 do
        if FArchive[i].Action <> aaDelete then begin
          CurItem := FArchive[i];
          Filename := AbNormalizeFilename(CurItem.FileName);
          // Exclude unwanted items
          if FlatList then begin
            if CurItem.IsDirectory then
              Continue;
          end
          else if Path <> ExtractFileDir(Filename) then
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
            if Col in FVisibleColumns then begin
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
                    ColText := ZipCompressionMethodToString(
                      TAbZipItem(CurItem).CompressionMethod);
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


{ ===== TAbCustomTreeView =================================================== }
constructor TAbCustomTreeView.Create(aOwner: TComponent);
var
  Bmp : TBitmap;
  Icon : TIcon;
  sfi: SHFILEINFO;
begin
  inherited;
  HideSelection := False;
  Images := TImageList.Create(Self);
  Bmp := TBitmap.Create;
  try
    Bmp.LoadFromResourceName(HInstance, 'AbComCtrls_Zip');
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
procedure TAbCustomTreeView.Change(aNode: TTreeNode);
var
  Filename: string;
begin
  inherited;
  if aNode.Selected then begin
    Filename := '';
    if aNode <> Items.GetFirstNode then begin
      Filename := aNode.Text;
      aNode := aNode.Parent;
      while aNode <> Items.GetFirstNode do begin
        Filename := aNode.Text + PathDelim + Filename;
        aNode := aNode.Parent;
      end;
    end;
    Path := Filename;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomTreeView.DoChange(Sender: TObject);
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
      ZipNode.Expand(False);
    end;
  finally
    Items.EndUpdate;
  end;
  if (Sender = FArchive) and Assigned(FListView) then
    FListView.DoChange(Self);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomTreeView.GetSelectedIndex(aNode: TTreeNode);
begin
  if aNode.Expanded then
    aNode.SelectedIndex := aNode.ExpandedImageIndex
  else
    aNode.SelectedIndex := aNode.ImageIndex;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomTreeView.Notification(aComponent: TComponent;
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
procedure TAbCustomTreeView.SetArchive(aValue: TAbBaseBrowser);
begin
  if Assigned(FArchive) then begin
    FArchive.RemoveFreeNotification(Self);
    if SameEvent(FArchive.OnChange, DoChange) then
      if Assigned(ListView) then
        FArchive.OnChange := ListView.DoChange
      else
        FArchive.OnChange := nil;
  end;
  FArchive := aValue;
  if Assigned(FArchive) then begin
    FArchive.FreeNotification(Self);
    FArchive.OnChange := DoChange;
    DoChange(Self);
  end
  else
    Items.Clear;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomTreeView.SetListView(aValue: TAbCustomListView);
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
{ -------------------------------------------------------------------------- }
procedure TAbCustomTreeView.SetPath(const aValue: string);
var
  Filename, Remaining: string;
  i: Integer;
  Node: TTreeNode;
begin
  if FPath <> aValue then begin
    FPath := aValue;
    // Find selected node, expanding parents along the way
    Node := Items.GetFirstNode;
    Remaining := FPath;
    if StartsText(PathDelim, Remaining) then
      System.Delete(Remaining, 1, 1);
    while Remaining <> '' do begin
      Node.Expand(False);
      i := Pos(PathDelim, Remaining);
      if i = 0 then
        i := Length(Remaining) + 1;
      Filename := Copy(Remaining, 1, i - 1);
      Remaining := Copy(Remaining, i + 1, MaxInt);
      if Filename = '' then
        Continue;
      Node := Node.getFirstChild;
      while (Node <> nil) and not SameText(Filename, Node.Text) do
        Node := Node.getNextSibling;
      if Node = nil then begin
        Node := Items.GetFirstNode;
        Break;
      end;
    end;
    Selected := Node;
    // Update listview
    if Assigned(FListView) then
      FListView.Path := aValue;
  end;
end;

end.
