unit ce_miniexplorer;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, ListViewFilterEdit, Forms, strutils ,
  Controls, Graphics, ExtCtrls, Menus, ComCtrls, Buttons, lcltype, dialogs,
  ce_widget, ce_sharedres, ce_common, ce_interfaces, ce_observer,
  ce_writableComponent, ce_dubproject, ce_ceproject, EditBtn, ShellCtrls,
  ce_dialogs, ce_synmemo, ce_projutils, ce_dsgncontrols, ce_stringrange, Types;

type

  TExplorerDoubleClick = (openInside, openOutside);

  TCEMiniExplorerWidget = class;

  TCEMiniExplorerEditableOptions = class(TPersistent, ICEEditableOptions)
  private
    fDblClick: TExplorerDoubleClick;
    fContextExpand: boolean;
    fShowHidden: boolean;
    fExplorer: TCEMiniExplorerWidget;
    function optionedWantCategory(): string;
    function optionedWantEditorKind: TOptionEditorKind;
    function optionedWantContainer: TPersistent;
    procedure optionedEvent(event: TOptionEditorEvent);
    function optionedOptionsModified: boolean;
    procedure apply;
  published
    property doubleClick: TExplorerDoubleClick read fDblClick write fDblClick;
    property contextExpand: boolean read fContextExpand write fContextExpand;
    property showHidden: boolean read fShowHidden write fShowHidden default true;
  public
    constructor create(miniexpl: TCEMiniExplorerWidget);
    destructor destroy; override;
  end;

  TCEMiniExplorerOptions = class(TWritableLfmTextComponent)
  private
    fFavoriteFolders: TStringList;
    fSplitter1Position: integer;
    fSplitter2Position: integer;
    fLastFolder: string;
    fDblClick: TExplorerDoubleClick;
    fContextExpand: boolean;
    fShowHidden: boolean;
    procedure setFavoriteFolders(value: TStringList);
  published
    property splitter1Position: integer read fSplitter1Position write fSplitter1Position;
    property splitter2Position: integer read fSplitter2Position write fSplitter2Position;
    property lastFolder: string read fLastFolder write fLastFolder;
    property favoriteFolders: TStringList read fFavoriteFolders write setFavoriteFolders;
    property doubleClick: TExplorerDoubleClick read fDblClick write fDblClick;
    property contextExpand: boolean read fContextExpand write fContextExpand;
    property showHidden: boolean read fShowHidden write fShowHidden default true;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    procedure assign(source: TPersistent); override;
    procedure assignTo(target: TPersistent); override;
  end;

  { TCEMiniExplorerWidget }

  TCEMiniExplorerWidget = class(TCEWidget, ICEProjectObserver, ICEDocumentObserver, ICEExplorer)
    btnAddFav: TCEToolButton;
    btnDrive: TCEToolButton;
    btnEdit: TCEToolButton;
    btnParentFolder: TCEToolButton;
    btnRemFav: TCEToolButton;
    btnShellOpen: TCEToolButton;
    lstFilter: TListViewFilterEdit;
    lstFav: TListView;
    Panel2: TPanel;
    lstFiles: TShellListView;
    mnuDrives: TPopupMenu;
    treeFolders: TShellTreeView;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure btnDriveClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnParentFolderClick(Sender: TObject);
    procedure btnShellOpenClick(Sender: TObject);
    procedure btnAddFavClick(Sender: TObject);
    procedure btnRemFavClick(Sender: TObject);
    procedure lstFavClick(Sender: TObject);
    procedure lstFavDeletion(Sender: TObject; Item: TListItem);
    procedure lstFavEnter(Sender: TObject);
    procedure lstFilesColumnClick(Sender: TObject; Column: TListColumn);
    procedure lstFilesDblClick(Sender: TObject);
    procedure lstFilesEnter(Sender: TObject);
    procedure lstFilesFileAdded(Sender: TObject; Item: TListItem);
    procedure lstFilterButtonClick(Sender: TObject);
    procedure lstFilterKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Splitter2MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure toolbarResize(Sender: TObject);
    procedure TreeEnter(Sender: TObject);
    procedure treeFoldersChange(Sender: TObject; Node: TTreeNode);
    procedure treeFoldersDblClick(Sender: TObject);
    procedure treeFoldersGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure treeFoldersGetSelectedIndex(Sender: TObject; Node: TTreeNode);
  private
    fMnxSubj: TCEMiniExplorerSubject;
    fProj: ICECommonProject;
    fFreeProj: ICECommonProject;
    fFavorites: TStringList;
    fLastFold: string;
    fLastListOrTree: TControl;
    fDblClick: TExplorerDoubleClick;
    fContextExpand: boolean;
    fEditableOptions: TCEMiniExplorerEditableOptions;
    fImages: TImageList;
    fFileListSortedColumnIndex: integer;
    fFileListSortDirection: TSortDirection;
    procedure filterFiles;
    procedure lstFavDblClick(Sender: TObject);
    procedure updateFavorites;
    procedure treeSetRoots;
    procedure favStringsChange(sender: TObject);
    procedure lstFavSelect(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure shellOpenSelected;
    procedure mnuDriveItemClick(sender: TObject);
    procedure mnuDriveSelect(sender: TObject);
    procedure compareFileList(Sender: TObject; Item1, Item2: TListItem; Data: Integer;
      var Compare: Integer);

    procedure projNew(project: ICECommonProject);
    procedure projChanged(project: ICECommonProject);
    procedure projClosing(project: ICECommonProject);
    procedure projFocused(project: ICECommonProject);
    procedure projCompiling(project: ICECommonProject);
    procedure projCompiled(project: ICECommonProject; success: boolean);

    procedure docNew(document: TCESynMemo);
    procedure docFocused(document: TCESynMemo);
    procedure docChanged(document: TCESynMemo);
    procedure docClosing(document: TCESynMemo);

    function singleServiceName: string;
    procedure browse(const location: string);
    function currentLocation: string;
  protected
    procedure setToolBarFlat(value: boolean); override;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
  end;

implementation
{$R *.lfm}

const
  OptsFname = 'miniexplorer.txt';


{$REGION TCEMiniExplorerEditableOptions}
constructor TCEMiniExplorerEditableOptions.create(miniexpl: TCEMiniExplorerWidget);
begin
  fExplorer := miniexpl;
  fShowHidden:=true;
  EntitiesConnector.addObserver(self);
end;

destructor TCEMiniExplorerEditableOptions.destroy;
begin
  EntitiesConnector.removeObserver(self);
  inherited;
end;

procedure TCEMiniExplorerEditableOptions.apply;
begin
  fExplorer.fContextExpand:= fContextExpand;
  fExplorer.fDblClick:= fDblClick;
  if fShowHidden then
  begin
    fExplorer.treeFolders.ObjectTypes := fExplorer.treeFolders.ObjectTypes + [otHidden];
    fExplorer.lstFiles.ObjectTypes := fExplorer.lstFiles.ObjectTypes + [otHidden];
  end
  else
  begin
    fExplorer.treeFolders.ObjectTypes := fExplorer.treeFolders.ObjectTypes - [otHidden];
    fExplorer.lstFiles.ObjectTypes := fExplorer.lstFiles.ObjectTypes - [otHidden];
  end;
  fExplorer.treeFolders.Refresh;
end;

function TCEMiniExplorerEditableOptions.optionedWantCategory(): string;
begin
  exit('Mini explorer');
end;

function TCEMiniExplorerEditableOptions.optionedWantEditorKind: TOptionEditorKind;
begin
  exit(oekGeneric);
end;

function TCEMiniExplorerEditableOptions.optionedWantContainer: TPersistent;
begin
  exit(self);
end;

procedure TCEMiniExplorerEditableOptions.optionedEvent(event: TOptionEditorEvent);
begin
  apply;
end;

function TCEMiniExplorerEditableOptions.optionedOptionsModified: boolean;
begin
  exit(false);
end;
{$ENDREGION}

{$REGION TCEMiniExplorerOptions ------------------------------------------------}
constructor TCEMiniExplorerOptions.create(aOwner: TComponent);
begin
  inherited;
  fFavoriteFolders := TStringList.Create;
  fShowHidden:=true;
end;

destructor TCEMiniExplorerOptions.destroy;
begin
  fFavoriteFolders.Free;
  inherited;
end;

procedure TCEMiniExplorerOptions.assign(source: TPersistent);
var
  widg: TCEMiniExplorerWidget;
begin
  if source is TCEMiniExplorerWidget then
  begin
    widg := TCEMiniExplorerWidget(source);
    fFavoriteFolders.Assign(widg.fFavorites);
    fLastFolder := widg.fLastFold;
    fSplitter1Position := widg.Splitter1.GetSplitterPosition;
    fSplitter2Position := widg.Splitter2.GetSplitterPosition;
    fDblClick:= widg.fDblClick;
    fContextExpand:=widg.fContextExpand;
    fShowHidden:= otHidden in widg.lstFiles.ObjectTypes;
  end
  else inherited;
end;

procedure TCEMiniExplorerOptions.assignTo(target: TPersistent);
var
  widg: TCEMiniExplorerWidget;
begin
  if target is TCEMiniExplorerWidget then
  begin
    widg := TCEMiniExplorerWidget(target);
    widg.fFavorites.Assign(fFavoriteFolders);
    widg.fLastFold:=fLastFolder;
    widg.Splitter1.SetSplitterPosition(fSplitter1Position);
    widg.Splitter2.SetSplitterPosition(fSplitter2Position);
    widg.fDblClick := fDblClick;
    widg.fEditableOptions.fDblClick := fDblClick;
    widg.fContextExpand := fContextExpand;
    widg.fEditableOptions.fContextExpand := fContextExpand;
    widg.updateFavorites;
    if fShowHidden then
    begin
      widg.treeFolders.ObjectTypes := widg.treeFolders.ObjectTypes + [otHidden];
      widg.lstFiles.ObjectTypes := widg.lstFiles.ObjectTypes + [otHidden];
    end
    else
    begin
      widg.treeFolders.ObjectTypes := widg.treeFolders.ObjectTypes - [otHidden];
      widg.lstFiles.ObjectTypes := widg.lstFiles.ObjectTypes -[otHidden];
    end;
   if widg.fLastFold.dirExists then
      widg.browse(fLastFolder);
  end
  else inherited;
end;

procedure TCEMiniExplorerOptions.setFavoriteFolders(value: TStringList);
begin
  fFavoriteFolders.Assign(value);
end;
{$ENDREGION}

{$REGION Standard Comp/Obj------------------------------------------------------}
constructor TCEMiniExplorerWidget.create(aOwner: TComponent);
var
  fname: string;
begin
  inherited;

  lstFiles.OnCompare := @compareFileList;
  fFileListSortDirection := sdAscending;
  fFileListSortedColumnIndex:=-1;

  fImages := TImageList.Create(self);
  case GetIconScaledSize of
    iss16:
    begin
      fImages.Width := 16;
      fImages.Height := 16;
      treeFolders.Indent := 16;
      fImages.AddResourceName(HINSTANCE, 'DOCUMENT');
      fImages.AddResourceName(HINSTANCE, 'FOLDER');
      fImages.AddResourceName(HINSTANCE, 'FOLDER_STAR');
      fImages.AddResourceName(HINSTANCE, 'FOLDER_ADD');
      fImages.AddResourceName(HINSTANCE, 'FOLDER_DELETE');
      AssignPng(lstFilter.Glyph, 'FILTER_CLEAR');
    end;
    iss24:
    begin
      fImages.Width := 24;
      fImages.Height := 24;
      treeFolders.Indent := 24;
      fImages.AddResourceName(HINSTANCE, 'DOCUMENT24');
      fImages.AddResourceName(HINSTANCE, 'FOLDER24');
      fImages.AddResourceName(HINSTANCE, 'FOLDER_STAR24');
      fImages.AddResourceName(HINSTANCE, 'FOLDER_ADD24');
      fImages.AddResourceName(HINSTANCE, 'FOLDER_DELETE24');
      AssignPng(lstFilter.Glyph, 'FILTER_CLEAR24');
    end;
    iss32:
    begin
      fImages.Width := 32;
      fImages.Height := 32;
      treeFolders.Indent := 32;
      fImages.AddResourceName(HINSTANCE, 'DOCUMENT32');
      fImages.AddResourceName(HINSTANCE, 'FOLDER32');
      fImages.AddResourceName(HINSTANCE, 'FOLDER_STAR32');
      fImages.AddResourceName(HINSTANCE, 'FOLDER_ADD32');
      fImages.AddResourceName(HINSTANCE, 'FOLDER_DELETE32');
      AssignPng(lstFilter.Glyph, 'FILTER_CLEAR32');
    end;
  end;
  lstFav.SmallImages := fImages;

  treeFolders.Images := fImages;
  treeFolders.StateImages := fImages;

  lstFiles.SmallImages := fImages;
  {$IFNDEF WINDOWS}
  lstFiles.StateImages := fImages;
  {$ENDIF}
  lstFiles.OnEnter:=@lstFilesEnter;

  fEditableOptions:= TCEMiniExplorerEditableOptions.create(self);

  fFavorites := TStringList.Create;
  fFavorites.onChange := @favStringsChange;
  lstFav.OnSelectItem := @lstFavSelect;
  lstFav.OnDblClick := @lstFavDblClick;

  lstFilter.BorderSpacing.Left := ScaleX(182, 96);

  treeSetRoots;

  fname := getCoeditDocPath + OptsFname;
  if fname.fileExists then
    with TCEMiniExplorerOptions.create(nil) do
  try
    loadFromFile(fname);
    assignTo(self);
  finally
    free;
  end;

  fMnxSubj:= TCEMiniExplorerSubject.Create;
  EntitiesConnector.addObserver(self);
  EntitiesConnector.addSingleService(self);
end;

destructor TCEMiniExplorerWidget.destroy;
begin
  fMnxSubj.free;
  EntitiesConnector.removeObserver(self);
  with TCEMiniExplorerOptions.create(nil) do
  try
    assign(self);
    saveToFile(getCoeditDocPath + OptsFname);
  finally
    free;
  end;

  fEditableOptions.Free;
  fFavorites.Free;
  inherited;
end;

procedure TCEMiniExplorerWidget.setToolBarFlat(value: boolean);
begin
  inherited setToolBarFlat(value);
  lstFilter.Flat:=value;
end;
{$ENDREGION}

{$REGION ICEProjectObserver ----------------------------------------------------}
procedure TCEMiniExplorerWidget.projNew(project: ICECommonProject);
begin
  fProj := project;
  if not project.inGroup then
    fFreeProj := project;
end;

procedure TCEMiniExplorerWidget.projChanged(project: ICECommonProject);
begin
end;

procedure TCEMiniExplorerWidget.projClosing(project: ICECommonProject);
begin
  fProj := nil;
  if project = fFreeProj then
    fFreeProj := nil;
end;

procedure TCEMiniExplorerWidget.projFocused(project: ICECommonProject);
begin
  fProj := project;
  if not project.inGroup then
    fFreeProj := project
  else if fFreeProj = project then
    fFreeProj := nil;
  if visible and project.fileName.fileExists and fContextExpand then
    browse(project.fileName);
end;

procedure TCEMiniExplorerWidget.projCompiling(project: ICECommonProject);
begin
end;

procedure TCEMiniExplorerWidget.projCompiled(project: ICECommonProject; success: boolean);
begin
end;
{$ENDREGION}

{$REGION ICEDocumentObserver ---------------------------------------------------}
procedure TCEMiniExplorerWidget.docNew(document: TCESynMemo);
begin
end;

procedure TCEMiniExplorerWidget.docFocused(document: TCESynMemo);
begin
  if visible and document.fileName.fileExists and fContextExpand then
    browse(document.fileName);
end;

procedure TCEMiniExplorerWidget.docChanged(document: TCESynMemo);
begin
end;

procedure TCEMiniExplorerWidget.docClosing(document: TCESynMemo);
begin
end;
{$ENDREGION}

{$REGION Favorites -------------------------------------------------------------}
procedure TCEMiniExplorerWidget.favStringsChange(sender: TObject);
begin
  updateFavorites;
end;

procedure TCEMiniExplorerWidget.updateFavorites;
var
  itm: TListItem;
  fold: string;
  dat: PString;
begin
  lstFav.Clear;
  for fold in fFavorites do
  begin
    itm := lstFav.Items.Add;
    itm.Caption := shortenPath(fold);
    dat := NewStr(fold);
    itm.Data := dat;
    itm.ImageIndex := 2;
  end;
end;

procedure TCEMiniExplorerWidget.lstFavSelect(Sender: TObject; Item: TListItem; Selected: Boolean);
var
  d: string;
begin
  if not Selected or Item.Data.isNil then
     exit;
  d := PString(Item.Data)^;
  if d.dirExists then
    browse(d)
end;

procedure TCEMiniExplorerWidget.btnRemFavClick(Sender: TObject);
var
  i: Integer;
begin
  if lstFav.Selected.isNil then
     exit;
  i := fFavorites.IndexOf(PString(lstFav.Selected.Data)^);
  if i <> -1 then
     fFavorites.Delete(i);
  lstFiles.Clear;
end;

procedure TCEMiniExplorerWidget.lstFavClick(Sender: TObject);
var
  d: string;
begin
  if lstFav.Selected.isNil or lstFav.Selected.Data.isNil then
     exit;
  d := PString(lstFav.Selected.Data)^;
  if not d.dirExists and (dlgYesNo('The favorite folder `' + d +
    '` does not exist. ' + 'Remove from the list ?') = mrYes) then
  begin
    fFavorites.Delete(lstFav.Selected.Index);
    lstFiles.Clear;
  end;
end;

procedure TCEMiniExplorerWidget.lstFavDeletion(Sender: TObject; Item: TListItem);
begin
  if Item.isNotNil and item.Data.isNotNil then
    dispose(PString(item.Data));
end;

procedure TCEMiniExplorerWidget.lstFavEnter(Sender: TObject);
begin
  fLastListOrTree := lstFav;
end;

procedure TCEMiniExplorerWidget.lstFilesColumnClick(Sender: TObject;Column: TListColumn);
begin
  if Column.isNotNil then
  begin
    if Column.Index = fFileListSortedColumnIndex then
    begin
      if fFileListSortDirection = sdAscending then
        fFileListSortDirection := sdDescending
      else
        fFileListSortDirection := sdAscending;
    end;
    fFileListSortedColumnIndex := Column.Index;
  end;
end;

procedure TCEMiniExplorerWidget.btnAddFavClick(Sender: TObject);
begin
  if treeFolders.Selected.isNil then
    exit;
  fFavorites.Add(treeFolders.GetPathFromNode(treeFolders.Selected).extractFileDir);
end;

procedure TCEMiniExplorerWidget.lstFavDblClick(Sender: TObject);
begin
  if lstFav.Selected.isNil then
     exit;
  treeFolders.Root := lstFav.Selected.Caption;
end;

procedure TCEMiniExplorerWidget.filterFiles;
var
  s: string;
  i: integer;
begin

  // getting the full list is not possible once no item anymore
  // e.g after filtering failed
  treeFolders.BeginUpdate;
  s := treeFolders.Root;
  treeFolders.Root:= '';
  treeFolders.Root:= s;
  treeFolders.EndUpdate;

  if lstFilter.filter.isEmpty then
    exit;

  lstFiles.BeginUpdate;
  for i:= lstFiles.Items.Count-1 downto 0 do
    if not AnsicontainsText(lstfiles.Items[i].Caption,lstFilter.Filter) then
      lstfiles.Items.Delete(i);
  lstFiles.EndUpdate;
end;

procedure TCEMiniExplorerWidget.compareFileList(Sender: TObject; Item1, Item2: TListItem;
  Data: Integer; var Compare: Integer);
var
  s1, s2: integer;
  u1, u2: string;
  r1: TStringRange = (ptr:nil; pos:0; len: 0);
  r2: TStringRange = (ptr:nil; pos:0; len: 0);
begin
  case fFileListSortedColumnIndex of
    0:
    begin
      if fFileListSortDirection = sdAscending then
        Compare := CompareStr(Item1.Caption, Item2.Caption)
      else
        Compare := CompareStr(Item2.Caption, Item1.Caption);
    end;
    2:
    begin
      if fFileListSortDirection = sdAscending then
        Compare := CompareStr(Item1.SubItems[1], Item2.SubItems[1])
      else
        Compare := CompareStr(Item2.SubItems[1], Item1.SubItems[1])
    end;
    1:
    begin
      if fFileListSortDirection = sdAscending then
      begin
        r1.init(Item1.SubItems[0]);
        r2.init(Item2.SubItems[0]);
      end
      else
      begin
        r1.init(Item2.SubItems[0]);
        r2.init(Item1.SubItems[0]);
      end;

      s1 := r1.takeUntil(' ').yield.toIntNoExcept();
      u1 := r1.popFront^.takeUntil(#0).yield;

      s2 := r2.takeUntil(' ').yield.toIntNoExcept();
      u2 := r2.popFront^.takeUntil(#0).yield;

      if u1 = u2 then
        Compare := s1 - s2
      else if u1 = 'bytes' then
        Compare := -1
      else if u1 = 'kB' then
      begin
        if u2 = 'bytes' then
          Compare := 1
        else
          Compare := -1;
      end
      else if u1 = 'MB' then
      begin
        if (u2 = 'bytes') or (u2 = 'kB') then
          Compare := 1
        else
          Compare := -1;
      end
      else if u1 = 'GB' then
      begin
        if (u2 = 'bytes') or (u2 = 'kB') or (u2 = 'MB') then
          Compare := 1
        else
          Compare := -1;
      end
      else if u1 = 'TB' then
      begin
        if u2 <> 'PB' then
          Compare := 1
        else
          Compare := -1;
      end;
    end;
  end;
end;
{$ENDREGION}

{$REGION Files -----------------------------------------------------------------}
procedure TCEMiniExplorerWidget.btnShellOpenClick(Sender: TObject);
begin
  shellOpenSelected;
end;

procedure TCEMiniExplorerWidget.btnEditClick(Sender: TObject);
var
  fname: string;
  fmt: TCEProjectFileFormat;
begin
  if lstFiles.Selected.isNil then
    exit;
  fname := lstFiles.GetPathFromItem(lstFiles.Selected);
  if not fname.fileExists then
    exit;
  fmt := projectFormat(fname);
  if fmt in [pffCe, pffDub] then
  begin
    if assigned(fFreeProj) then
    begin
      if fFreeProj.modified and (dlgFileChangeClose(fFreeProj.filename, UnsavedProj) = mrCancel) then
        exit;
      fFreeProj.getProject.Free;
    end;
    if fmt = pffCe then
      TCENativeProject.create(nil)
    else
      TCEDubProject.create(nil);
    fProj.loadFromFile(fname);
    fProj.activate;
  end
  else getMultiDocHandler.openDocument(fname);
end;

procedure TCEMiniExplorerWidget.btnDriveClick(Sender: TObject);
begin
  mnuDriveSelect(nil);
end;

procedure TCEMiniExplorerWidget.btnParentFolderClick(Sender: TObject);
var
  p: string;
begin
  p := treeFolders.Root.extractFileDir;
  if p.dirExists then
    treeFolders.Root := p;
end;

procedure TCEMiniExplorerWidget.lstFilesDblClick(Sender: TObject);
begin
  case fDblClick of
    openInside: btnEditClick(nil);
    openOutside: shellOpenSelected;
  end;
end;

procedure TCEMiniExplorerWidget.lstFilesEnter(Sender: TObject);
begin
  fLastListOrTree := lstFiles;
end;

procedure TCEMiniExplorerWidget.lstFilesFileAdded(Sender: TObject;
  Item: TListItem);
begin
  Item.ImageIndex:=0;
  lstFiles.Sort;
end;

procedure TCEMiniExplorerWidget.lstFilterButtonClick(Sender: TObject);
var
  s: string;
begin
  s := treeFolders.Root;
  treeFolders.Root:= '';
  treeFolders.Root:= s;
end;

procedure TCEMiniExplorerWidget.lstFilterKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  filterFiles;
end;

procedure TCEMiniExplorerWidget.Splitter2MouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
var
  offs: integer;
  splt: TSplitter;
begin
  offs := -240 * 8 div WheelDelta;
  splt := TSplitter(sender);
  splt.MoveSplitter(offs);
  if splt.ResizeAnchor in [akLeft, akRight] then
    Mouse.CursorPos:= classes.Point(Mouse.CursorPos.X + offs, Mouse.CursorPos.Y)
  else
    Mouse.CursorPos:= classes.Point(Mouse.CursorPos.X, Mouse.CursorPos.Y + offs);
  Handled := true;
end;

procedure TCEMiniExplorerWidget.toolbarResize(Sender: TObject);
begin
  lstFilter.Width := toolbar.Width - lstFilter.Left - lstFilter.BorderSpacing.Around;
end;

procedure TCEMiniExplorerWidget.shellOpenSelected;
var
  fname: string = '';
begin
  if fLastListOrTree = lstFiles then
  begin
    if lstFiles.Selected.isNil then
       exit;
    fname := lstFiles.GetPathFromItem(lstFiles.Selected);
  end else if fLastListOrTree = treeFolders then
  begin
    if treeFolders.Selected.isNil then
       exit;
    fname := treeFolders.GetPathFromNode(treeFolders.Selected).extractFileDir;
  end
  else if fLastListOrTree = lstFav then
  begin
    if lstFav.Selected.isNil or lstFav.Selected.Data.isNil then
       exit;
    fname := PString(lstFav.Selected.Data)^;
  end;
  if (fname.fileExists or fname.dirExists) and not shellOpen(fname) then
    getMessageDisplay.message((format('the shell failed to open "%s"',
    [shortenPath(fname, 25)])), nil, amcMisc, amkErr);
end;
{$ENDREGION}

{$REGION Tree ------------------------------------------------------------------}
procedure TCEMiniExplorerWidget.TreeEnter(Sender: TObject);
begin
  fLastListOrTree := treeFolders;
end;

procedure TCEMiniExplorerWidget.treeFoldersChange(Sender: TObject;
  Node: TTreeNode);
begin
  if treeFolders.Selected.isNil then
     exit;
  fLastFold := treeFolders.Path.extractFileDir; // trailing path sep
  subjMnexDirectoryChanged(fMnxSubj, fLastFold);
end;

procedure TCEMiniExplorerWidget.treeFoldersDblClick(Sender: TObject);
begin
  if treeFolders.Selected.isNil then
     exit;
  treeFolders.Root := treeFolders.GetPathFromNode(treeFolders.Selected)
    .extractFileDir; // trailing path sep
end;

procedure TCEMiniExplorerWidget.treeFoldersGetImageIndex(Sender: TObject;
  Node: TTreeNode);
begin
  Node.ImageIndex:=1;
  Node.SelectedIndex:=1;
end;

procedure TCEMiniExplorerWidget.treeFoldersGetSelectedIndex(Sender: TObject;
  Node: TTreeNode);
begin
  Node.ImageIndex:=1;
  Node.SelectedIndex:=1;
end;

procedure TCEMiniExplorerWidget.treeSetRoots;
var
  m: TMenuItem;
  d: TStringList;
  i: integer;
begin
  d := TStringList.Create;
  try
    listDrives(d);
    for i := 0 to d.Count-1 do
    begin
      m := TMenuItem.Create(self);
      m.Caption := d[i];
      m.OnClick := @mnuDriveItemClick;
      mnuDrives.Items.Add(m);
      if i = 0 then
        treeFolders.Root:= m.Caption;
    end;
    m := Tmenuitem.Create(self);
    m.Caption:= 'Select a custom location...';
    m.OnClick:=@mnuDriveSelect;
    mnuDrives.Items.Add(m);
  finally
    d.Free;
  end;
end;

procedure TCEMiniExplorerWidget.mnuDriveItemClick(sender: TObject);
begin
  treeFolders.Root := TMenuItem(sender).Caption;
end;

procedure TCEMiniExplorerWidget.mnuDriveSelect(sender: TObject);
var
  d: string;
begin
  if SelectDirectory('Select the new tree root', '', d) then
    treeFolders.Root:=d;
end;
{$ENDREGION}

{$REGION ICEEXplorer -----------------------------------------------------------}
function TCEMiniExplorerWidget.singleServiceName: string;
begin
  exit('ICEExplorer');
end;

procedure TCEMiniExplorerWidget.browse(const location: string);
begin
  if location.EndsWith('\') or location.EndsWith('/') then
    treeFolders.Root := location[1..location.length - 1]
  else if location.dirExists then
    treeFolders.Root := location
  else if location.fileExists then
    treeFolders.Root := location.extractFileDir;
  fLastFold:=treeFolders.Root;
  fLastListOrTree := treeFolders;
  if treeFolders.Items.Count > 0 then
    treeFolders.Items.Item[0].Selected:=true;
end;

function TCEMiniExplorerWidget.currentLocation: string;
begin
  result := treeFolders.Root;
end;
{$ENDREGION}

end.
