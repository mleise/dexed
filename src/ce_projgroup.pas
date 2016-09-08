unit ce_projgroup;
{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, ExtCtrls, Menus,
  Buttons, dialogs, ComCtrls, StdCtrls,
  ce_widget, ce_common, ce_interfaces, ce_writableComponent, ce_observer,
  ce_nativeproject, ce_dubproject, ce_projutils, ce_sharedres, ce_dsgncontrols,
  ce_dialogs;

type

  TProjectGroup = class;

  TCEProjectAsyncMode = (amSequential, amParallel);

const
  asyncStr: array[TCEProjectAsyncMode] of string = ('wait', 'async');

type

  (**
   * Represents a project in a project group
   *)
  TProjectGroupItem = class(TCollectionItem)
  private
    fFilename: string;
    fProj: ICECommonProject;
    fGroup: TProjectGroup;
    fAsyncMode: TCEProjectAsyncMode;
  published
    property filename: string read fFilename write fFilename;
    property asyncMode: TCEProjectAsyncMode read fAsyncMode write fAsyncMode;
  public
    property project: ICECommonProject read fProj;
    procedure lazyLoad;
    destructor destroy; override;
    function absoluteFilename: string;
  end;

  (**
   * Collection that handles several project at once.
   *)
  TProjectGroup = class(TWritableLfmTextComponent, ICEProjectGroup, IFPObserver, ICEProjectObserver)
  private
    fProjectIndex: integer;
    fItems: TCollection;
    fModified: boolean;
    fOnChanged: TNotifyEvent;
    fBasePath: string;
    fFreeStanding: ICECommonProject;
    procedure setItems(value: TCollection);
    function getItem(index: integer): TProjectGroupItem;
    procedure doChanged;
    //
    procedure FPOObservedChanged(ASender : TObject; Operation :
      TFPObservedOperation; Data : Pointer);
    procedure projNew(project: ICECommonProject);
    procedure projChanged(project: ICECommonProject);
    procedure projClosing(project: ICECommonProject);
    procedure projFocused(project: ICECommonProject);
    procedure projCompiling(project: ICECommonProject);
    procedure projCompiled(project: ICECommonProject; success: boolean);
  protected
    procedure afterLoad; override;
    procedure afterSave; override;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    //
    function singleServiceName: string;
    procedure addProject(project: ICECommonProject);
    procedure openGroup(const fname: string);
    procedure saveGroup(const fname: string);
    procedure closeGroup;
    function groupModified: boolean;
    function groupFilename: string;
    function projectCount: integer;
    function getProjectIndex: integer;
    function getProject(ix: Integer): ICECommonProject;
    function findProject(const fname: string): ICECommonProject;
    procedure setProjectIndex(value: Integer);
    function projectIsAsync(index: integer): boolean;
    //
    function addItem(const fname: string): TProjectGroupItem;
    property item[ix: integer]: TProjectGroupItem read getItem; default;
    property onChanged: TNotifyEvent read fOnChanged write fOnChanged;
  published
    property items: TCollection read fItems write setItems;
    property index: integer read fProjectIndex write setProjectIndex;
  end;

  (**
   * GUI for a project group
   *)

  { TCEProjectGroupWidget }

  TCEProjectGroupWidget = class(TCEWidget, ICEProjectObserver)
    BtnAddProj: TCEToolButton;
    btnAddUnfocused: TSpeedButton;
    btnAsync: TCEToolButton;
    btnFreeFocus: TSpeedButton;
    btnMoveDown: TCEToolButton;
    btnMoveUp: TCEToolButton;
    btnRemProj: TCEToolButton;
    lstProj: TListView;
    Panel2: TPanel;
    StaticText1: TStaticText;
    procedure btnAddUnfocusedClick(Sender: TObject);
    procedure btnAsyncClick(Sender: TObject);
    procedure btnFreeFocusClick(Sender: TObject);
    procedure BtnAddProjClick(Sender: TObject);
    procedure btnMoveDownClick(Sender: TObject);
    procedure btnMoveUpClick(Sender: TObject);
    procedure btnRemProjClick(Sender: TObject);
    procedure lstProjDblClick(Sender: TObject);
    procedure slstProjSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    fPrevProj: ICECommonProject;
    fFreeProj: ICECommonProject;
    fProjSubj: TCEProjectSubject;
    //
    procedure projNew(project: ICECommonProject);
    procedure projChanged(project: ICECommonProject);
    procedure projClosing(project: ICECommonProject);
    procedure projFocused(project: ICECommonProject);
    procedure projCompiling(project: ICECommonProject);
    procedure projCompiled(project: ICECommonProject; success: boolean);
    //
    procedure updateButtons;
    procedure updateList;
    procedure handleChanged(sender: TObject);
  protected
    procedure DoShow; override;
    procedure setToolBarFlat(value: boolean); override;
  public
    constructor create(aOwner: TCOmponent); override;
    destructor destroy; override;
  end;

implementation
{$R *.lfm}

var
  projectGroup: TProjectGroup;

{$REGION TProjectGroup ---------------------------------------------------------}
constructor TProjectGroup.create(aOwner: TComponent);
begin
  inherited;
  Name := 'projectGroup';
  fItems := TCollection.Create(TProjectGroupItem);
  fItems.FPOAttachObserver(self);
  EntitiesConnector.addSingleService(self);
  EntitiesConnector.addObserver(self);
end;

destructor TProjectGroup.destroy;
begin
  EntitiesConnector.removeObserver(self);
  fItems.Clear;
  fItems.Free;
  inherited;
end;

procedure TProjectGroup.setItems(value: TCollection);
begin
  fItems.Assign(value);
end;

function TProjectGroup.getItem(index: integer): TProjectGroupItem;
begin
  exit(TProjectGroupItem(fItems.Items[index]));
end;

procedure TProjectGroup.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data : Pointer);
begin
  if operation = ooChange then
    fModified := true;
end;

procedure TProjectGroup.projNew(project: ICECommonProject);
begin
  if (project <> nil) and not project.inGroup then
    fFreeStanding := project;
end;

procedure TProjectGroup.projChanged(project: ICECommonProject);
begin
end;

procedure TProjectGroup.projClosing(project: ICECommonProject);
begin
  if (project <> nil) and (project = fFreeStanding) then
    fFreeStanding := nil;
end;

procedure TProjectGroup.projFocused(project: ICECommonProject);
begin
  if (project <> nil) and not project.inGroup then
    fFreeStanding := project;
end;

procedure TProjectGroup.projCompiling(project: ICECommonProject);
begin
end;

procedure TProjectGroup.projCompiled(project: ICECommonProject; success: boolean);
begin
end;

procedure TProjectGroup.doChanged;
begin
  if assigned(fOnChanged) then
    fOnChanged(self);
end;

procedure TProjectGroup.setProjectIndex(value: integer);
begin
  if value < 0 then
    value := 0
  else if value > fItems.Count-1 then
    value := fItems.Count-1;
  if fProjectIndex <> value then
  begin
    fProjectIndex := value;
    fModified := true;
  end;
end;

function TProjectGroup.projectIsAsync(index: integer): boolean;
begin
  exit(item[index].asyncMode = amParallel);
end;

function TProjectGroup.addItem(const fname: string): TProjectGroupItem;
var
  it: TCollectionItem;
begin
  fModified := true;
  for it in fItems do
  begin
    if SameFileName(TProjectGroupItem(it).absoluteFilename, fname) then
      exit(TProjectGroupItem(it));
  end;
  result := TProjectGroupItem(fItems.Add);
  result.fGroup := self;
  if fBasePath = '' then
    result.fFilename := fname
  else
    result.fFilename := ExtractRelativepath(fBasePath, fname);
  if assigned(fFreeStanding) and SameFileName(fname, fFreeStanding.filename) then
  begin
    result.fProj := fFreeStanding;
    fFreeStanding.inGroup(true);
    fFreeStanding := nil;
    result.fProj.activate;
  end;
end;

function TProjectGroup.getProject(ix: Integer): ICECommonProject;
begin
  item[ix].lazyLoad;
  exit(item[ix].fProj);
end;

function TProjectGroup.findProject(const fname: string): ICECommonProject;
var
  i: integer;
begin
  result := nil;
  for i := 0 to projectCount-1 do
    if SameFileName(item[i].absoluteFilename, fname) then
    begin
      item[i].lazyLoad;
      exit(item[i].fProj);
    end;
end;

procedure TProjectGroup.afterLoad;
var
  p: TProjectGroupItem;
  i: integer;
  b: boolean = false;
  f: string = '';
begin
  inherited;
  for i:= projectCount-1 downto 0 do
  begin
    p := item[i];
    p.fGroup := self;
    if assigned(fFreeStanding) and (p.absoluteFilename = fFreeStanding.filename) then
    begin
      p.fProj := fFreeStanding;
      fFreeStanding.inGroup(true);
      fFreeStanding := nil;
      p.fProj.activate;
    end;
    if not p.absoluteFilename.fileExists then
    begin
      f += LineEnding + '"' + p.absoluteFilename + '"';
      fItems.Delete(i);
      b := true;
    end;
  end;
  fModified := b;
  if b then
    dlgOkError('the following projects are missing and are removed from the group:' + f,
      'Project group error');
end;

procedure TProjectGroup.afterSave;
begin
  inherited;
  fModified:=false;
end;

procedure TProjectGroup.addProject(project: ICECommonProject);
var
  it: TCollectionItem;
begin
  fModified := true;
  for it in fItems do
    if SameFileName(TProjectGroupItem(it).absoluteFilename, project.filename) then
      exit;
  it := fItems.Add;
  if fBasePath = '' then
    TProjectGroupItem(it).fFilename := project.filename
  else
    TProjectGroupItem(it).fFilename := ExtractRelativepath(fBasePath, project.filename);
  TProjectGroupItem(it).fProj := project;
  TProjectGroupItem(it).fGroup := self;
  project.inGroup(true);
  fProjectIndex := it.Index;
  doChanged;
end;

procedure TProjectGroup.openGroup(const fname: string);
var
  i: integer;
begin
  fBasePath := fname.extractFilePath;
  loadFromFile(fname);
  for i:= 0 to fItems.Count-1 do
    getItem(i).fGroup := self;
  doChanged;
end;

procedure TProjectGroup.saveGroup(const fname: string);
var
  i: integer;
  c: boolean = false;
  n: string;
begin
  n := fname.extractFilePath;
  if (fBasePath <> '') and (n <> fBasePath) then
  begin
    c := true;
    for i:= 0 to projectCount-1 do
      getItem(i).fFilename := getItem(i).absoluteFilename;
  end
  else if fBasePath = '' then
    c := true;
  if c then for i:= 0 to projectCount-1 do
    getItem(i).fFilename := ExtractRelativepath(n, getItem(i).fFilename);
  fBasePath := n;
  saveToFile(fname);
end;

procedure TProjectGroup.closeGroup;
begin
  fItems.Clear;
  fBasePath:='';
  fFilename:= '';
  fModified:=false;
  fProjectIndex := -1;
  doChanged;
end;

function TProjectGroup.groupModified: boolean;
var
  i: integer;
  b: boolean = false;
begin
  for i:= 0 to fItems.Count-1 do
    if (getItem(i).fProj <> nil) and getItem(i).fProj.modified then
    begin
      b := true;
      break;
    end;
  exit(fModified or b);
end;

function TProjectGroup.groupFilename: string;
begin
  exit(Filename);
end;

function TProjectGroup.projectCount: integer;
begin
  exit(fItems.Count);
end;

function TProjectGroup.getProjectIndex: integer;
begin
  exit(fProjectIndex);
end;

function TProjectGroup.singleServiceName: string;
begin
  exit('ICEProjectGroup');
end;

procedure TProjectGroupItem.lazyLoad;
begin
  if fProj = nil then
  begin
    fProj := loadProject(absoluteFilename, true);
    fProj.inGroup(true);
  end;
end;

destructor TProjectGroupItem.destroy;
begin
  if fProj <> nil then
    fProj.getProject.free;
  fProj := nil;
  inherited;
end;

function TProjectGroupItem.absoluteFilename: string;
begin
  if fGroup.fBasePath = '' then
    result := fFilename
  else
    result := expandFilenameEx(fGroup.fBasePath, fFilename);
end;
{$ENDREGION}

{$REGION Widget Standard component things --------------------------------------}
constructor TCEProjectGroupWidget.create(aOwner: TCOmponent);
begin
  inherited;
  AssignPng(btnFreeFocus, 'PENCIL');
  AssignPng(btnAddUnfocused, 'DOCUMENT_ADD');
  projectGroup.onChanged:= @handleChanged;
  fProjSubj:= TCEProjectSubject.Create;
end;

destructor TCEProjectGroupWidget.destroy;
begin
  fProjSubj.free;
  inherited;
end;

procedure TCEProjectGroupWidget.DoShow;
begin
  inherited;
  updateList;
end;

procedure TCEProjectGroupWidget.setToolBarFlat(value: boolean);
begin
  inherited setToolBarFlat(value);
  btnFreeFocus.flat := value;
  btnAddUnfocused.flat := value;
end;
{$ENDREGION}

{$REGION Widget ICEProjectObserver ---------------------------------------------}
procedure TCEProjectGroupWidget.projNew(project: ICECommonProject);
begin
  fPrevProj := project;
  if not project.inGroup then
    fFreeProj := project;
end;

procedure TCEProjectGroupWidget.projChanged(project: ICECommonProject);
begin
  updateList;
end;

procedure TCEProjectGroupWidget.projClosing(project: ICECommonProject);
begin
  fPrevProj := nil;
  if project = fFreeProj then
  begin
    fFreeProj := nil;
    updateList;
  end;
end;

procedure TCEProjectGroupWidget.projFocused(project: ICECommonProject);
begin
  fPrevProj := project;
  if not project.inGroup then
  begin
    fFreeProj := project;
    updateList;
  end
  else if project = fFreeProj then
  begin
    fFreeProj := nil;
    updateList;
  end;
end;

procedure TCEProjectGroupWidget.projCompiling(project: ICECommonProject);
begin
end;

procedure TCEProjectGroupWidget.projCompiled(project: ICECommonProject; success: boolean);
begin
end;
{$ENDREGION}

{$REGION Widget project group things -------------------------------------------}
procedure TCEProjectGroupWidget.BtnAddProjClick(Sender: TObject);
var
  fname: string;
  added: boolean;
begin
  with TOpenDialog.Create(nil) do
  try
    Options:= [ofAllowMultiSelect, ofEnableSizing];
    if not execute then
      exit;
    for fname in Files do
    begin
      if projectGroup.findProject(fname) <> nil then
        continue;
      projectGroup.addItem(fname);
      added := true;
    end;
    if added then
      updateList;
  finally
    free;
  end;
end;

procedure TCEProjectGroupWidget.btnFreeFocusClick(Sender: TObject);
begin
  if fFreeProj <> nil then
    subjProjFocused(fProjSubj, fFreeProj);
end;

procedure TCEProjectGroupWidget.btnAddUnfocusedClick(Sender: TObject);
begin
  if fFreeProj = nil then
    exit;
  if not fFreeProj.filename.fileExists then
    exit;
  projectGroup.addProject(fFreeProj);
  fFreeProj := nil;
  updateList;
end;

procedure TCEProjectGroupWidget.btnAsyncClick(Sender: TObject);
var
  prj: TProjectGroupItem;
begin
  if lstProj.ItemIndex = -1 then exit;
  //
  prj := projectGroup.item[lstProj.ItemIndex];
  case prj.asyncMode of
    amSequential: prj.asyncMode := amParallel;
    amParallel: prj.asyncMode := amSequential;
  end;
  updateButtons;
end;

procedure TCEProjectGroupWidget.btnMoveDownClick(Sender: TObject);
begin
  if lstProj.ItemIndex = -1 then exit;
  if lstProj.ItemIndex = lstProj.Items.Count-1 then exit;
  //
  projectGroup.items.Exchange(lstProj.ItemIndex, lstProj.ItemIndex + 1);
  lstProj.Items.Exchange(lstProj.ItemIndex, lstProj.ItemIndex + 1);
  projectGroup.index:=projectGroup.index+1;
  lstProj.ItemIndex:=lstProj.ItemIndex+1;
end;

procedure TCEProjectGroupWidget.btnMoveUpClick(Sender: TObject);
begin
  if lstProj.ItemIndex = -1 then exit;
  if lstProj.ItemIndex = 0 then exit;
  //
  projectGroup.items.Exchange(lstProj.ItemIndex, lstProj.ItemIndex - 1);
  lstProj.Items.Exchange(lstProj.ItemIndex, lstProj.ItemIndex - 1);
  projectGroup.index:=projectGroup.index-1;
  lstProj.ItemIndex:=lstProj.ItemIndex-1;
end;

procedure TCEProjectGroupWidget.btnRemProjClick(Sender: TObject);
begin
  if lstProj.ItemIndex = -1 then exit;
  projectGroup.items.Delete(lstProj.Selected.Index);
  updateList;
end;

procedure TCEProjectGroupWidget.lstProjDblClick(Sender: TObject);
begin
  if lstProj.ItemIndex = -1 then
    exit;
  TProjectGroupItem(lstProj.Selected.Data).lazyLoad;
  subjProjFocused(fProjSubj, TProjectGroupItem(lstProj.Selected.Data).project);
  if projectGroup.getProjectIndex <> lstProj.ItemIndex then
    projectGroup.setProjectIndex(lstProj.ItemIndex);
end;

procedure TCEProjectGroupWidget.slstProjSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  updateButtons
end;

procedure TCEProjectGroupWidget.handleChanged(sender: TObject);
begin
  updateList;
  if (projectGroup.getProjectIndex <> -1) and (projectGroup.getProjectIndex <> lstProj.ItemIndex) then
  begin
    lstProj.ItemIndex := projectGroup.getProjectIndex;
    lstProjDblClick(nil);
  end;
end;

procedure TCEProjectGroupWidget.updateButtons;
var
  idx: integer;
  asc: TCEProjectAsyncMode;
begin
  idx := lstProj.ItemIndex;
  if idx = -1 then
  begin
    btnMoveDown.Enabled:= false;
    btnMoveUp.Enabled:= false;
    btnRemProj.Enabled:= false;
    btnAsync.Enabled:= false;
  end
  else
  begin
    btnMoveDown.Enabled:= idx <> projectGroup.projectCount-1;
    btnMoveUp.Enabled:= idx <> 0;
    btnRemProj.Enabled:= true;
    btnAsync.Enabled:= true;
    asc := projectGroup.item[idx].asyncMode;
    case asc of
      amSequential:
      begin
        btnAsync.resourceName:= 'ARROW_JOIN';
        btnAsync.hint := 'do no wait for the previous projects';
      end;
      amParallel:
      begin
        btnAsync.resourceName:= 'ARROW_DIVIDE';
        btnAsync.hint := 'wait for the previous projects';
      end;
    end;
    lstProj.Items.Item[idx].SubItems[1] := asyncStr[asc];
  end;
end;

procedure TCEProjectGroupWidget.updateList;
var
  i: integer;
  prj: TProjectGroupItem;
  fmt: TCEProjectFormat;
const
  typeStr: array[TCEProjectFormat] of string = ('CE','DUB');
begin
  lstProj.BeginUpdate;
  lstProj.Items.Clear;
  for i := 0 to projectGroup.projectCount-1 do
  begin
    with lstProj.Items.Add do
    begin
      prj := projectGroup.item[i];
      prj.fGroup := projectGroup;
      prj.lazyLoad;
      Data:= prj;
      fmt := prj.project.getFormat;
      case fmt of
        pfNative: Caption := prj.fFilename.extractFileName;
        pfDub: Caption := TCEDubProject(prj.project.getProject).packageName;
      end;
      SubItems.Add(typeStr[fmt]);
      SubItems.Add(asyncStr[prj.fAsyncMode]);
      SubItems.Add(prj.fProj.configurationName(prj.fProj.getActiveConfigurationIndex));
    end;
  end;
  if projectGroup.projectCount > 0 then
  begin
    i := projectGroup.getProjectIndex;
    if i > projectGroup.projectCount then
      i := projectGroup.projectCount-1;
    lstProj.ItemIndex:= i;
  end;
  lstProj.EndUpdate;
  if fFreeProj <> nil then
  begin
    if fFreeProj.filename.fileExists then
      case fFreeProj.getFormat of
        pfNative: StaticText1.Caption:= 'Free standing: ' + fFreeProj.filename.extractFileName;
        pfDub: StaticText1.Caption:= 'Free standing: ' + TCEDubProject(fFreeProj.getProject).packageName;
      end
    else
      StaticText1.Caption:= 'Free standing: (not yet saved)';
  end
  else
    StaticText1.Caption:= 'No free standing project';
  updateButtons;
end;
{$ENDREGION}

initialization
  projectGroup := TProjectGroup.create(nil);
finalization
  projectGroup.Free;
end.

