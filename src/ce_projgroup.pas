unit ce_projgroup;
{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, ExtCtrls, Menus,
  Buttons, dialogs, ComCtrls, StdCtrls,
  ce_widget, ce_common, ce_interfaces, ce_writableComponent, ce_observer,
  ce_ceproject, ce_dubproject, ce_projutils, ce_sharedres, ce_dsgncontrols,
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
    fConfigIndex: integer;
    fFilename: string;
    fProj: ICECommonProject;
    fGroup: TProjectGroup;
    fAsyncMode: TCEProjectAsyncMode;
    function storeConfigIndex: boolean;
  published
    property filename: string read fFilename write fFilename;
    property asyncMode: TCEProjectAsyncMode read fAsyncMode write fAsyncMode;
    property configurationIndex: integer read fConfigIndex write fConfigIndex stored storeConfigIndex;
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
    fSavedIndex: integer;
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
    function projectIsAsync(ix: integer): boolean;
    function projectModified(ix: integer): boolean;
    function reloadedProjectIndex: integer;
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
    fUpdating: boolean;
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
  fSavedIndex := -1;
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
var
  itm: TProjectGroupItem;
begin
  if assigned(project) and project.inGroup and (project.getFormat = pfDUB) then
  begin
    itm := Self.addItem(project.filename);
    if assigned(itm) then
      itm.configurationIndex:=project.getActiveConfigurationIndex;
  end;
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

function TProjectGroup.projectIsAsync(ix: integer): boolean;
begin
  exit(item[ix].asyncMode = amParallel);
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
  result.configurationIndex:=result.fProj.getActiveConfigurationIndex;
end;

function TProjectGroup.getProject(ix: Integer): ICECommonProject;
begin
  item[ix].lazyLoad;
  result := item[ix].project;
  if result.getFormat = pfDUB then
    result.setActiveConfigurationIndex(item[ix].configurationIndex);
end;

function TProjectGroup.projectModified(ix: integer): boolean;
var
  p: ICECommonProject;
begin
  result := false;
  p := item[ix].project;
  if assigned(p) and (p.modified) then
    result := true
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

function TProjectGroup.reloadedProjectIndex: integer;
begin
  exit(fSavedIndex);
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
    p.filename := patchPlateformPath(p.filename);
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
  if fProjectIndex > projectCount -1 then
    fProjectIndex:= projectCount -1;
  fSavedIndex := fProjectIndex;
  fModified := b;
  if b then
    dlgOkError('the following projects are missing and are removed from the group:' + f,
      'Project group error');
end;

procedure TProjectGroup.afterSave;
begin
  inherited;
  fModified := false;
  fSavedIndex := fProjectIndex;
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
  project.activate;
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
  fModified := false;
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
  fModified := false;
end;

procedure TProjectGroup.closeGroup;
begin
  fItems.Clear;
  fBasePath:='';
  fFilename:= '';
  fModified:=false;
  fProjectIndex := -1;
  fSavedIndex := -1;
  doChanged;
end;

function TProjectGroup.groupModified: boolean;
var
  i: integer;
  b: boolean = false;
begin
  for i:= 0 to fItems.Count-1 do
    if projectModified(i) then
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
    if fProj.getFormat = pfDUB then
      fProj.setActiveConfigurationIndex(fConfigIndex);
  end;
end;

destructor TProjectGroupItem.destroy;
begin
  if fProj <> nil then
    fProj.getProject.free;
  fProj := nil;
  inherited;
end;

function TProjectGroupItem.storeConfigIndex: boolean;
begin
  exit(fProj.getFormat = pfDUB);
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
    Options:= Options + [ofAllowMultiSelect];
    if not execute then
      exit;
    for fname in Files do
    begin
      if projectGroup.findProject(fname.normalizePath) <> nil then
        continue;
      projectGroup.addItem(fname.normalizePath);
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
  if fUpdating then
    exit;
  fUpdating := true;
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
        pfCE: Caption := prj.fFilename.extractFileName;
        pfDUB: Caption := TCEDubProject(prj.project.getProject).packageName;
      end;
      SubItems.Add(typeStr[fmt]);
      SubItems.Add(asyncStr[prj.fAsyncMode]);
      SubItems.Add(prj.fProj.configurationName(prj.fProj.getActiveConfigurationIndex));
    end;
  end;
  if projectGroup.projectCount > 0 then
  begin
    i := projectGroup.getProjectIndex;
    if i > projectGroup.projectCount - 1 then
      i := projectGroup.projectCount-1;
    lstProj.ItemIndex:= i;
  end;
  lstProj.EndUpdate;
  if fFreeProj <> nil then
  begin
    if fFreeProj.filename.fileExists then
      case fFreeProj.getFormat of
        pfCE: StaticText1.Caption:= 'Free standing: ' + fFreeProj.filename.extractFileName;
        pfDUB: StaticText1.Caption:= 'Free standing: ' + TCEDubProject(fFreeProj.getProject).packageName;
      end
    else
      StaticText1.Caption:= 'Free standing: (not yet saved)';
  end
  else
    StaticText1.Caption:= 'No free standing project';
  updateButtons;
  fUpdating := false;
end;
{$ENDREGION}

initialization
  projectGroup := TProjectGroup.create(nil);
finalization
  projectGroup.Free;
end.

