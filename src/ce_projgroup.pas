unit ce_projgroup;
{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, ExtCtrls, Menus,
  Buttons, dialogs, ComCtrls, StdCtrls,
  ce_widget, ce_common, ce_interfaces, ce_writableComponent, ce_observer,
  ce_nativeproject, ce_dubproject, ce_projutils, ce_sharedres, ce_dsgncontrols;

type

  (**
   * Represents a project in a project group
   *)
  TProjectGroupItem = class(TCollectionItem)
  private
    fFilename: string;
    fProj: ICECommonProject;
  published
    property filename: string read fFilename write fFilename;
  public
    property project: ICECommonProject read fProj;
    procedure lazyLoad;
    destructor destroy; override;
  end;

  (**
   * Collection that handles several project at once.
   *)
  TProjectGroup = class(TWritableLfmTextComponent, ICEProjectGroup, IFPObserver)
  private
    fProjectIndex: integer;
    fItems: TCollection;
    fModified: boolean;
    fOnChanged: TNotifyEvent;
    procedure setItems(value: TCollection);
    function getItem(index: integer): TProjectGroupItem;
    procedure doChanged;
    //
    procedure FPOObservedChanged(ASender : TObject; Operation :
      TFPObservedOperation; Data : Pointer);
  protected
    procedure afterLoad; override;
    procedure afterSave; override;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    //
    function singleServiceName: string;
    procedure addProject(aProject: ICECommonProject);
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
    btnFreeFocus: TSpeedButton;
    btnMoveDown: TCEToolButton;
    btnMoveUp: TCEToolButton;
    btnRemProj: TCEToolButton;
    lstProj: TListView;
    Panel2: TPanel;
    StaticText1: TStaticText;
    procedure btnAddUnfocusedClick(Sender: TObject);
    procedure btnFreeFocusClick(Sender: TObject);
    procedure BtnAddProjClick(Sender: TObject);
    procedure btnMoveDownClick(Sender: TObject);
    procedure btnMoveUpClick(Sender: TObject);
    procedure btnRemProjClick(Sender: TObject);
    procedure lstProjDblClick(Sender: TObject);
  private
    fPrevProj: ICECommonProject;
    fFreeProj: ICECommonProject;
    fProjSubj: TCEProjectSubject;
    //
    procedure projNew(aProject: ICECommonProject);
    procedure projChanged(aProject: ICECommonProject);
    procedure projClosing(aProject: ICECommonProject);
    procedure projFocused(aProject: ICECommonProject);
    procedure projCompiling(aProject: ICECommonProject);
    procedure projCompiled(aProject: ICECommonProject; success: boolean);
    //
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
  fItems := TCollection.Create(TProjectGroupItem);
  fItems.FPOAttachObserver(self);
  EntitiesConnector.addSingleService(self);
end;

destructor TProjectGroup.destroy;
begin
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

function TProjectGroup.addItem(const fname: string): TProjectGroupItem;
var
  it: TCollectionItem;
begin
  fModified := true;
  for it in fItems do
  begin
    if SameFileName(TProjectGroupItem(it).fFilename, fname) then
      exit(TProjectGroupItem(it));
  end;
  result := TProjectGroupItem(fItems.Add);
  result.fFilename := fname;
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
    if SameFileName(item[i].fFilename, fname) then
    begin
      item[i].lazyLoad;
      exit(item[i].fProj);
    end;
end;

procedure TProjectGroup.afterLoad;
begin
  inherited;
  fModified:=false;
end;

procedure TProjectGroup.afterSave;
begin
  inherited;
  fModified:=false;
end;

procedure TProjectGroup.addProject(aProject: ICECommonProject);
var
  it: TCollectionItem;
begin
  fModified := true;
  for it in fItems do
  begin
    if SameFileName(TProjectGroupItem(it).fFilename, aProject.filename) then
      exit;
  end;
  it := fItems.Add;
  TProjectGroupItem(it).fFilename := aProject.filename;
  TProjectGroupItem(it).fProj := aProject;
  aProject.inGroup(true);
  fProjectIndex := it.Index;
  doChanged;
end;

procedure TProjectGroup.openGroup(const fname: string);
begin
  loadFromFile(fname);
  doChanged;
end;

procedure TProjectGroup.saveGroup(const fname: string);
begin
  saveToFile(fname);
end;

procedure TProjectGroup.closeGroup;
begin
  fItems.Clear;
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
    fProj := loadProject(fFilename, true);
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
procedure TCEProjectGroupWidget.projNew(aProject: ICECommonProject);
begin
  fPrevProj := aProject;
  if not aProject.inGroup then
    fFreeProj := aProject;
end;

procedure TCEProjectGroupWidget.projChanged(aProject: ICECommonProject);
begin
  updateList;
end;

procedure TCEProjectGroupWidget.projClosing(aProject: ICECommonProject);
begin
  fPrevProj := nil;
  if aProject = fFreeProj then
  begin
    fFreeProj := nil;
    updateList;
  end;
end;

procedure TCEProjectGroupWidget.projFocused(aProject: ICECommonProject);
begin
  fPrevProj := aProject;
  if not aProject.inGroup then
  begin
    fFreeProj := aProject;
    updateList;
  end
  else if (aProject = fFreeProj) and (aProject.inGroup) then
  begin
    fFreeProj := nil;
    updateList;
  end;
end;

procedure TCEProjectGroupWidget.projCompiling(aProject: ICECommonProject);
begin
end;

procedure TCEProjectGroupWidget.projCompiled(aProject: ICECommonProject; success: boolean);
begin
end;
{$ENDREGION}

{$REGION Widget project group things -------------------------------------------}
procedure TCEProjectGroupWidget.BtnAddProjClick(Sender: TObject);
begin
  with TOpenDialog.Create(nil) do
  try
    if not execute then
      exit;
    if projectGroup.findProject(filename) <> nil then
      exit;
    projectGroup.addItem(filename);
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
  projectGroup.addProject(fFreeProj);
  fFreeProj := nil;
  updateList;
end;

procedure TCEProjectGroupWidget.btnMoveDownClick(Sender: TObject);
begin
  if lstProj.ItemIndex = -1 then exit;
  if lstProj.ItemIndex = lstProj.Items.Count-1 then exit;
  //
  projectGroup.items.Exchange(lstProj.ItemIndex, lstProj.ItemIndex + 1);
  lstProj.Items.Exchange(lstProj.ItemIndex, lstProj.ItemIndex + 1);
end;

procedure TCEProjectGroupWidget.btnMoveUpClick(Sender: TObject);
begin
  if lstProj.ItemIndex = -1 then exit;
  if lstProj.ItemIndex = 0 then exit;
  //
  projectGroup.items.Exchange(lstProj.ItemIndex, lstProj.ItemIndex - 1);
  lstProj.Items.Exchange(lstProj.ItemIndex, lstProj.ItemIndex - 1);
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

procedure TCEProjectGroupWidget.handleChanged(sender: TObject);
begin
  updateList;
  if (projectGroup.getProjectIndex <> -1) and (projectGroup.getProjectIndex <> lstProj.ItemIndex) then
  begin
    lstProj.ItemIndex := projectGroup.getProjectIndex;
    lstProjDblClick(nil);
  end;
end;

procedure TCEProjectGroupWidget.updateList;
var
  i: integer;
  p: TProjectGroupItem;
const
  typeStr: array[TCEProjectFormat] of string = ('CE','DUB');
begin
  lstProj.Clear;
  for i := 0 to projectGroup.projectCount-1 do
  begin
    with lstProj.Items.Add do
    begin
      p := projectGroup.item[i];
      p.lazyLoad;
      Data:= p;
      case p.project.getFormat of
        pfNative: Caption := p.fFilename.extractFileName;
        pfDub: Caption := TCEDubProject(p.project.getProject).packageName;
      end;
      SubItems.Add(typeStr[p.fProj.getFormat]);
      SubItems.Add(p.fProj.configurationName(p.fProj.getActiveConfigurationIndex));
    end;
  end;
  if fFreeProj <> nil then
  begin
    case fFreeProj.getFormat of
      pfNative: StaticText1.Caption:= 'Free standing: ' + fFreeProj.filename.extractFileName;
      pfDub: StaticText1.Caption:= 'Free standing: ' + TCEDubProject(fFreeProj.getProject).packageName;
    end;
  end
  else
    StaticText1.Caption:= 'No free standing project';
end;
{$ENDREGION}

initialization
  projectGroup := TProjectGroup.create(nil);
finalization
  projectGroup.Free;
end.

