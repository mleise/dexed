unit ce_projinspect;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, TreeFilterEdit, Forms, Controls, Graphics, actnlist,
  Dialogs, ExtCtrls, ComCtrls, Menus, Buttons, lcltype, ce_nativeproject, ce_interfaces,
  ce_common, ce_widget, ce_observer, ce_dialogs, ce_sharedres, ce_dsgncontrols;

type

  { TCEProjectInspectWidget }

  TCEProjectInspectWidget = class(TCEWidget, ICEProjectObserver)
    btnAddFile: TCEToolButton;
    btnAddFold: TCEToolButton;
    btnRemFile: TCEToolButton;
    btnRemFold: TCEToolButton;
    imgList: TImageList;
    Tree: TTreeView;
    TreeFilterEdit1: TTreeFilterEdit;
    procedure btnAddFileClick(Sender: TObject);
    procedure btnAddFoldClick(Sender: TObject);
    procedure btnRemFileClick(Sender: TObject);
    procedure btnRemFoldClick(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const fnames: array of String);
    procedure TreeClick(Sender: TObject);
    procedure TreeDeletion(Sender: TObject; Node: TTreeNode);
    procedure TreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TreeSelectionChanged(Sender: TObject);
  protected
    procedure updateImperative; override;
    procedure updateDelayed; override;
    procedure SetVisible(value: boolean); override;
    procedure setToolBarFlat(value: boolean); override;
  private
    fActOpenFile: TAction;
    fActSelConf: TAction;
    fActBuildConf: TAction;
    fProject: ICECommonProject;
    fFileNode, fConfNode: TTreeNode;
    fLastFileOrFolder: string;
    fSymStringExpander: ICESymStringExpander;
    procedure actUpdate(sender: TObject);
    procedure TreeDblClick(sender: TObject);
    procedure actOpenFileExecute(sender: TObject);
    procedure actBuildExecute(sender: TObject);
    //
    procedure projNew(project: ICECommonProject);
    procedure projClosing(project: ICECommonProject);
    procedure projFocused(project: ICECommonProject);
    procedure projChanged(project: ICECommonProject);
    procedure projCompiling(project: ICECommonProject);
    procedure projCompiled(project: ICECommonProject; success: boolean);
    procedure updateButtons;
  protected
    function contextName: string; override;
    function contextActionCount: integer; override;
    function contextAction(index: integer): TAction; override;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
  end;

implementation
{$R *.lfm}

{$REGION Standard Comp/Obj------------------------------------------------------}
constructor TCEProjectInspectWidget.create(aOwner: TComponent);
begin
  fSymStringExpander:= getSymStringExpander;
  //
  fActOpenFile := TAction.Create(self);
  fActOpenFile.Caption := 'Open file in editor';
  fActOpenFile.OnExecute := @actOpenFileExecute;
  fActSelConf := TAction.Create(self);
  fActSelConf.Caption := 'Select configuration';
  fActSelConf.OnExecute := @actOpenFileExecute;
  fActSelConf.OnUpdate := @actUpdate;
  fActBuildConf:= TAction.Create(self);
  fActBuildConf.Caption := 'Build configuration';
  fActBuildConf.OnExecute := @actBuildExecute;
  fActBuildConf.OnUpdate := @actUpdate;
  //
  inherited;
  //
  Tree.OnDblClick := @TreeDblClick;
  fFileNode := Tree.Items[0];
  fConfNode := Tree.Items[1];
  //
  Tree.PopupMenu := contextMenu;
  //
  EntitiesConnector.addObserver(self);
end;

destructor TCEProjectInspectWidget.destroy;
begin
  EntitiesConnector.removeObserver(self);
  inherited;
end;

procedure TCEProjectInspectWidget.SetVisible(value: boolean);
begin
  inherited;
  if value then updateImperative;
end;

procedure TCEProjectInspectWidget.setToolBarFlat(value: boolean);
begin
  inherited setToolBarFlat(value);
  TreeFilterEdit1.Flat:=value;
end;
{$ENDREGION}

{$REGION ICEContextualActions---------------------------------------------------}
function TCEProjectInspectWidget.contextName: string;
begin
  exit('Inspector');
end;

function TCEProjectInspectWidget.contextActionCount: integer;
begin
  exit(3);
end;

function TCEProjectInspectWidget.contextAction(index: integer): TAction;
begin
  case index of
    0: exit(fActOpenFile);
    1: exit(fActSelConf);
    else exit(nil);
  end;
end;

procedure TCEProjectInspectWidget.actOpenFileExecute(sender: TObject);
begin
  TreeDblClick(sender);
end;

procedure TCEProjectInspectWidget.actBuildExecute(sender: TObject);
begin
  if fProject <> nil then
  begin
    actOpenFileExecute(sender);
    fProject.compile;
  end;
end;
{$ENDREGION}

{$REGION ICEProjectMonitor -----------------------------------------------------}
procedure TCEProjectInspectWidget.projNew(project: ICECommonProject);
begin
  fLastFileOrFolder := '';
  fProject := project;
  if Visible then
    updateImperative;
  updateButtons;
end;

procedure TCEProjectInspectWidget.projClosing(project: ICECommonProject);
begin
  if not assigned(fProject)  then
    exit;
  if project <> fProject then
    exit;
  fProject := nil;
  fLastFileOrFolder := '';
  updateImperative;
end;

procedure TCEProjectInspectWidget.projFocused(project: ICECommonProject);
begin
  fLastFileOrFolder := '';
  fProject := project;
  updateButtons;
  if Visible then
    beginDelayedUpdate;
end;

procedure TCEProjectInspectWidget.projChanged(project: ICECommonProject);
begin
  if not assigned(fProject)  then
    exit;
  if fProject <> project then
    exit;
  if Visible then
    beginDelayedUpdate;
end;

procedure TCEProjectInspectWidget.projCompiling(project: ICECommonProject);
begin
end;

procedure TCEProjectInspectWidget.projCompiled(project: ICECommonProject; success: boolean);
begin
end;

procedure TCEProjectInspectWidget.updateButtons;
var
  ce: boolean;
begin
  ce := fProject.getFormat = pfNative;
  btnRemFile.Enabled:= ce;
  btnRemFold.Enabled:= ce;
  btnAddFile.Enabled:= ce;
  btnAddFold.Enabled:= ce;
end;

{$ENDREGION}

{$REGION Inspector things -------------------------------------------------------}
procedure TCEProjectInspectWidget.TreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    TreeDblClick(nil);
end;

procedure TCEProjectInspectWidget.TreeClick(Sender: TObject);
begin
  if Tree.Selected.isNotNil then
  begin
    Tree.MultiSelect := Tree.Selected.Parent = fFileNode;
    if not (Tree.Selected.Parent = fFileNode) then
    begin
      Tree.MultiSelect := false;
      Tree.ClearSelection(true);
      Tree.Selected.MultiSelected:=false;
    end;
  end
  else
  begin
    Tree.MultiSelect := false;
    Tree.ClearSelection(true);
  end;
end;

procedure TCEProjectInspectWidget.TreeDeletion(Sender: TObject; Node: TTreeNode
  );
begin
  if Node.isNotNil and Node.Data.isNotNil then
    dispose(PString(Node.Data));
end;

procedure TCEProjectInspectWidget.TreeSelectionChanged(Sender: TObject);
begin
  actUpdate(sender);
  if not assigned(fProject) or Tree.Selected.isNil then
    exit;
  if (Tree.Selected.Parent = fFileNode) then
    fLastFileOrFolder := expandFilenameEx(fProject.basePath,tree.Selected.Text)
  else
    fLastFileOrFolder := tree.Selected.Text;
end;

procedure TCEProjectInspectWidget.TreeDblClick(sender: TObject);
var
  fname: string;
  i: integer;
begin
  if not assigned(fProject) or Tree.Selected.isNil then
    exit;

  if Tree.Selected.Parent = fFileNode then
  begin
    if Tree.Selected.Data.isNotNil then
    begin
      fname := PString(Tree.Selected.Data)^;
      if isEditable(fname.extractFileExt) and fname.fileExists then
        getMultiDocHandler.openDocument(fname);
    end;
  end
  else if Tree.Selected.Parent = fConfNode then
  begin
    i := Tree.Selected.Index;
    fProject.setActiveConfigurationIndex(i);
    beginDelayedUpdate;
  end;
end;

procedure TCEProjectInspectWidget.actUpdate(sender: TObject);
begin
  fActSelConf.Enabled := false;
  fActOpenFile.Enabled := false;
  fActBuildConf.Enabled:= false;
  if Tree.Selected.isNil then
    exit;
  fActSelConf.Enabled := Tree.Selected.Parent = fConfNode;
  fActBuildConf.Enabled := Tree.Selected.Parent = fConfNode;
  fActOpenFile.Enabled := Tree.Selected.Parent = fFileNode;
end;

procedure TCEProjectInspectWidget.btnAddFileClick(Sender: TObject);
var
  fname: string;
  proj: TCENativeProject;
begin
  if not assigned(fProject) or (fProject.getFormat = pfDub) then
    exit;

  proj := TCENativeProject(fProject.getProject);
  with TOpenDialog.Create(nil) do
  try
    options := options + [ofAllowMultiSelect];
    if fLastFileOrFolder.fileExists then
      InitialDir := fLastFileOrFolder.extractFilePath
    else if fLastFileOrFolder.dirExists then
      InitialDir := fLastFileOrFolder;
    filter := DdiagFilter;
    if execute then
    begin
      proj.beginUpdate;
      for fname in Files do
        proj.addSource(fname);
      proj.endUpdate;
    end;
  finally
    free;
  end;
end;

procedure TCEProjectInspectWidget.btnAddFoldClick(Sender: TObject);
var
  dir, fname: string;
  lst: TStringList;
  proj: TCENativeProject;
  i: integer;
begin
  if not assigned(fProject) or (fProject.getFormat = pfDub) then
    exit;

  dir := '';
  proj := TCENativeProject(fProject.getProject);
  if fLastFileOrFolder.fileExists then
    dir := fLastFileOrFolder.extractFilePath
  else if fLastFileOrFolder.dirExists then
    dir := fLastFileOrFolder
  else if fProject.fileName.fileExists then
    dir := fProject.fileName.extractFilePath;
  if selectDirectory('sources', dir, dir, true, 0) then
  begin
    proj.beginUpdate;
    lst := TStringList.Create;
    try
      listFiles(lst, dir, true);
      for i := 0 to lst.Count-1 do
      begin
        fname := lst[i];
        if isDlangCompilable(fname.extractFileExt) then
          proj.addSource(fname);
      end;
    finally
      lst.Free;
      proj.endUpdate;
    end;
  end;
end;

procedure TCEProjectInspectWidget.btnRemFoldClick(Sender: TObject);
var
  dir, fname: string;
  proj: TCENativeProject;
  i: Integer;
begin
  if not assigned(fProject) or (fProject.getFormat = pfDub)
  or Tree.Selected.isNil or (Tree.Selected.Parent <> fFileNode) then
    exit;

  proj := TCENativeProject(fProject.getProject);
  fname := Tree.Selected.Text;
  i := proj.Sources.IndexOf(fname);
  if i = -1 then
    exit;
  fname := fProject.sourceAbsolute(i);
  dir := fname.extractFilePath;
  if not dir.dirExists then
    exit;

  proj.beginUpdate;
  for i:= proj.Sources.Count-1 downto 0 do
    if proj.sourceAbsolute(i).extractFilePath = dir then
      proj.Sources.Delete(i);
  proj.endUpdate;
end;

procedure TCEProjectInspectWidget.btnRemFileClick(Sender: TObject);
var
  fname: string;
  proj: TCENativeProject;
  i, j: integer;
begin
  if not assigned(fProject) or (fProject.getFormat = pfDub)
  or Tree.Selected.isNil or (Tree.Selected.Parent = fFileNode) then
    exit;

  proj := TCENativeProject(fProject.getProject);
  proj.beginUpdate;
  for j:= 0 to Tree.SelectionCount-1 do
  begin
    fname := Tree.Selections[j].Text;
    i := proj.Sources.IndexOf(fname);
    if i <> -1 then
      proj.Sources.Delete(i);
  end;
  proj.endUpdate;
end;

procedure TCEProjectInspectWidget.FormDropFiles(Sender: TObject; const fnames: array of String);
var
  fname, direntry: string;
  lst: TStringList;
  proj: TCENativeProject;
procedure addFile(const value: string);
var
  ext: string;
begin
  ext := value.extractFileExt;
  if not isDlangCompilable(ext) then
    exit;
  proj.addSource(value);
  if isEditable(ext) then
    getMultiDocHandler.openDocument(value);
end;
begin
  if not assigned(fProject) or (fProject.getFormat = pfDub) then
    exit;

  proj := TCENativeProject(fProject.getProject);
  lst := TStringList.Create;
  proj.beginUpdate;
  try for fname in fnames do
    if fname.fileExists then
      addFile(fname)
    else if fname.dirExists then
    begin
      lst.Clear;
      listFiles(lst, fname, true);
      for direntry in lst do
        addFile(dirEntry);
    end;
  finally
    proj.endUpdate;
    lst.Free;
  end;
end;

procedure TCEProjectInspectWidget.updateDelayed;
begin
  updateImperative;
end;

procedure TCEProjectInspectWidget.updateImperative;
var
  src, conf: string;
  itm: TTreeNode;
  i,j: integer;
begin
  fConfNode.DeleteChildren;
  fFileNode.DeleteChildren;

  if not assigned(fProject) then
    exit;

  Tree.BeginUpdate;
  for i := 0 to fProject.sourcesCount-1 do
  begin
    itm := Tree.Items.AddChild(fFileNode, fProject.sourceRelative(i));
    itm.Data:= NewStr(fProject.sourceAbsolute(i));
    itm.ImageIndex := 2;
    itm.SelectedIndex := 2;
  end;
  j := fProject.getActiveConfigurationIndex;
  for i := 0 to fProject.configurationCount-1 do
  begin
    conf := fProject.configurationName(i);
    if i = j then
      conf += ' (active)';
    itm := Tree.Items.AddChild(fConfNode, conf);
    if i = j then
    begin
      itm.ImageIndex := 7;
      itm.SelectedIndex:= 7;
    end
    else
    begin
      itm.ImageIndex := 3;
      itm.SelectedIndex:= 3;
    end;
  end;
  Tree.EndUpdate;
end;
{$ENDREGION --------------------------------------------------------------------}

end.
