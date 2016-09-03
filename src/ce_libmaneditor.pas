unit ce_libmaneditor;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, ComCtrls, Buttons, LazFileUtils, strutils, fphttpclient, StdCtrls,
  xfpjson, xjsonparser,
  ce_widget, ce_interfaces, ce_nativeproject, ce_dmdwrap, ce_common, ce_dialogs,
  ce_sharedres, process, ce_dubproject, ce_observer, ce_dlang, ce_libman,
  ce_projutils, ce_dsgncontrols;

type

  TDubPackageQueryForm = class(TForm)
  private
    class var fList: TJSONData;
    class var fGetLatestTag: boolean;
    cbb: TComboBox;
    function getPackageName: string;
    function getPackageVersion: string;
    procedure getList(sender: TObject);
    procedure fillList;
    procedure btnTagCLick(sender: TObject);
    procedure updateHint(sender: TObject);
  public
    class function showAndWait(out pName, pVersion: string): TModalResult; static;
    class destructor classDtor;
    constructor Create(TheOwner: TComponent); override;
    property packageName: string read getPackageName;
    property packageVersion: string read getPackageVersion;
  end;

  { TCELibManEditorWidget }
  TCELibManEditorWidget = class(TCEWidget, ICEProjectObserver)
    btnAddLib: TCEToolButton;
    btnDubFetch: TCEToolButton;
    btnEditAlias: TCEToolButton;
    btnEnabled: TCEToolButton;
    btnMoveDown: TCEToolButton;
    btnMoveUp: TCEToolButton;
    btnOpenProj: TCEToolButton;
    btnReg: TCEToolButton;
    btnRemLib: TCEToolButton;
    btnSelFile: TCEToolButton;
    btnSelfoldOfFiles: TCEToolButton;
    btnSelProj: TCEToolButton;
    btnSelRoot: TCEToolButton;
    List: TListView;
    procedure btnAddLibClick(Sender: TObject);
    procedure btnEnabledClick(Sender: TObject);
    procedure btnDubFetchClick(Sender: TObject);
    procedure btnEditAliasClick(Sender: TObject);
    procedure btnOpenProjClick(Sender: TObject);
    procedure btnRegClick(Sender: TObject);
    procedure btnRemLibClick(Sender: TObject);
    procedure btnSelFileClick(Sender: TObject);
    procedure btnSelfoldOfFilesClick(Sender: TObject);
    procedure btnSelProjClick(Sender: TObject);
    procedure btnSelRootClick(Sender: TObject);
    procedure btnMoveUpClick(Sender: TObject);
    procedure btnMoveDownClick(Sender: TObject);
    procedure ListEdited(Sender: TObject; Item: TListItem; var value: string);
    procedure ListSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean
      );
  private
    fProj: ICECommonProject;
    fFreeProj: ICECommonProject;
    fModStart: boolean;
    procedure updateButtonsState;
    procedure projNew(project: ICECommonProject);
    procedure projChanged(project: ICECommonProject);
    procedure projClosing(project: ICECommonProject);
    procedure projFocused(project: ICECommonProject);
    procedure projCompiling(project: ICECommonProject);
    procedure projCompiled(project: ICECommonProject; success: boolean);
    function  itemForRow(row: TListItem): TLibraryItem;
    procedure RowToLibrary(row: TListItem);
    function sourceRoot(project: ICECommonProject): string;
    procedure lexFindToken(const token: PLexToken; out stop: boolean);
    //
    procedure dataToGrid;
  protected
    procedure DoShow; override;
  public
    constructor Create(aOwner: TComponent); override;
  end;

  // determine the root of a library, according to the module names
  //function sourceRoot(project: ICECommonProject): string;

implementation
{$R *.lfm}


const
  notav: string = '< n/a >';
  enableStr: array [boolean] of string = ('false','true');

constructor TCELibManEditorWidget.Create(aOwner: TComponent);
begin
  inherited;
end;

procedure TCELibManEditorWidget.updateButtonsState;
begin
  btnReg.Enabled := (fProj <> nil) and (fProj.binaryKind = staticlib) and
    fProj.Filename.fileExists;
  btnOpenProj.Enabled := List.Selected.isNotNil and
    List.Selected.SubItems[2].fileExists;
  if List.Selected.isNotNil and itemForRow(List.Selected).isNotNil and
    itemForRow(List.Selected).enabled then
      btnEnabled.resourceName := 'BOOK'
  else
    btnEnabled.resourceName := 'BOOK_GREY';
end;

procedure TCELibManEditorWidget.projNew(project: ICECommonProject);
begin
  fProj := project;
  if not project.inGroup then
    fFreeProj := project;
end;

procedure TCELibManEditorWidget.projChanged(project: ICECommonProject);
begin
  if fProj = nil then exit;
  if fProj <> project then
    exit;
  //
  updateButtonsState;
end;

procedure TCELibManEditorWidget.projClosing(project: ICECommonProject);
begin
  fProj := nil;
  if project = fFreeProj then
    fFreeProj := nil;
  updateButtonsState;
end;

procedure TCELibManEditorWidget.projFocused(project: ICECommonProject);
begin
  fProj := project;
  if not project.inGroup then
    fFreeProj := project;
  updateButtonsState;
end;

procedure TCELibManEditorWidget.projCompiling(project: ICECommonProject);
begin
end;

procedure TCELibManEditorWidget.projCompiled(project: ICECommonProject; success: boolean);
begin
end;

function TCELibManEditorWidget.itemForRow(row: TListItem): TLibraryItem;
begin
  result := TLibraryItem(row.Data);
end;

procedure TCELibManEditorWidget.ListEdited(Sender: TObject; Item: TListItem; var value: string);
begin
  if Item.isNotNil then
    RowToLibrary(item);
end;

procedure TCELibManEditorWidget.ListSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  updateButtonsState;
end;

procedure TCELibManEditorWidget.btnAddLibClick(Sender: TObject);
var
  itm: TListItem;
begin
  itm := List.Items.Add;
  itm.Data := LibMan.libraries.Add;
  itm.Caption := notav;
  itm.SubItems.Add(notav);
  itm.SubItems.Add(notav);
  itm.SubItems.Add(notav);
  itm.SubItems.Add(enableStr[true]);
  SetFocus;
  itm.Selected := True;
end;

class destructor TDubPackageQueryForm.classDtor;
begin
  fList.Free;
end;

constructor TDubPackageQueryForm.Create(TheOwner: TComponent);
var
  bok: TBitBtn;
  bno: TBitBtn;
  bww: TBitBtn;
  bsv: TSpeedButton;
begin
  inherited;

  width  := 400;
  height := 34;
  BorderStyle:= bsToolWindow;
  caption := 'Select or type the DUB package name';
  Position:= poMainFormCenter;

  cbb := TComboBox.Create(self);
  cbb.Parent := self;
  cbb.AutoComplete := true;
  cbb.Align := alClient;
  cbb.BorderSpacing.Around := 6;
  cbb.Sorted:= true;
  cbb.ShowHint:=true;
  cbb.OnSelect:= @updateHint;
  cbb.OnCloseUp:=@updateHint;

  bsv := TSpeedButton.Create(self);
  bsv.Parent := self;
  bsv.Align := alRight;
  bsv.Width:= 28;
  bsv.BorderSpacing.Around := 4;
  bsv.ShowHint := true;
  bsv.Hint := 'get latest tag, by default get master';
  bsv.OnClick:= @btnTagCLick;
  bsv.AllowAllUp := true;
  bsv.GroupIndex := 1;
  bsv.Layout:= blGlyphTop;
  bsv.Spacing:= 2;
  bsv.Down:=fGetLatestTag;
  AssignPng(bsv, 'TAG_PURPLE');

  bww := TBitBtn.Create(self);
  bww.Parent := self;
  bww.Align := alRight;
  bww.Width:= 28;
  bww.BorderSpacing.Around := 4;
  bww.ShowHint := true;
  bww.Hint := 'get the package list';
  bww.OnClick:= @getList;
  bww.Layout:= blGlyphTop;
  bww.Spacing:= 2;
  AssignPng(bww, 'ARROW_UPDATE');

  bok := TBitBtn.Create(self);
  bok.Parent := self;
  bok.ModalResult:= mrOk;
  bok.Align := alRight;
  bok.Width := 28;
  bok.BorderSpacing.Around := 4;
  bok.Hint := 'try to fetch, compile and auto-register';
  bok.ShowHint := true;
  bok.Layout:= blGlyphTop;
  bok.Spacing:= 2;
  AssignPng(bok, 'ACCEPT');

  bno := TBitBtn.Create(self);
  bno.Parent := self;
  bno.ModalResult:= mrCancel;
  bno.Align := alRight;
  bno.Width:= 28;
  bno.BorderSpacing.Around := 4;
  bno.Hint := 'cancel and do nothing';
  bno.ShowHint := true;
  bno.Layout:= blGlyphTop;
  bno.Spacing:= 2;
  AssignPng(bno, 'CANCEL');

  fillList;
end;

procedure TDubPackageQueryForm.btnTagCLick(sender: TObject);
begin
  fGetLatestTag:= TSpeedButton(sender).down;
end;

procedure TDubPackageQueryForm.getList(sender: TObject);
var
  pge: string;
  cli: TFPHTTPClient;
  prs: TJSONParser;
begin
  if assigned(fList) then
    fList.free;
  cli := TFPHTTPClient.Create(nil);
  try
    try
      pge := cli.Get('https://code.dlang.org/api/packages/search');
    except
      pge := '[]';
    end;
  finally
    cli.Free;
  end;
  prs := TJSONParser.Create(pge, []);
  try
    fList := prs.Parse;
  finally
    prs.Free;
  end;
  fillList;
end;

procedure TDubPackageQueryForm.fillList;
var
  itm: TJSONData;
  i: integer;
begin
  cbb.Clear;
  if fList.isNotNil and (fList.JSONType = jtArray) then
    for i := 0 to fList.Count -1 do
  begin
    itm := fList.Items[i].FindPath('version');
    if itm.isNil then
      continue;
    itm := fList.Items[i].FindPath('name');
    if itm.isNil then
      continue;
    cbb.Items.AddObject(itm.AsString, fList.Items[i]);
  end;
end;

function TDubPackageQueryForm.getPackageName: string;
begin
  result := cbb.Text;
end;

function TDubPackageQueryForm.getPackageVersion: string;
var
  jsn: TJSONData;
begin
  result := 'master';
  if fGetLatestTag then
  begin
    // list is updated
    if fList.isNotNil and (cbb.ItemIndex <> -1) and
      cbb.Items.Objects[cbb.ItemIndex].isNotNil then
    begin
      jsn := TJSONData(cbb.Items.Objects[cbb.ItemIndex]);
      jsn := jsn.FindPath('version');
      result := jsn.AsString;
    end
    else
    // use API
    begin
      with TFPHTTPClient.Create(nil) do
      try
        try
          result := Get('https://code.dlang.org/api/packages/' + packageName + '/latest');
        except
          result := 'master';
        end;
      finally
        Free;
      end;
      if (result.length >= 7) and (result[2] in ['0'..'9']) then
        result := result[2..result.length-1]
    end;
  end;
end;

procedure TDubPackageQueryForm.updateHint(sender: TObject);
var
  jsn: TJSONData;
begin
  if (cbb.ItemIndex <> -1) and cbb.Items.Objects[cbb.ItemIndex].isNotNil then
  try
    jsn := TJSONData(cbb.Items.Objects[cbb.ItemIndex]);
    jsn := jsn.FindPath('description');
    if jsn.isNotNil then
      cbb.Hint:= jsn.AsString;
  except
  end;
end;

class function TDubPackageQueryForm.showAndWait(out pName, pVersion: string): TModalResult;
var
  frm: TDubPackageQueryForm;
begin
  frm := TDubPackageQueryForm.Create(nil);
  result := frm.ShowModal;
  if result = mrOk then
  begin
    pName := frm.packageName;
    pVersion := frm.packageVersion;
  end
  else
  begin
    pName := '';
    pVersion := ''
  end;
  frm.Free;
end;

procedure TCELibManEditorWidget.btnDubFetchClick(Sender: TObject);
var
  dub: TProcess;
  nme: string = '';
  ver: string;
  msg: string;
  pth: string;
  dfn: string;
  str: TStringList;
  itf: ICEMessagesDisplay;
  err: integer;
  prj: TCEDubProject;
  ovw: boolean = false;
  row: TListItem = nil;
begin
  if TDubPackageQueryForm.showAndWait(nme, ver) <> mrOk then
    exit;
  if List.Items.FindCaption(0, nme, false, false, false).isNotNil then
  begin
    if dlgYesNo(format('a library item with the alias "%s" already exists, do you wish to update it ?',
      [nme])) <> mrYes then exit
    else ovw := true;
  end;
  {$IFDEF WINDOWS}
  pth := GetEnvironmentVariable('APPDATA') + '\dub\packages\' + nme + '-' + ver;
  {$ELSE}
  pth := GetEnvironmentVariable('HOME') + '/.dub/packages/' + nme + '-' + ver;
  {$ENDIF}
  itf := getMessageDisplay;
  if pth.dirExists and not DeleteDirectory(pth, false) then
  begin
    itf.message('the existing package cant be deleted. To be updated the package must be deleted manually',
      nil, amcMisc, amkWarn);
    exit;
  end;

  // fetch
  dub := TProcess.Create(nil);
  try
    dub.Executable:= 'dub';
    dub.Options:= [poUsePipes, poStderrToOutPut];
    dub.ShowWindow:= swoHIDE;
    dub.Parameters.Add('fetch');
    dub.Parameters.Add(nme);
    if ver = 'master' then
      dub.Parameters.Add('--version=~master')
    else
      dub.Parameters.Add('--version=' + ver);
    dub.Execute;
    str := TStringList.Create;
    try
      processOutputToStrings(dub, str);
      while dub.Running do;
      err := dub.ExitStatus;
      for msg in str do
        itf.message(msg, nil, amcMisc, amkAuto);
    finally
      str.Free;
    end;
  finally
    dub.Free;
  end;
  if err <> 0 then
  begin
    itf.message('error, failed to fetch the package', nil, amcMisc, amkErr);
    exit;
  end;

  // get the description
  if FileExists(pth + DirectorySeparator + 'dub.json') then
    dfn := pth + DirectorySeparator + 'dub.json'
  else if FileExists(pth + DirectorySeparator + 'package.json') then
    dfn := pth + DirectorySeparator + 'package.json'
  else if FileExists(pth + DirectorySeparator + nme + DirectorySeparator + 'dub.json') then
    dfn := pth + DirectorySeparator + nme + DirectorySeparator + 'dub.json'
  else if FileExists(pth + DirectorySeparator + nme + DirectorySeparator + 'package.json') then
    dfn := pth + DirectorySeparator + nme + DirectorySeparator + 'package.json'
  else
    dfn := '';

  if not dfn.fileExists or dfn.isEmpty then
  begin
    itf.message('error, the DUB description cannot be located or it has not the JSON format',
      nil, amcMisc, amkErr);
    exit;
  end;
  pth := dfn.extractFileDir;

  // build
  dub := TProcess.Create(nil);
  try
    dub.Executable:= 'dub';
    dub.ShowWindow:= swoHIDE;
    dub.Options:= [poUsePipes, poStderrToOutPut];
    dub.Parameters.Add('build');
    dub.Parameters.Add('--build=release');
    dub.Parameters.Add('--force');
    dub.Parameters.Add('--compiler=' + DubCompilerFilename);
    dub.CurrentDirectory:= pth;
    dub.Execute;
    str := TStringList.Create;
    try
      processOutputToStrings(dub, str);
      while dub.Running do ;
      err := dub.ExitStatus;
      for msg in str do
        itf.message(msg, nil, amcMisc, amkAuto);
    finally
      str.Free;
    end;
  finally
    dub.Free;
  end;
  if err <> 0 then
  begin
    // allow "sourceLibrary"
    EntitiesConnector.beginUpdate;
    prj := TCEDubProject.create(nil);
    try
      prj.loadFromFile(dfn);
      if prj.json.isNotNil and TJSONObject(prj.json).Find('targetType').isNotNil
        and (TJSONObject(prj.json).Find('targetType').AsString = 'sourceLibrary')
      then
      begin
        if ovw then
           row := List.FindCaption(0, nme, true, true, true);
        if row.isNil then
           row := List.Items.Add;
        if row.Data.isNil then
          row.Data := LibMan.libraries.Add;
        row.Caption:= nme;
        row.SubItems.Clear;
        nme := sourceRoot(prj as ICECommonProject);
        row.SubItems.Add(nme);
        row.SubItems.Add(nme);
        row.SubItems.Add(prj.filename);
        row.SubItems.Add(enableStr[true]);
        row.Selected:=true;
        RowToLibrary(row);
        itf.message('The package to register is a source library.' +
          'It is not pre-compiled but its sources are registered', nil, amcMisc, amkInf);
      end else
        itf.message('error, failed to compile the package to register', nil, amcMisc, amkErr);
    finally
      prj.Free;
      EntitiesConnector.endUpdate;
    end;
    showWidget;
    exit;
  end;

  // project used to get the infos
  EntitiesConnector.beginUpdate;
  prj := TCEDubProject.create(nil);
  try
    prj.loadFromFile(dfn);
    if prj.filename.isNotEmpty and (prj.binaryKind = staticlib) then
    begin
      if ovw then
        row := List.FindCaption(0, nme, true, true, true);
      if row.isNil then
        row := List.Items.Add;
      if row.Data.isNil then
        row.Data := LibMan.libraries.Add;
      row.Caption := nme;
      row.SubItems.Clear;
      row.SubItems.Add(prj.outputFilename);
      row.SubItems.Add(sourceRoot(prj as ICECommonProject));
      row.SubItems.Add(prj.filename);
      row.SubItems.Add(enableStr[true]);
      row.Selected:=true;
      RowToLibrary(row);
      showWidget;
    end else
      itf.message('warning, the package json description can not be found or the target is not a static library',
        nil, amcMisc, amkWarn);
  finally
    prj.Free;
    EntitiesConnector.endUpdate;
  end;
end;

procedure TCELibManEditorWidget.btnEditAliasClick(Sender: TObject);
var
  al: string;
  i: integer;
begin
  if List.Selected.isNil then
    exit;
  al := List.Selected.Caption;
  if (al = 'phobos') or (al = 'druntime') then
  begin
    dlgOkInfo('phobos and druntime cannot be renamed');
  end else
  begin
    if inputQuery('library alias', '', al) then
    begin
      for i := 0 to LibMan.librariesCount-1 do
        if (LibMan.libraryByIndex[i].libAlias = al) and
          (LibMan.libraryByIndex[i] <> itemForRow(List.Selected)) then
      begin
        dlgOkError('This alias is already used by another library, the renaming is canceled');
        exit;
      end;
      List.Selected.Caption := al;
      RowToLibrary(List.Selected);
    end;
  end;
end;

procedure TCELibManEditorWidget.btnEnabledClick(Sender: TObject);
begin
  if List.Selected.isNil then
    exit;

  if List.Selected.SubItems[3] = 'true' then
    List.Selected.SubItems[3] := 'false'
  else
    List.Selected.SubItems[3] := 'true';
  RowToLibrary(List.Selected);
  updateButtonsState;
end;

procedure TCELibManEditorWidget.btnOpenProjClick(Sender: TObject);
var
  fname: string;
  fmt: TCEProjectFileFormat;
begin
  if List.Selected.isNil then exit;
  fname := List.Selected.SubItems[2];
  if not fname.fileExists then exit;
  //
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
  end
  else dlgOkInfo('the project file for this library seems to be invalid');
end;

procedure TCELibManEditorWidget.btnRegClick(Sender: TObject);
var
  str: TStringList;
  fname: string;
  root: string;
  lalias: string;
  row: TListItem;
begin
  if fProj = nil then exit;
  //
  fname := fProj.filename;
  lalias := ExtractFileNameOnly(fname);
  if List.Items.FindCaption(0, lalias, false, false, false) <> nil then
  begin
    dlgOkInfo(format('a library item with the alias "%s" already exists, delete it before trying again.',
      [lalias]));
    exit;
  end;
  //
  str := TStringList.Create;
  try
    root := sourceRoot(fProj);
    if root.isEmpty then
    begin
      dlgOkInfo('the static library can not be registered because its source files have no common folder');
      exit;
    end;
    //
    fname := fProj.outputFilename;
    row := List.Items.Add;
    row.Data := LibMan.libraries.Add;
    row.Caption := ExtractFileNameOnly(fname);
    if fname.extractFileExt <> libExt then
      row.SubItems.add(fname + libExt)
    else
      row.SubItems.add(fname);
    row.SubItems.add(root);
    row.SubItems.add(fProj.filename);
    row.SubItems.add(enableStr[true]);
    if not row.SubItems[0].fileExists then
      dlgOkInfo('the library file does not exist, maybe the project not been already compiled ?');
    row.Selected:= true;
    SetFocus;
    RowToLibrary(row);
  finally
    str.free;
  end;
end;

procedure TCELibManEditorWidget.btnRemLibClick(Sender: TObject);
begin
  if List.Selected.isNil then
    exit;
  LibMan.libraries.Delete(List.Selected.Index);
  List.Items.Delete(List.Selected.Index);
end;

procedure TCELibManEditorWidget.btnSelProjClick(Sender: TObject);
var
  ini: string;
begin
  if List.Selected.isNil then
    exit;
  //
  ini := List.Selected.SubItems[2];
  with TOpenDialog.Create(nil) do
  try
    FileName := ini;
    if Execute then
      List.Selected.SubItems[2] := FileName;
  finally
    free;
  end;
  RowToLibrary(List.Selected);
end;

procedure TCELibManEditorWidget.btnSelFileClick(Sender: TObject);
var
  ini: string = '';
begin
  if List.Selected.isNil then
    exit;
  //
  ini := List.Selected.SubItems[0];
  with TOpenDialog.Create(nil) do
  try
    filename := ini;
    if Execute then
    begin
      if not filename.fileExists then
        List.Selected.SubItems[0] := filename.extractFilePath
      else
      begin
        List.Selected.SubItems[0] := filename;
        if (List.Selected.Caption.isEmpty) or (List.Selected.Caption = notav) then
          List.Selected.Caption := ChangeFileExt(filename.extractFileName, '');
      end;
    end;
  finally
    Free;
  end;
  RowToLibrary(List.Selected);
end;

procedure TCELibManEditorWidget.btnSelfoldOfFilesClick(Sender: TObject);
var
  dir, outdir: string;
begin
  if List.Selected.isNil then
    exit;
  //
  dir := List.Selected.SubItems[0];
  if selectDirectory('folder of static libraries', dir, outdir, True, 0) then
    List.Selected.SubItems[0] := outdir;
  RowToLibrary(List.Selected);
end;

procedure TCELibManEditorWidget.btnSelRootClick(Sender: TObject);
var
  dir, outdir: string;
begin
  if List.Selected.isNil then
    exit;
  //
  dir := List.Selected.SubItems[1];
  if selectDirectory('sources root', dir, outdir, True, 0) then
    List.Selected.SubItems[1] := outdir;
  RowToLibrary(List.Selected);
end;

procedure TCELibManEditorWidget.btnMoveUpClick(Sender: TObject);
var
  i: integer;
begin
  if list.Selected.isNil then
    exit;
  if list.Selected.Index = 0 then
    exit;
  //
  i := list.Selected.Index;
  list.Items.Exchange(i, i - 1);
  LibMan.libraries.Exchange(i, i - 1);
end;

procedure TCELibManEditorWidget.btnMoveDownClick(Sender: TObject);
var
  i: integer;
begin
  if list.Selected.isNil then
    exit;
  if list.Selected.Index = list.Items.Count - 1 then
    exit;
  //
  i := list.Selected.Index;
  list.Items.Exchange(i, i + 1);
  LibMan.libraries.Exchange(i, i + 1);
end;

procedure TCELibManEditorWidget.DoShow;
begin
  inherited;
  dataToGrid;
end;

procedure TCELibManEditorWidget.dataToGrid;
var
  itm: TLibraryItem;
  row: TListItem;
  i: Integer;
begin
  if LibMan.isNil then
    exit;
  List.BeginUpdate;
  List.Clear;
  for i := 0 to LibMan.libraries.Count - 1 do
  begin
    itm := TLibraryItem(LibMan.libraries.Items[i]);
    row := List.Items.Add;
    row.Data:= itm;
    row.Caption := itm.libAlias;
    row.SubItems.Add(itm.libFile);
    row.SubItems.Add(itm.libSourcePath);
    row.SubItems.Add(itm.projectFile);
    row.SubItems.Add(enableStr[itm.enabled]);
  end;
  List.EndUpdate;
end;

procedure TCELibManEditorWidget.RowToLibrary(row: TListItem);
var
  itm: TLibraryItem;
begin
  itm := itemForRow(row);
  if itm.isNil then
    exit;

  itm.libAlias      := row.Caption;
  itm.libFile       := row.SubItems[0];
  itm.libSourcePath := row.SubItems[1];
  itm.projectFile   := row.SubItems[2];
  itm.enabled       := row.SubItems[3] = enableStr[true];
  itm.updateModulesInfo;

  LibMan.updateDCD;
  LibMan.updateCrossDependencies;
end;

function TCELibManEditorWidget.sourceRoot(project: ICECommonProject): string;
var
  i, j: integer;
  mnme: string;
  path: string;
  base: string;
  lst: TStringList;
  srcc: TStringList;
  toks: TLexTokenList;
begin

  // 1 source, same folder
  if project.sourcesCount = 1 then
  begin
    base := project.basePath;
    path := project.sourceAbsolute(0);
    if path.extractFilePath = base then
      exit(base);
  end;

  lst := TStringList.Create;
  srcc := TStringList.Create;
  toks := TLexTokenList.Create;
  lst.Duplicates:= dupIgnore;
  try
    // get module name and store the parent.parent.parent... dir
    for i := 0 to project.sourcesCount-1 do
    begin
      path := project.sourceAbsolute(i);
      if not hasDlangSyntax(path.extractFileExt) then
        continue;
      fModStart := false;
      srcc.LoadFromFile(path);
      lex(srcc.Text, toks, @lexFindToken, [lxoNoComments]);
      mnme := getModuleName(toks);
      toks.Clear;
      for j := 0 to WordCount(mnme, ['.'])-1 do
        path := path.extractFileDir;
      lst.Add(path);
    end;
    if project.sourcesCount = 0 then
      result := ''
    else
      result := lst[0];
    if (project.sourcesCount > 1) and (lst.Count > 1) then
    begin
      lst.Clear;
      for j := 0 to project.sourcesCount-1 do
      begin
        path := project.sourceAbsolute(j);
        if hasDlangSyntax(path.extractFileExt) then
          lst.Add(path);
      end;
      result := commonFolder(lst);
      result := result.extractFileDir;
    end;
  finally
    srcc.Free;
    lst.Free;
    toks.Free;
  end;
end;

procedure TCELibManEditorWidget.lexFindToken(const token: PLexToken; out stop: boolean);
begin
  if (token^.kind = ltkKeyword) and (token^.data = 'module') then
  begin
    fModStart := true;
    exit;
  end;
  if fModStart and (token^.kind = ltkSymbol) and (token^.data = ';') then
  begin
    stop := true;
    fModStart := false;
  end;
end;

end.
