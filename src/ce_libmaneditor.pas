unit ce_libmaneditor;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, ComCtrls, Buttons, LazFileUtils, strutils, fphttpclient, StdCtrls, xfpjson,
  ce_widget, ce_interfaces, ce_nativeproject, ce_dmdwrap, ce_common, ce_dialogs,
  ce_sharedres, process, ce_dubproject, ce_observer, ce_dlang, ce_stringrange,
  ce_libman, ce_projutils;

type

  TDubPackageQueryForm = class(TForm)
  private
    cbb: TComboBox;
    function getText: string;
    procedure getList(sender: TObject);
  public
    class function showAndWait(out value: string): TModalResult; static;
    constructor Create(TheOwner: TComponent); override;
    property text: string read getText;
  end;

  { TCELibManEditorWidget }
  TCELibManEditorWidget = class(TCEWidget, ICEProjectObserver)
    btnOpenProj: TBitBtn;
    btnMoveDown: TBitBtn;
    btnMoveUp: TBitBtn;
    btnReg: TBitBtn;
    btnDubFetch: TBitBtn;
    btnSelFile: TBitBtn;
    btnAddLib: TBitBtn;
    btnRemLib: TBitBtn;
    btnEditAlias: TBitBtn;
    btnSelfoldOfFiles: TBitBtn;
    btnSelRoot: TBitBtn;
    btnSelProj: TBitBtn;
    List: TListView;
    Panel1: TPanel;
    btnEnabled: TSpeedButton;
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
    procedure ListEdited(Sender: TObject; Item: TListItem; var AValue: string);
    procedure ListSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean
      );
  private
    fProj: ICECommonProject;
    fFreeProj: ICECommonProject;
    procedure updateButtonsState;
    procedure projNew(aProject: ICECommonProject);
    procedure projChanged(aProject: ICECommonProject);
    procedure projClosing(aProject: ICECommonProject);
    procedure projFocused(aProject: ICECommonProject);
    procedure projCompiling(aProject: ICECommonProject);
    procedure projCompiled(aProject: ICECommonProject; success: boolean);
    function  itemForRow(row: TListItem): TLibraryItem;
    procedure RowToLibrary(row: TListItem);
    //
    procedure dataToGrid;
  protected
    procedure DoShow; override;
  public
    constructor Create(aOwner: TComponent); override;
  end;

  // determine the root of a library, according to the module names
  function sourceRoot(project: ICECommonProject): string;

implementation
{$R *.lfm}


const
  notav: string = '< n/a >';
  enableStr: array [boolean] of string = ('false','true');

constructor TCELibManEditorWidget.Create(aOwner: TComponent);
begin
  inherited;
  AssignPng(btnMoveDown, 'arrow_down');
  AssignPng(btnMoveUp, 'arrow_up');
  AssignPng(btnAddLib, 'book_add');
  AssignPng(btnRemLib, 'book_delete');
  AssignPng(btnEditAlias, 'book_edit');
  AssignPng(btnSelFile, 'folder_brick');
  AssignPng(btnSelfoldOfFiles, 'bricks');
  AssignPng(btnSelRoot, 'folder_add');
  AssignPng(btnReg, 'book_link');
  AssignPng(btnDubFetch, 'dub');
  AssignPng(btnSelProj, 'script_bricks');
  AssignPng(btnOpenProj, 'book_open');
  AssignPng(btnEnabled, 'book');
end;

procedure TCELibManEditorWidget.updateButtonsState;
begin
  btnReg.Enabled := (fProj <> nil) and (fProj.binaryKind = staticlib) and
    fProj.Filename.fileExists;
  btnOpenProj.Enabled := List.Selected.isNotNil and
    List.Selected.SubItems[2].fileExists;
  if List.Selected.isNotNil and itemForRow(List.Selected).isNotNil and
    itemForRow(List.Selected).enabled then
      AssignPng(btnEnabled, 'book')
  else
    AssignPng(btnEnabled, 'book_grey');
end;

procedure TCELibManEditorWidget.projNew(aProject: ICECommonProject);
begin
  fProj := aProject;
  if not aProject.inGroup then
    fFreeProj := aProject;
end;

procedure TCELibManEditorWidget.projChanged(aProject: ICECommonProject);
begin
  if fProj = nil then exit;
  if fProj <> aProject then
    exit;
  //
  updateButtonsState;
end;

procedure TCELibManEditorWidget.projClosing(aProject: ICECommonProject);
begin
  fProj := nil;
  if aProject = fFreeProj then
    fFreeProj := nil;
  updateButtonsState;
end;

procedure TCELibManEditorWidget.projFocused(aProject: ICECommonProject);
begin
  fProj := aProject;
  if not aProject.inGroup then
    fFreeProj := aProject;
  updateButtonsState;
end;

procedure TCELibManEditorWidget.projCompiling(aProject: ICECommonProject);
begin
end;

procedure TCELibManEditorWidget.projCompiled(aProject: ICECommonProject; success: boolean);
begin
end;

function TCELibManEditorWidget.itemForRow(row: TListItem): TLibraryItem;
begin
  result := TLibraryItem(row.Data);
end;

procedure TCELibManEditorWidget.ListEdited(Sender: TObject; Item: TListItem; var AValue: string);
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

constructor TDubPackageQueryForm.Create(TheOwner: TComponent);
var
  bok: TBitBtn;
  bno: TBitBtn;
  bww: TBitBtn;
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
  AssignPng(bww, 'arrow_update');

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
  AssignPng(bok, 'accept');

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
  AssignPng(bno, 'cancel');
end;

procedure TDubPackageQueryForm.getList(sender: TObject);
var
  pge: string;
  cli: TFPHTTPClient;
begin
  cbb.Items.Clear;
  cli := TFPHTTPClient.Create(nil);
  try
    try
      pge := cli.Get('https://code.dlang.org/');
    except
      pge := '';
    end;
  finally
    cli.Free;
  end;
  // note, also works with regex \"packages\/[a-zA-Z0-9_-]+\"
  with TStringRange.create(pge) do while not empty do
  begin
    if popUntil('"')^.startsWith('"packages/') then
    begin
      popUntil('/')^.popFront;
      cbb.Items.Add(takeUntil('"').yield);
      popUntil('"')^.popFront;
    end
    else popFront;
  end;
end;

function TDubPackageQueryForm.getText: string;
begin
  result := cbb.Text;
end;

class function TDubPackageQueryForm.showAndWait(out value: string): TModalResult;
var
  frm: TDubPackageQueryForm;
begin
  frm := TDubPackageQueryForm.Create(nil);
  result := frm.ShowModal;
  if result = mrOk then
    value := frm.text
  else
    value := '';
  frm.Free;
end;

procedure TCELibManEditorWidget.btnDubFetchClick(Sender: TObject);
var
  dub: TProcess;
  nme: string = '';
  msg: string;
  pth: string;
  dfn: string;
  str: TStringList;
  itf: ICEMessagesDisplay;
  err: integer;
  prj: TCEDubProject;
  upd: boolean = false;
  ovw: boolean = false;
  row: TListItem = nil;
begin
  if TDubPackageQueryForm.showAndWait(nme) <> mrOk then
    exit;
  if List.Items.FindCaption(0, nme, false, false, false).isNotNil then
  begin
    if dlgYesNo(format('a library item with the alias "%s" already exists, do you wish to update it ?',
      [nme])) <> mrYes then exit
    else ovw := true;
  end;
  {$IFDEF WINDOWS}
  pth := GetEnvironmentVariable('APPDATA') + '\dub\packages\' + nme + '-master';
  {$ELSE}
  pth := GetEnvironmentVariable('HOME') + '/.dub/packages/' + nme + '-master';
  {$ENDIF}
  itf := getMessageDisplay;
  if pth.dirExists and not DeleteDirectory(pth, false) then
  begin
    upd := true;
    itf.message('information, the dub package is already fetched and will be upgraded', nil, amcMisc, amkInf);
  end;

  // fetch / updgrade
  dub := TProcess.Create(nil);
  try
    dub.Executable:= 'dub';
    dub.Options:= [poUsePipes, poStderrToOutPut];
    dub.ShowWindow:= swoHIDE;
    if not upd then
    begin
      dub.Parameters.Add('fetch');
      dub.Parameters.Add(nme);
      // fetch project, version handling, pth is hard to set because of semVer suffix.
      // needed: a folder monitor to detect the one created by dub.
      dub.Parameters.Add('--version=~master');
    end else
    begin
      dub.CurrentDirectory := pth;
      dub.Parameters.Add('upgrade');
    end;
    dub.Execute;
    while dub.Running do sleep(10);
    err := dub.ExitStatus;
    str := TStringList.Create;
    try
      processOutputToStrings(dub, str);
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
    itf.message('error, failed to fetch or upgrade the repository', nil, amcMisc, amkErr);
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
    itf.message('error, the DUB description is not found or it has not the JSON format',
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
    while dub.Running do sleep(10);
    err := dub.ExitStatus;
    str := TStringList.Create;
    try
      processOutputToStrings(dub, str);
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
begin
  if List.Selected.isNil then
    exit;
  al := List.Selected.Caption;
  if inputQuery('library alias', '', al) then
    List.Selected.Caption := al;
  RowToLibrary(List.Selected);
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
      if fFreeProj.modified and (dlgFileChangeClose(fFreeProj.filename) = mrCancel) then
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

  LibMan.updateDCD;
end;

function sourceRoot(project: ICECommonProject): string;
var
  i, j: integer;
  name: string;
  fold: string;
  modn: TStringList;
  modf: TStringList;
  toks: TLexTokenList;
  base: string;
begin
  base := project.basePath;

  // 1 source, same folder
  if project.sourcesCount = 1 then
  begin
    name := project.sourceAbsolute(0);
    if name.extractFilePath = base then
      exit(base);
  end;

  modn := TStringList.Create;
  modf := TStringList.Create;
  toks := TLexTokenList.Create;
  try
    // get module name and store the parent.parent.parent... dir
    for i := 0 to project.sourcesCount-1 do
    begin
      fold := project.sourceAbsolute(i);
      modf.LoadFromFile(fold);
      lex(modf.Text, toks);
      name := getModuleName(toks);
      for j := 0 to WordCount(name, ['.'])-1 do
        fold := extractFileDir(fold);
      modn.Add(fold);
      toks.Clear;
    end;
    result := modn[0];
    // no error possible if 1 module
    if project.sourcesCount > 1 then
    begin
      for i := 1 to modn.Count-1 do
      begin
        // expect same folder
        if modn[i] = modn[i-1] then
          continue;
        // if not use common directory.
        modf.Clear;
        for j := 0 to project.sourcesCount-1 do
          modf.Add(project.sourceAbsolute(j));
        result := commonFolder(modf);
        result := result.extractFileDir;
        break;
      end;
    end;
  finally
    modf.Free;
    modn.Free;
    toks.Free;
  end;
end;

end.
