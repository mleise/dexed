unit ce_compilers;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls,
  ce_dcd, ce_common, ce_interfaces, ce_observer, ce_writableComponent,
  ce_dialogs;

type

  //TODO-cCompilerPaths: add a way to relaunch DCD after compiler change

  TCompilersPaths = class(TWritableLfmTextComponent)
  strict private
    fDefaultCompiler: DCompiler;
    fDmdExeName: string;
    fDmdRuntimePath: string;
    fDmdPhobosPath: string;
    fGdcExeName: string;
    fGdcRuntimePath: string;
    fGdcPhobosPath: string;
    fLdcExeName: string;
    fLdcRuntimePath: string;
    fLdcPhobosPath: string;
    fUser1ExeName: string;
    fUser1RuntimePath: string;
    fUser1PhobosPath: string;
    fUser2ExeName: string;
    fUser2RuntimePath: string;
    fUser2PhobosPath: string;
  published
    property defaultCompiler: DCompiler read fDefaultCompiler write fDefaultCompiler;
    property DmdExeName: string read fDmdExeName write fDmdExeName;
    property DmdRuntimePath: string read fDmdRuntimePath write fDmdRuntimePath;
    property DmdPhobosPath: string read fDmdPhobosPath write fDmdPhobosPath;
    property GdcExeName: string read fGdcExeName write fGdcExeName;
    property GdcRuntimePath: string read fGdcRuntimePath write fGdcRuntimePath;
    property GdcPhobosPath: string read fGdcPhobosPath write fGdcPhobosPath;
    property LdcExeName: string read fLdcExeName write fLdcExeName;
    property LdcRuntimePath: string read fLdcRuntimePath write fLdcRuntimePath;
    property LdcPhobosPath: string read fLdcPhobosPath write fLdcPhobosPath;
    property User1ExeName: string read fUser1ExeName write fUser1ExeName;
    property User1RuntimePath: string read fUser1RuntimePath write fUser1RuntimePath;
    property User1PhobosPath: string read fUser1PhobosPath write fUser1PhobosPath;
    property User2ExeName: string read fUser2ExeName write fUser2ExeName;
    property User2RuntimePath: string read fUser2RuntimePath write fUser2RuntimePath;
    property User2PhobosPath: string read fUser2PhobosPath write fUser2PhobosPath;
  public
    procedure assign(source: TPersistent); override;
  end;

  TForm1 = class(TForm, ICEEditableOptions, ICECompilerSelector)
    selDefault: TComboBox;
    selDMDrt: TDirectoryEdit;
    selUSER2std: TDirectoryEdit;
    selDMDstd: TDirectoryEdit;
    selGDCrt: TDirectoryEdit;
    selGDCstd: TDirectoryEdit;
    selLDCrt: TDirectoryEdit;
    selLDCstd: TDirectoryEdit;
    selUSER1rt: TDirectoryEdit;
    selUSER1std: TDirectoryEdit;
    selUSER2rt: TDirectoryEdit;
    selDMDexe: TFileNameEdit;
    selGDCexe: TFileNameEdit;
    selLDCexe: TFileNameEdit;
    selUSER1exe: TFileNameEdit;
    selUSER2exe: TFileNameEdit;
    grpDMD: TGroupBox;
    grpGDC: TGroupBox;
    grpLDC: TGroupBox;
    grpUSER1: TGroupBox;
    grpUSER2: TGroupBox;
    GroupBox6: TGroupBox;
    ScrollBox1: TScrollBox;
    StaticText1: TStaticText;
    StaticText10: TStaticText;
    StaticText11: TStaticText;
    StaticText12: TStaticText;
    StaticText13: TStaticText;
    StaticText14: TStaticText;
    StaticText15: TStaticText;
    StaticText2: TStaticText;
    StaticText3: TStaticText;
    StaticText4: TStaticText;
    StaticText5: TStaticText;
    StaticText6: TStaticText;
    StaticText7: TStaticText;
    StaticText8: TStaticText;
    StaticText9: TStaticText;
  strict private
    fPaths: TCompilersPaths;
    fPathsBackup: TCompilersPaths;
    procedure selectedExe(sender: TObject; var value: string);
    procedure selectedRt(sender: TObject; var value: string);
    procedure selectedStd(sender: TObject; var value: string);
    procedure selectedDefault(sender: TObject);
    procedure autoDetectDMD;
    procedure autoDetectGDC;
    procedure autoDetectLDC;
    procedure dataToGui;
    //
    function optionedWantCategory(): string;
    function optionedWantEditorKind: TOptionEditorKind;
    function optionedWantContainer: TPersistent;
    procedure optionedEvent(event: TOptionEditorEvent);
    function optionedOptionsModified: boolean;
    //
    function singleServiceName: string;
    function isCompilerValid(value: DCompiler): boolean;
    function getCompilerPath(value: DCompiler): string;
    procedure getCompilerImports(value: DCompiler; paths: TStrings);
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
  end;

implementation
{$R *.lfm}

var
  Form1: TForm1;

const
  optFname = 'compilerspaths.txt';

{$REGION Standard Object/COmponents things -------------------------------------}
constructor TForm1.create(aOwner: TComponent);
var
  fname: string;
  imprt: TStringList;
begin
  inherited;
  fPaths:= TCompilersPaths.Create(self);
  fPathsBackup:= TCompilersPaths.Create(self);

  fname := getCoeditDocPath + optFname;
  if fname.fileExists then
    fPaths.loadFromFile(fname)
  else
  begin
    autoDetectDMD;
    autoDetectGDC;
    autoDetectLDC;
  end;
  fPathsBackup.Assign(fPaths);
  dataToGui;

  imprt := TStringList.Create;
  try
    getCompilerImports(fPaths.defaultCompiler, imprt);
    DcdWrapper.addImportFolders(imprt);
  finally
    imprt.free;
  end;

  selDMDexe.OnAcceptFileName:= @selectedExe;
  selGDCexe.OnAcceptFileName:= @selectedExe;
  selLDCexe.OnAcceptFileName:= @selectedExe;
  selUSER1exe.OnAcceptFileName:= @selectedExe;
  selUSER2exe.OnAcceptFileName:= @selectedExe;

  selDMDrt.OnAcceptDirectory:= @selectedRt;
  selGDCrt.OnAcceptDirectory:= @selectedRt;
  selLDCrt.OnAcceptDirectory:= @selectedRt;
  selUSER1rt.OnAcceptDirectory:= @selectedRt;
  selUSER2rt.OnAcceptDirectory:= @selectedRt;

  selDMDstd.OnAcceptDirectory:= @selectedStd;
  selGDCstd.OnAcceptDirectory:= @selectedStd;
  selLDCstd.OnAcceptDirectory:= @selectedStd;
  selUSER1std.OnAcceptDirectory:= @selectedStd;
  selUSER2std.OnAcceptDirectory:= @selectedStd;

  selDefault.OnSelect:= @selectedDefault;

  EntitiesConnector.addSingleService(self);
  EntitiesConnector.addObserver(self);
end;

destructor TForm1.destroy;
begin
  fPaths.saveToFile(getCoeditDocPath + optFname);
  EntitiesConnector.removeObserver(self);
  inherited;
end;

procedure TCompilersPaths.assign(source: TPersistent);
var
  src: TCompilersPaths;
begin
  if source is TCompilersPaths then
  begin
    src := TCompilersPaths(source);
    fDefaultCompiler  := src.fDefaultCompiler;
    fDmdExeName       := src.fDmdExeName;
    fDmdRuntimePath   := src.fDmdRuntimePath;
    fDmdPhobosPath    := src.fDmdPhobosPath;
    fGdcExeName       := src.fGdcExeName;
    fGdcRuntimePath   := src.fGdcRuntimePath;
    fGdcPhobosPath    := src.fGdcPhobosPath;
    fLdcExeName       := src.fLdcExeName;
    fLdcRuntimePath   := src.fLdcRuntimePath;
    fLdcPhobosPath    := src.fLdcPhobosPath;
    fUser1ExeName     := src.fUser1ExeName;
    fUser1RuntimePath := src.fUser1RuntimePath;
    fUser1PhobosPath  := src.fUser1PhobosPath;
    fUser2ExeName     := src.fUser2ExeName;
    fUser2RuntimePath := src.fUser2RuntimePath;
    fUser2PhobosPath  := src.fUser2PhobosPath;
  end
  else inherited;
end;
{$ENDREGION}

{$REGION ICEEditableOptions ----------------------------------------------------}
function TForm1.optionedWantCategory(): string;
begin
  exit('Compilers paths');
end;

function TForm1.optionedWantEditorKind: TOptionEditorKind;
begin
  exit(oekForm);
end;

function TForm1.optionedWantContainer: TPersistent;
begin
  fPathsBackup.assign(fPaths);
  exit(self);
end;

procedure TForm1.optionedEvent(event: TOptionEditorEvent);
begin
  case event of
    oeeAccept: fPathsBackup.assign(fPaths);
    oeeCancel: fPaths.assign(fPathsBackup);
  end;
end;

function TForm1.optionedOptionsModified: boolean;
begin
  //TODO-cCompilerPaths: add setters to detect if a TCompilersPaths is modified
  exit(false);
end;
{$ENDREGION}

{$REGION ICECompilerSelector ---------------------------------------------------}
function TForm1.singleServiceName: string;
begin
  exit('ICECompilerSelector');
end;

function TForm1.isCompilerValid(value: DCompiler): boolean;
begin
  result := false;
  with fPaths do case value of
    DCompiler.dmd: exit(DmdExeName.fileExists);
    DCompiler.gdc: exit(GdcExeName.fileExists);
    DCompiler.gdmd: exit(exeFullName('gdmd' + exeExt).fileExists);
    DCompiler.ldc: exit(LdcExeName.fileExists);
    DCompiler.ldmd: exit(exeFullName('ldmd2' + exeExt).fileExists);
    DCompiler.user1: exit(User1ExeName.fileExists);
    DCompiler.user2: exit(User2ExeName.fileExists);
  end;
end;

function TForm1.getCompilerPath(value: DCompiler): string;
begin
  result := '';
  with fPaths do case value of
    DCompiler.dmd: exit(DmdExeName);
    DCompiler.gdc: exit(GdcExeName);
    DCompiler.gdmd: exit(exeFullName('gdmd' + exeExt));
    DCompiler.ldc: exit(LdcExeName);
    DCompiler.ldmd: exit(exeFullName('ldmd2' + exeExt));
    DCompiler.user1: exit(User1ExeName);
    DCompiler.user2: exit(User2ExeName);
  end;
end;

procedure TForm1.getCompilerImports(value: DCompiler; paths: TStrings);
  procedure tryAdd(const pth: string);
  begin
    if pth.isNotEmpty then
      paths.Add(pth);
  end;
begin
  with fPaths do case value of
    DCompiler.dmd: begin tryAdd(DmdRuntimePath); tryAdd(DmdPhobosPath); end;
    DCompiler.gdc, DCompiler.gdmd:
      begin tryAdd(GdcRuntimePath); tryAdd(GdcPhobosPath); end;
    DCompiler.ldc, DCompiler.ldmd:
      begin tryAdd(LdcRuntimePath); tryAdd(LdcPhobosPath); end;
    DCompiler.user1:
      begin tryAdd(User1RuntimePath); tryAdd(User1PhobosPath); end;
    DCompiler.user2:
      begin tryAdd(User2RuntimePath); tryAdd(User2PhobosPath); end;
  end;
end;
{$ENDREGION}

{$REGION Compilers paths things ------------------------------------------------}
procedure TForm1.dataToGui;
begin
  with fPaths do
  begin
    selDMDexe.FileName  := DmdExeName;
    selDMDrt.Directory  := DmdRuntimePath;
    selDMDstd.Directory := DmdPhobosPath;

    selGDCexe.FileName  := GdcExeName;
    selGDCrt.Directory  := GdcRuntimePath;
    selGDCstd.Directory := GdcPhobosPath;

    selLDCexe.FileName  := LdcExeName;
    selLDCrt.Directory  := LdcRuntimePath;
    selLDCstd.Directory := LdcPhobosPath;

    selUSER1exe.FileName  := User1ExeName;
    selUSER1rt.Directory  := User1RuntimePath;
    selUSER1std.Directory := User1PhobosPath;

    selUSER2exe.FileName  := User2ExeName;
    selUSER2rt.Directory  := User2RuntimePath;
    selUSER2std.Directory := User2PhobosPath;

    selDefault.ItemIndex := integer(defaultCompiler);
  end;
end;

procedure TForm1.selectedExe(sender: TObject; var value: string);
var
  ctrl: TWinControl;
begin
  ctrl := TWinControl(sender);
  if ctrl.Parent = grpDMD then
    fPaths.DmdExeName:=value
  else if ctrl.Parent = grpGDC then
    fPaths.GDCExeName:=value
  else if ctrl.Parent = grpLDC then
    fPaths.LdcExeName:=value
  else if ctrl.Parent = grpUSER1 then
    fPaths.User1ExeName:=value
  else if ctrl.Parent = grpUSER2 then
    fPaths.User2ExeName:=value;
end;

procedure TForm1.selectedRt(sender: TObject; var value: string);
var
  ctrl: TWinControl;
begin
  ctrl := TWinControl(sender);
  if ctrl.Parent = grpDMD then
    fPaths.DmdRuntimePath:=value
  else if ctrl.Parent = grpGDC then
    fPaths.GDCRuntimePath:=value
  else if ctrl.Parent = grpLDC then
    fPaths.LdcRuntimePath:=value
  else if ctrl.Parent = grpUSER1 then
    fPaths.User1RuntimePath:=value
  else if ctrl.Parent = grpUSER2 then
    fPaths.User2RuntimePath:=value;
end;

procedure TForm1.selectedStd(sender: TObject; var value: string);
var
  ctrl: TWinControl;
begin
  ctrl := TWinControl(sender);
  if ctrl.Parent = grpDMD then
    fPaths.DmdPhobosPath:=value
  else if ctrl.Parent = grpGDC then
    fPaths.GDCPhobosPath:=value
  else if ctrl.Parent = grpLDC then
    fPaths.LdcPhobosPath:=value
  else if ctrl.Parent = grpUSER1 then
    fPaths.User1PhobosPath:=value
  else if ctrl.Parent = grpUSER2 then
    fPaths.User2PhobosPath:=value;
end;

procedure TForm1.selectedDefault(sender: TObject);
begin
  fPaths.defaultCompiler:= DCompiler(selDefault.ItemIndex);
  dlgOkInfo('A restart might be necessary so that DCD caches a different version'
    + 'of the runtime and the standard library.');
end;

procedure TForm1.autoDetectDMD;
{$IFDEF WINDOWS}
var
  path: string;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  path := exeFullName('dmd' + exeExt);
  if path.dirExists then
  begin
    fPaths.DmdExeName:= path;
    path := path.extractFileDir.extractFileDir.extractFileDir;
    if (path + '\src\drunime\import').dirExists then
      fPaths.DmdRuntimePath := path + '\src\drunime\import';
    if (path + '\src\phobos').dirExists then
      fPaths.DmdRuntimePath := path + '\src\phobos';
  end;
  {$ENDIF}
  {$IFDEF LINUX}
  if '/usr/bin/dmd'.fileExists then
    fPaths.DmdExeName:='/usr/bin/dmd';
  if '/usr/include/dmd/phobos'.dirExists then
    fPaths.DmdPhobosPath:='/usr/include/dmd/phobos';
  if '/usr/include/dmd/druntime/import'.dirExists then
    fPaths.DmdRuntimePath:='/usr/include/dmd/druntime/import';
  {$ENDIF}
  {$IFDEF DARWIN}
  if '/usr/local/bin/dmd'.fileExists then
    fPaths.DmdExeName:='/usr/local/bin/dmd';
  if '/Library/D/dmd/src/phobos'.dirExists then
    fPaths.DmdPhobosPath:='/Library/D/dmd/src/phobos';
  if '/Library/D/dmd/src/druntime/import' then
    fPaths.DmdRuntimePath:='/Library/D/dmd/src/druntime/import';
  {$ENDIF}
end;

procedure TForm1.autoDetectGDC;
begin
  //TODO-cCompilerPaths: detect GDC
end;

procedure TForm1.autoDetectLDC;
begin
  //TODO-cCompilerPaths: detect LDC
end;
{$ENDREGION}

initialization
  Form1 := TForm1.create(nil);
finalization
  Form1.free;
end.

