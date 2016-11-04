unit ce_main;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, LazFileUtils, SynEditKeyCmds, SynHighlighterLFM, Forms,
  StdCtrls, AnchorDocking, AnchorDockStorage, AnchorDockOptionsDlg, Controls,
  Graphics, strutils, Dialogs, Menus, ActnList, ExtCtrls, process,
  {$IFDEF WINDOWS}Windows, {$ENDIF} XMLPropStorage, SynExportHTML, fphttpclient,
  xfpjson, xjsonparser, xjsonscanner,
  ce_common, ce_dmdwrap, ce_ceproject, ce_synmemo, ce_writableComponent,
  ce_widget, ce_messages, ce_interfaces, ce_editor, ce_projinspect, ce_ceprojeditor,
  ce_search, ce_miniexplorer, ce_libman, ce_libmaneditor, ce_todolist, ce_observer,
  ce_toolseditor, ce_procinput, ce_optionseditor, ce_symlist, ce_mru, ce_processes,
  ce_infos, ce_dubproject, ce_dialogs, ce_dubprojeditor,{$IFDEF UNIX} ce_gdb,{$ENDIF}
  ce_dfmt, ce_lcldragdrop, ce_projgroup, ce_projutils, ce_stringrange;

type

  TCEApplicationOptions = class;

  TAnchorDockSplitterEx = class(TAnchorDockSplitter)
  public
    property OnMouseWheel;
  end;

  TAsynWait = (awNo, awYes, awCustom);

  TRunnableToFolderCondition = (
    ifInProject,  // runnable src is part of the project
    ifNotSaved,   // runnable src is an unsaved module (tmp_XXXXX)
    ifSaved       // runnable src not in project but saved not in temp dir
  );

  TCERunnableOptions = class(TWritableLfmTextComponent)
  private
    fCompiler: TCECompiler;
    fDetectMain: boolean;
    fDetectLibraries: boolean;
    fOutputFolder: TCEPathname;
    fAlwaysToFolder: boolean;
    fStaticSwitches: TStringList;
    procedure setOutputFolder(const value: TCEPathname);
    procedure setStaticSwitches(value: TStringList);
    procedure setCompiler(value: TCECompiler);
  protected
    procedure afterLoad; override;
  published
    property alwaysToFolder: boolean read fAlwaysToFolder write fAlwaysToFolder;
    property compiler: TCECompiler read fCompiler write setCompiler;
    property detectMain: boolean read fDetectMain write fDetectMain;
    property detectLibraries: boolean read fDetectLibraries write fDetectLibraries;
    property outputFolder: TCEPathname read fOutputFolder write setOutputFolder;
    property staticSwitches: TStringList read fStaticSwitches write setStaticSwitches;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    procedure assign(source: TPersistent); override;
    procedure setDefaultSwitches;
    procedure sanitizeSwitches;
  end;

  TCEEditableRunnableOptions = class(TCERunnableOptions, ICEEditableOptions)
  private
    fBackup: TCERunnableOptions;
    function optionedWantCategory(): string;
    function optionedWantEditorKind: TOptionEditorKind;
    function optionedWantContainer: TPersistent;
    procedure optionedEvent(event: TOptionEditorEvent);
    function optionedOptionsModified: boolean;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
  end;

  { TCEMainForm }
  TCEMainForm = class(TForm, ICEDocumentObserver, ICEEditableShortCut, ICEProjectObserver)
    actFileCompAndRun: TAction;
    actFileSaveAll: TAction;
    actFileClose: TAction;
    actFileAddToProj: TAction;
    actFileNewRun: TAction;
    actFileNew: TAction;
    actFileOpen: TAction;
    actFileSaveAs: TAction;
    actFileSave: TAction;
    actFileCompAndRunWithArgs: TAction;
    actEdFind: TAction;
    actEdFindNext: TAction;
    actFileOpenContFold: TAction;
    actFileHtmlExport: TAction;
    actFileUnittest: TAction;
    actFileCompileAndRunOut: TAction;
    actFileSaveCopyAs: TAction;
    actFileCompile: TAction;
    actFileRun: TAction;
    actFileDscanner: TAction;
    actFileRunOut: TAction;
    actFileRunDub: TAction;
    actFileRunDubOut: TAction;
    actFileNewDubScript: TAction;
    actProjGroupCompileCustomSync: TAction;
    actProjGroupClose: TAction;
    actProjGroupCompileSync: TAction;
    actProjGroupCompile: TAction;
    actProjSelUngrouped: TAction;
    actProjAddToGroup: TAction;
    actProjNewGroup: TAction;
    actProjOpenGroup: TAction;
    actProjSaveGroup: TAction;
    actProjSaveGroupAs: TAction;
    actProjNewDubJson: TAction;
    actProjNewNative: TAction;
    actSetRunnableSw: TAction;
    actLayoutSave: TAction;
    actProjOpenContFold: TAction;
    actProjOptView: TAction;
    actProjSource: TAction;
    actProjRun: TAction;
    actProjRunWithArgs: TAction;
    actProjCompile: TAction;
    actProjCompileAndRun: TAction;
    actProjCompAndRunWithArgs: TAction;
    actProjClose: TAction;
    actProjOpts: TAction;
    actProjOpen: TAction;
    actProjSave: TAction;
    actProjSaveAs: TAction;
    actEdMacPlay: TAction;
    actEdMacStartStop: TAction;
    actEdCut: TAction;
    actEdRedo: TAction;
    actEdUndo: TAction;
    actEdPaste: TAction;
    actEdCopy: TAction;
    actEdIndent: TAction;
    actEdUnIndent: TAction;
    Actions: TActionList;
    ApplicationProperties1: TApplicationProperties;
    imgList: TImageList;
    mainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem100: TMenuItem;
    MenuItem101: TMenuItem;
    MenuItem102: TMenuItem;
    MenuItem103: TMenuItem;
    MenuItem104: TMenuItem;
    MenuItem105: TMenuItem;
    mnuItemMruGroup: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem32: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem36: TMenuItem;
    MenuItem37: TMenuItem;
    MenuItem38: TMenuItem;
    MenuItem39: TMenuItem;
    MenuItem40: TMenuItem;
    MenuItem41: TMenuItem;
    MenuItem42: TMenuItem;
    MenuItem43: TMenuItem;
    MenuItem44: TMenuItem;
    MenuItem45: TMenuItem;
    MenuItem46: TMenuItem;
    MenuItem47: TMenuItem;
    MenuItem48: TMenuItem;
    MenuItem49: TMenuItem;
    MenuItem50: TMenuItem;
    MenuItem51: TMenuItem;
    MenuItem52: TMenuItem;
    MenuItem53: TMenuItem;
    MenuItem54: TMenuItem;
    MenuItem55: TMenuItem;
    MenuItem56: TMenuItem;
    MenuItem57: TMenuItem;
    MenuItem58: TMenuItem;
    MenuItem59: TMenuItem;
    MenuItem60: TMenuItem;
    MenuItem61: TMenuItem;
    MenuItem62: TMenuItem;
    MenuItem63: TMenuItem;
    MenuItem64: TMenuItem;
    MenuItem65: TMenuItem;
    MenuItem66: TMenuItem;
    MenuItem67: TMenuItem;
    MenuItem68: TMenuItem;
    MenuItem69: TMenuItem;
    MenuItem70: TMenuItem;
    MenuItem71: TMenuItem;
    MenuItem72: TMenuItem;
    MenuItem73: TMenuItem;
    MenuItem74: TMenuItem;
    MenuItem75: TMenuItem;
    MenuItem76: TMenuItem;
    MenuItem77: TMenuItem;
    MenuItem78: TMenuItem;
    MenuItem79: TMenuItem;
    MenuItem80: TMenuItem;
    MenuItem81: TMenuItem;
    MenuItem82: TMenuItem;
    MenuItem83: TMenuItem;
    MenuItem84: TMenuItem;
    MenuItem85: TMenuItem;
    MenuItem86: TMenuItem;
    MenuItem87: TMenuItem;
    MenuItem88: TMenuItem;
    MenuItem89: TMenuItem;
    MenuItem90: TMenuItem;
    MenuItem91: TMenuItem;
    MenuItem92: TMenuItem;
    MenuItem93: TMenuItem;
    MenuItem94: TMenuItem;
    MenuItem95: TMenuItem;
    MenuItem96: TMenuItem;
    MenuItem97: TMenuItem;
    MenuItem98: TMenuItem;
    MenuItem99: TMenuItem;
    mnuLayout: TMenuItem;
    mnuItemMruFile: TMenuItem;
    mnuItemMruProj: TMenuItem;
    mnuItemWin: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    procedure actFileCompileExecute(Sender: TObject);
    procedure actFileDscannerExecute(Sender: TObject);
    procedure actFileNewDubScriptExecute(Sender: TObject);
    procedure actFileRunDubExecute(Sender: TObject);
    procedure actFileRunDubOutExecute(Sender: TObject);
    procedure actFileRunExecute(Sender: TObject);
    procedure actFileRunOutExecute(Sender: TObject);
    procedure actFileSaveCopyAsExecute(Sender: TObject);
    procedure actNewGroupExecute(Sender: TObject);
    procedure actProjAddToGroupExecute(Sender: TObject);
    procedure actProjGroupCompileCustomSyncExecute(Sender: TObject);
    procedure actProjGroupCompileExecute(Sender: TObject);
    procedure actProjGroupCompileSyncExecute(Sender: TObject);
    procedure actProjNewDubJsonExecute(Sender: TObject);
    procedure actProjNewGroupExecute(Sender: TObject);
    procedure actProjNewNativeExecute(Sender: TObject);
    procedure actProjOpenGroupExecute(Sender: TObject);
    procedure actProjSaveGroupAsExecute(Sender: TObject);
    procedure actProjSaveGroupExecute(Sender: TObject);
    procedure actProjSelUngroupedExecute(Sender: TObject);
    procedure actSetRunnableSwExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure MenuItem77Click(Sender: TObject);
    procedure updateDocumentBasedAction(sender: TObject);
    procedure updateProjectBasedAction(sender: TObject);
    procedure updateDocEditBasedAction(sender: TObject);
    procedure actFileCompileAndRunOutExecute(Sender: TObject);
    procedure actEdFindExecute(Sender: TObject);
    procedure actEdFindNextExecute(Sender: TObject);
    procedure actFileAddToProjExecute(Sender: TObject);
    procedure actFileCloseExecute(Sender: TObject);
    procedure actFileCompAndRunExecute(Sender: TObject);
    procedure actFileCompAndRunWithArgsExecute(Sender: TObject);
    procedure actFileHtmlExportExecute(Sender: TObject);
    procedure actFileOpenContFoldExecute(Sender: TObject);
    procedure actFileSaveAllExecute(Sender: TObject);
    procedure actEdIndentExecute(Sender: TObject);
    procedure actFileUnittestExecute(Sender: TObject);
    procedure actLayoutSaveExecute(Sender: TObject);
    procedure actProjCompAndRunWithArgsExecute(Sender: TObject);
    procedure actProjCompileAndRunExecute(Sender: TObject);
    procedure actProjCompileExecute(Sender: TObject);
    procedure actEdCopyExecute(Sender: TObject);
    procedure actEdCutExecute(Sender: TObject);
    procedure ActionsUpdate(AAction: TBasicAction; var Handled: Boolean);
    procedure actEdMacPlayExecute(Sender: TObject);
    procedure actEdMacStartStopExecute(Sender: TObject);
    procedure actFileNewExecute(Sender: TObject);
    procedure actFileNewRunExecute(Sender: TObject);
    procedure actFileOpenExecute(Sender: TObject);
    procedure actProjOpenContFoldExecute(Sender: TObject);
    procedure actProjOpenExecute(Sender: TObject);
    procedure actEdPasteExecute(Sender: TObject);
    procedure actProjCloseExecute(Sender: TObject);
    procedure actProjOptsExecute(Sender: TObject);
    procedure actEdRedoExecute(Sender: TObject);
    procedure actFileSaveAsExecute(Sender: TObject);
    procedure actFileSaveExecute(Sender: TObject);
    procedure actProjOptViewExecute(Sender: TObject);
    procedure actProjRunExecute(Sender: TObject);
    procedure actProjRunWithArgsExecute(Sender: TObject);
    procedure actProjSaveAsExecute(Sender: TObject);
    procedure actProjSaveExecute(Sender: TObject);
    procedure actEdUndoExecute(Sender: TObject);
    procedure actProjSourceExecute(Sender: TObject);
    procedure actEdUnIndentExecute(Sender: TObject);
    procedure ApplicationProperties1Exception(Sender: TObject; E: Exception);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormDropFiles(Sender: TObject; const fnames: array of string);

  protected

    procedure DoFirstShow; override;

  private

    fRunnablesOptions: TCEEditableRunnableOptions;
    fSymStringExpander: ICESymStringExpander;
    fProjectGroup: ICEProjectGroup;
    fCovModUt: boolean;
    fDscanUnittests: boolean;
    fDoc: TCESynMemo;
    fFirstTimeCoedit: boolean;
    fActionHandler: TCEActionProviderSubject;
    fMultidoc: ICEMultiDocHandler;
    fScCollectCount: Integer;
    fUpdateCount: NativeInt;
    fProject: ICECommonProject;
    fFreeProj: ICECommonProject;
    fDubProject: TCEDubProject;
    fNativeProject: TCENativeProject;
    fProjMru: TCEMRUProjectList;
    fFileMru: TCEMRUDocumentList;
    fPrjGrpMru: TCEMRUProjectsGroupList;
    fWidgList: TCEWidgetList;
    fMesgWidg: TCEMessagesWidget;
    fEditWidg: TCEEditorWidget;
    fProjWidg: TCEProjectInspectWidget;
    fPrjCfWidg: TCEProjectConfigurationWidget;
    fFindWidg:  TCESearchWidget;
    fExplWidg: TCEMiniExplorerWidget;
    fLibMWidg: TCELibManEditorWidget;
    fTlsEdWidg: TCEToolsEditorWidget;
    fPrInpWidg: TCEProcInputWidget;
    fTodolWidg: TCETodoListWidget;
    fOptEdWidg: TCEOptionEditorWidget;
    fSymlWidg: TCESymbolListWidget;
    fInfoWidg: TCEInfoWidget;
    fDubProjWidg: TCEDubProjectEditorWidget;
    fPrjGrpWidg: TCEProjectGroupWidget;
    {$IFDEF UNIX}
    fGdbWidg: TCEGdbWidget;
    {$ENDIF}
    fDfmtWidg:  TCEDfmtWidget;
    fCompStart: TDateTime;

    fRunProjAfterCompArg: boolean;
    fRunProjAfterCompile: boolean;
    fIsCompilingGroup: boolean;
    fGroupCompilationCnt: integer;
    fProjFromCommandLine: boolean;
    fInitialized: boolean;
    fRunProc: TCEProcess;
    fMsgs: ICEMessagesDisplay;
    fMainMenuSubj: TCEMainMenuSubject;
    fAppliOpts: TCEApplicationOptions;
    fProjActionsLock: boolean;
    procedure updateMainMenuProviders;
    procedure updateFloatingWidgetOnTop(onTop: boolean);
    procedure widgetDockingChanged(sender: TCEWidget; newState: TWidgetDockingState);

    // action provider handling;
    procedure clearActProviderEntries;
    procedure collectedActProviderEntries;

    // ICEDocumentObserver
    procedure docNew(document: TCESynMemo);
    procedure docClosing(document: TCESynMemo);
    procedure docFocused(document: TCESynMemo);
    procedure docChanged(document: TCESynMemo);

    // ICEProjectObserver
    procedure projNew(project: ICECommonProject);
    procedure projChanged(project: ICECommonProject);
    procedure projClosing(project: ICECommonProject);
    procedure projFocused(project: ICECommonProject);
    procedure projCompiling(project: ICECommonProject);
    procedure projCompiled(project: ICECommonProject; success: boolean);

    // ICEEditableShortcut
    function scedWantFirst: boolean;
    function scedWantNext(out category, identifier: string; out aShortcut: TShortcut): boolean;
    procedure scedSendItem(const category, identifier: string; aShortcut: TShortcut);
    procedure scedSendDone;

    //Init - Fina
    procedure getCMdParams;
    procedure InitMRUs;
    procedure InitWidgets;
    procedure InitDocking;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure LoadDocking;
    procedure SaveDocking;
    procedure LoadLastDocsAndProj;
    procedure SaveLastDocsAndProj;
    procedure FreeRunnableProc;

    // widget interfaces subroutines
    procedure updateWidgetMenuEntry(sender: TObject);
    procedure widgetShowFromAction(sender: TObject);

    // run & exec sub routines
    function runnableExename: string;
    procedure asyncprocOutput(sender: TObject);
    procedure asyncprocTerminate(sender: TObject);
    procedure unittestDone(Sender: TObject);
    function  compileRunnable(unittest: boolean = false): boolean;
    procedure executeRunnable(unittest: boolean = false; redirect: boolean = true;
      const runArgs: string = '');
    procedure runFile(outside: boolean);
    procedure dubFile(outside: boolean);

    // file sub routines
    procedure newFile;
    procedure saveFile(document: TCESynMemo);
    procedure openFile(const fname: string);

    // project sub routines
    procedure saveProjSource(const document: TCESynMemo);
    procedure newNativeProj;
    procedure newDubProj;
    procedure saveProj;
    procedure saveProjAs(const fname: string);
    procedure openProj(const fname: string);
    function closeProj: boolean;
    procedure showProjTitle;
    function  checkProjectLock(message: boolean = true): boolean;
    procedure compileGroup(async: TAsynWait);

    // mru
    procedure mruChange(Sender: TObject);
    procedure mruFileItemClick(Sender: TObject);
    procedure mruProjItemClick(Sender: TObject);
    procedure mruProjGroupItemClick(Sender: TObject);
    procedure mruClearClick(Sender: TObject);

    // layout
    procedure setSplitterWheelEvent;
    procedure DockSplitterMw(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure LockTopWindow(Sender: TObject; var NewSize: Integer; var Accept: Boolean);
    procedure layoutMnuItemClick(sender: TObject);
    procedure layoutLoadFromFile(const fname: string);
    procedure layoutSaveToFile(const fname: string);
    procedure layoutUpdateMenu;

  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    procedure UpdateDockCaption(Exclude: TControl = nil); override;
  end;

  TCEPersistentMainShortcuts = class(TWritableLfmTextComponent)
  private
    fCol: TCollection;
    procedure setCol(value: TCollection);
  published
    property shortcut: TCollection read fCol write setCol;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    //
    procedure assign(source: TPersistent); override;
    procedure assignTo(target: TPersistent); override;
  end;

  TCEPersistentMainMrus = class(TWritableLfmTextComponent)
  private
    fFileMruPt: TCEMRUFileList;
    fProjMruPt: TCEMRUFileList;
    fPrjGrpMruPt: TCEMRUFileList;
    procedure setProjMru(value: TCEMRUFileList);
    procedure setFileMru(value: TCEMRUFileList);
    procedure setProjectsGroupMru(value: TCEMRUFileList);
  published
    property mostRecentFiles: TCEMRUFileList read fFileMruPt write setFileMru;
    property mostRecentprojects: TCEMRUFileList read fProjMruPt write setProjMru;
    property mostRecentProjectsGroups: TCEMRUFileList read fPrjGrpMruPt write setProjectsGroupMru;
  public
    procedure setTargets(projs: TCEMRUFileList; files: TCEMRUFileList; group: TCEMRUFileList);
  end;

  TCELastDocsAndProjs = class(TWritableLfmTextComponent)
  private
    fDocuments: TStringList;
    fProject: string;
    fDocIndex: integer;
    fProjectGroup: string;
    fProjectIndex: integer;
    procedure setDocuments(value: TStringList);
  protected
    procedure beforeSave; override;
    procedure afterLoad; override;
  published
    property documentIndex: integer read fDocIndex write fDocIndex;
    property documents: TStringList read fDocuments write setDocuments;
    property project: string read fProject write fProject;
    property projectGroup: string read fProjectGroup write fProjectGroup;
    property projectIndex: integer read fProjectIndex write fProjectIndex;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    procedure Assign(source: TPersistent); override;
    procedure AssignTo(target: TPersistent); override;
  end;

  TCEApplicationOptionsBase = class(TWritableLfmTextComponent)
  private
    fFloatingWidgetOnTop: boolean;
    fReloadLastDocuments: boolean;
    fCovModUt: boolean;
    fMaxRecentProjs: integer;
    fMaxRecentDocs: integer;
    fMaxRecentGroups: integer;
    fDcdPort: word;
    fDscanUnittests: boolean;
    fAutoSaveProjectFiles: boolean;
    fFlatLook: boolean;
    fSplitterScrollSpeed: byte;
    fAutoCheckUpdates: boolean;
    fShowBuildDuration: boolean;
    function getAdditionalPATH: string;
    procedure setAdditionalPATH(const value: string);
    function getNativeProjecCompiler: TCECompiler;
    procedure setNativeProjecCompiler(value: TCECompiler);
    procedure setSplitterScsrollSpeed(value: byte);
  published
    property additionalPATH: string read getAdditionalPATH write setAdditionalPath;
    property autoCheckUpdates: boolean read fAutoCheckUpdates write fAutoCheckUpdates;
    property coverModuleTests: boolean read fCovModUt write fCovModUt;
    property floatingWidgetOnTop: boolean read fFloatingWidgetOnTop write fFloatingWidgetOnTop;
    property reloadLastDocuments: boolean read fReloadLastDocuments write fReloadLastDocuments;
    property maxRecentProjects: integer read fMaxRecentProjs write fMaxRecentProjs;
    property maxRecentDocuments: integer read fMaxRecentDocs write fMaxRecentDocs;
    property maxRecentProjectsGroups: integer read fMaxRecentGroups write fMaxRecentGroups;
    property nativeProjectCompiler: TCECompiler read getNativeProjecCompiler write setNativeProjecCompiler;
    property dscanUnittests: boolean read fDscanUnittests write fDscanUnittests default true;
    property autoSaveProjectFiles: boolean read fAutoSaveProjectFiles write fAutoSaveProjectFiles default false;
    property flatLook: boolean read fFlatLook write fFlatLook;
    property splitterScrollSpeed: byte read fSplitterScrollSpeed write setSplitterScsrollSpeed;
    property showBuildDuration: boolean read fShowBuildDuration write fShowBuildDuration default false;
    // published for ICEEditableOptions but stored by DCD wrapper since it reloads before CEMainForm
    property dcdPort: word read fDcdPort write fDcdPort stored false;
  end;

  TCEApplicationOptions = class(TCEApplicationOptionsBase, ICEEditableOptions)
  private
    fBackup:TCEApplicationOptionsBase;
    //
    function optionedWantCategory(): string;
    function optionedWantEditorKind: TOptionEditorKind;
    function optionedWantContainer: TPersistent;
    procedure optionedEvent(event: TOptionEditorEvent);
    function optionedOptionsModified: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure assign(source: TPersistent); override;
    procedure assignTo(target: TPersistent); override;
  end;

var
  CEMainForm: TCEMainForm;

implementation
{$R *.lfm}

uses
  SynMacroRecorder, ce_dcd;

{$REGION TCERunnableOptions ----------------------------------------------------}
constructor TCERunnableOptions.create(aOwner: TComponent);
begin
  inherited;
  fStaticSwitches := TStringList.create;
  fStaticSwitches.Duplicates := TDuplicates.dupIgnore;
  fStaticSwitches.Sorted:=true;
end;


destructor TCERunnableOptions.destroy;
begin
  fStaticSwitches.free;
  inherited;
end;

procedure TCERunnableOptions.assign(source: TPersistent);
var
  src: TCERunnableOptions;
begin
  if source is TCERunnableOptions then
  begin
    src := TCERunnableOptions(source);
    fCompiler:= src.fCompiler;
    fDetectMain:= src.fDetectMain;
    fDetectLibraries:= src.fDetectLibraries;
    fOutputFolder:= src.fOutputFolder;
    fAlwaysToFolder:= src.alwaysToFolder;
    fStaticSwitches.assign(src.fStaticSwitches);
  end
  else inherited;
end;

procedure TCERunnableOptions.setStaticSwitches(value: TStringList);
begin
  fStaticSwitches.Assign(value);
  sanitizeSwitches;
end;

procedure TCERunnableOptions.afterLoad;
begin
  inherited;
  if fStaticSwitches.Count = 0 then
    setDefaultSwitches
  else
    sanitizeSwitches;
end;

procedure TCERunnableOptions.setDefaultSwitches;
begin
  fStaticSwitches.Clear;
  fStaticSwitches.AddStrings(['-vcolumns', '-w', '-wi']);
end;

procedure TCERunnableOptions.sanitizeSwitches;
var
  i: integer;
  sw: string;
  lst: TStringList;
begin
  lst := TStringList.Create;
  try
    for i:= 0 to fStaticSwitches.Count-1 do
    begin
      sw := fStaticSwitches[i];
      RemovePadChars(sw, [#0..#32]);
      // not a switch
      if sw.length < 2 then
        continue
      else if sw[1] <> '-' then
        continue
      // set according to the context
      else if sw = '-unittest' then
        continue
      else if sw = '-main' then
        continue
      // would break location detection
      else if (sw.length > 2) and (sw[1..3] = '-of') then
        continue
      // useless
      else if sw = '-run' then
        continue
      else
        lst.Add(sw);
    end;
    fStaticSwitches.Assign(lst);
  finally
    lst.free;
  end;
end;

procedure TCERunnableOptions.setOutputFolder(const value: TCEPathname);
begin
  fOutputFolder := value;
  if (length(fOutputFolder) > 0)
    and (fOutputFolder[length(fOutputFolder)] <> DirectorySeparator) then
      fOutputFolder += DirectorySeparator;
end;

procedure TCERunnableOptions.setCompiler(value: TCECompiler);
begin
  case value of
    ldc: if not exeInSysPath('ldmd2' + exeExt) then
      value := dmd;
    gdc: if not exeInSysPath('gdmd' + exeExt) then
      value := dmd;
  end;
  fCompiler :=value;
end;

constructor TCEEditableRunnableOptions.create(aOwner: TComponent);
begin
  inherited;
  fBackup := TCERunnableOptions.create(nil);
  EntitiesConnector.addObserver(self);
end;

destructor TCEEditableRunnableOptions.destroy;
begin
  fBackup.free;
  EntitiesConnector.removeObserver(self);
  inherited;
end;

function TCEEditableRunnableOptions.optionedWantCategory(): string;
begin
  exit('Runnable modules');
end;

function TCEEditableRunnableOptions.optionedWantEditorKind: TOptionEditorKind;
begin
  exit(oekGeneric);
end;

function TCEEditableRunnableOptions.optionedWantContainer: TPersistent;
begin
  fBackup.assign(self);
  exit(self);
end;

procedure TCEEditableRunnableOptions.optionedEvent(event: TOptionEditorEvent);
begin
  case event of
    oeeAccept:
      begin
        fBackup.assign(self);
        sanitizeSwitches;
      end;
    oeeCancel: assign(fBackup);
    oeeSelectCat: fBackup.assign(self);
  end;
end;

function TCEEditableRunnableOptions.optionedOptionsModified: boolean;
begin
  exit(false);
end;
{$ENDREGION --------------------------------------------------------------------}

{$REGION TCEApplicationOptions -------------------------------------------------}
constructor TCEApplicationOptions.Create(AOwner: TComponent);
begin
  inherited;
  fBackup := TCEApplicationOptionsBase.Create(self);
  EntitiesConnector.addObserver(self);
  fDscanUnittests := true;
  fSplitterScrollSpeed := 2;
  fMaxRecentProjs := 10;
  fMaxRecentDocs := 10;
  fMaxRecentGroups:= 10;
  fFlatLook:=true;
end;

function TCEApplicationOptionsBase.getNativeProjecCompiler: TCECompiler;
begin
  exit(ce_ceproject.getCEProjectCompiler);
end;

procedure TCEApplicationOptionsBase.setNativeProjecCompiler(value: TCECompiler);
begin
  ce_ceproject.setCEProjectCompiler(value);
end;

procedure TCEApplicationOptionsBase.setSplitterScsrollSpeed(value: byte);
begin
  if value < 1 then
    value := 1
  else if value > 10 then
    value := 10;
  fSplitterScrollSpeed:=value;
end;

function TCEApplicationOptionsBase.getAdditionalPATH: string;
begin
  exit(ce_common.additionalPath);
end;

procedure TCEApplicationOptionsBase.setAdditionalPath(const value: string);
var
  str: TStringList;
  cat: string;
  i: integer;
begin
  str := TStringList.Create;
  try
    str.Delimiter:= PathSeparator;
    str.DelimitedText:= value;
    for i := str.Count-1 downto 0 do
      if not str[i].dirExists then
        str.Delete(i);
    cat := str.DelimitedText;
    ce_common.additionalPath := cat;
  finally
    str.Free;
  end;
end;

destructor TCEApplicationOptions.Destroy;
begin
  EntitiesConnector.removeObserver(self);
  inherited;
end;

procedure TCEApplicationOptions.assign(source: TPersistent);
begin
  if source = CEMainForm then
  begin
    fMaxRecentProjs:= CEMainForm.fProjMru.maxCount;
    fMaxRecentDocs:= CEMainForm.fFileMru.maxCount;
    fMaxRecentGroups:= CEMainForm.fPrjGrpMru.maxCount;
    fDcdPort := DcdWrapper.port;
    fCovModUt:= CEMainForm.fCovModUt;
    fDscanUnittests := CEMainForm.fDscanUnittests;
  end else if source = fBackup then
  begin
    fCovModUt:=fBackup.fCovModUt;
    fDcdPort:=fBackup.fDcdPort;
    fMaxRecentDocs:= fBackup.fMaxRecentDocs;
    fMaxRecentProjs:= fBackup.fMaxRecentProjs;
    fMaxRecentGroups := fBackup.fMaxRecentGroups;
    fReloadLastDocuments:=fBackup.fReloadLastDocuments;
    fFloatingWidgetOnTop := fBackup.fFloatingWidgetOnTop;
    fShowBuildDuration:= fBackup.fShowBuildDuration;
    fAutoSaveProjectFiles:= fBackup.fAutoSaveProjectFiles;
    fdscanUnittests:= fBackup.dscanUnittests;
    fFlatLook:=fBackup.fFlatLook;
    fAutoCheckUpdates:= fBackup.fAutoCheckUpdates;
    CEMainForm.fDscanUnittests := fDscanUnittests;
    nativeProjectCompiler:= fBackup.nativeProjectCompiler;
  end
  else inherited;
end;

procedure TCEApplicationOptions.assignTo(target: TPersistent);
var
  i: integer;
begin
  if target = CEMainForm then
  begin
    CEMainForm.fCovModUt:= fCovModUt;
    CEMainForm.fProjMru.maxCount := fMaxRecentProjs;
    CEMainForm.fFileMru.maxCount := fMaxRecentDocs;
    CEMainForm.fPrjGrpMru.maxCount:= fMaxRecentGroups;
    CEMainForm.updateFloatingWidgetOnTop(fFloatingWidgetOnTop);
    CEMainForm.fDscanUnittests := fDscanUnittests;
    DcdWrapper.port:=fDcdPort;
    for i := 0 to CEMainForm.fWidgList.Count-1 do
      CEMainForm.fWidgList.widget[i].toolbarFlat:=fFlatLook;
  end
  else if target = fBackup then
  begin
    fBackup.fMaxRecentDocs:= fMaxRecentDocs;
    fBackup.fMaxRecentProjs:= fMaxRecentProjs;
    fBackup.fMaxRecentGroups:= fMaxRecentGroups;
    fBackup.fReloadLastDocuments:=fReloadLastDocuments;
    fBackup.fFloatingWidgetOnTop:=fFloatingWidgetOnTop;
    fBackup.fDcdPort:=fDcdPort;
    fBackup.fCovModUt:=fCovModUt;
    fBackup.fAutoSaveProjectFiles:= fAutoSaveProjectFiles;
    fBackup.fDscanUnittests:= fDscanUnittests;
    fBackup.fFlatLook:= fFlatLook;
    fBackup.fAutoCheckUpdates:= fAutoCheckUpdates;
    fBackup.fShowBuildDuration:= fShowBuildDuration;
    fBackup.nativeProjectCompiler:= nativeProjectCompiler;
  end
  else inherited;
end;

function TCEApplicationOptions.optionedWantCategory(): string;
begin
  exit('Application');
end;

function TCEApplicationOptions.optionedWantEditorKind: TOptionEditorKind;
begin
  exit(oekGeneric);
end;

function TCEApplicationOptions.optionedWantContainer: TPersistent;
begin
  AssignTo(fBackup);
  exit(self);
end;

procedure TCEApplicationOptions.optionedEvent(event: TOptionEditorEvent);
begin
  case event of
    oeeCancel: begin Assign(fBackup); AssignTo(CEMainForm); end;
    oeeAccept: begin AssignTo(CEMainForm); AssignTo(fBackup);end;
    oeeSelectCat: begin Assign(CEMainForm); AssignTo(fBackup); end;
    oeeChange: AssignTo(CEMainForm);
  end;
end;

function TCEApplicationOptions.optionedOptionsModified: boolean;
begin
  exit(false);
end;
{$ENDREGION}

{$REGION TCELastDocsAndProjs ---------------------------------------------------}
constructor TCELastDocsAndProjs.create(aOwner: TComponent);
begin
  inherited;
  fDocuments := TStringList.Create;
end;

destructor TCELastDocsAndProjs.destroy;
begin
  fDocuments.Free;
  inherited;
end;

procedure TCELastDocsAndProjs.Assign(source: TPersistent);
var
  itf: ICECommonProject = nil;
begin
  if source = CEMainForm then
  begin
    itf := CEMainForm.fFreeProj;
    if assigned(itf) then
      fProject := itf.filename;
    fProjectGroup := getProjectGroup.groupFilename;
    if itf = CEMainForm.fProject then
      fProjectIndex:=-1
    else
      fProjectIndex := getProjectGroup.getProjectIndex;
  end else
    inherited;
end;

procedure TCELastDocsAndProjs.AssignTo(target: TPersistent);
var
  itf: ICECommonProject = nil;
  dst: TCEMainForm;
  hdl: ICEMultiDocHandler;
  mem: TCESynMemo = nil;
begin
  if target is TCEMainForm then
  begin
    dst := TCEMainForm(target);
    if dst.fProjFromCommandLine then
      exit;
    itf := dst.fProject;
    if assigned(itf) and (itf.filename = fProject) and
      (itf.filename.fileExists) then exit;
    if fProject.isNotEmpty and fProject.fileExists then
    begin
      dst.openProj(fProject);
      hdl := getMultiDocHandler;
      if assigned(hdl) then
      mem := hdl.findDocument(dst.fProject.filename);
      if mem.isNotNil then
        if dst.fProject.getFormat = pfCE then
          mem.Highlighter := LfmSyn
        else
          mem.Highlighter := JsSyn;
    end;
    if fProjectGroup.isNotEmpty and fProjectGroup.fileExists then
    begin
      getProjectGroup.openGroup(fProjectGroup);
    end;
    if (fProjectIndex = -1) and assigned(dst.fFreeProj) then
      dst.fFreeProj.activate;
  end else
    inherited;
end;

procedure TCELastDocsAndProjs.setDocuments(value: TStringList);
begin
  fDocuments.Assign(value);
end;

procedure TCELastDocsAndProjs.beforeSave;
var
  i: integer;
  docHandler: ICEMultiDocHandler;
  document: TCESynMemo;
  str: string;
begin
  docHandler := getMultiDocHandler;
  if not assigned(docHandler) then
    exit;

  for i:= 0 to docHandler.documentCount-1 do
  begin
    document := docHandler.document[i];
    str := document.fileName;
    if (str <> document.tempFilename) and str.fileExists then
    begin
      fDocuments.Add(str);
      if document.Focused then
        documentIndex := i;
    end;
  end;
end;

procedure TCELastDocsAndProjs.afterLoad;
var
  docHandler: ICEMultiDocHandler;
  str: string;
  focusedName: string = '';
  i: integer;
begin
  docHandler := getMultiDocHandler;
  if not assigned(docHandler) then
    exit;

  for i := 0 to fDocuments.Count-1 do
  begin
    str := fDocuments[i];
    if str.fileExists then
    begin
      docHandler.openDocument(str);
      if i = fDocIndex then
        focusedName := str;
    end;
  end;

  if focusedName.isNotEmpty then
    docHandler.openDocument(focusedName);
end;
{$ENDREGION}

{$REGION Actions shortcuts -----------------------------------------------------}
constructor TCEPersistentMainShortcuts.create(aOwner: TComponent);
begin
  inherited;
  fCol := TCollection.Create(TCEPersistentShortcut);
end;

destructor TCEPersistentMainShortcuts.destroy;
begin
  fCol.Free;
  inherited;
end;

procedure TCEPersistentMainShortcuts.setCol(value: TCollection);
begin
  fCol.Assign(value);
end;

procedure TCEPersistentMainShortcuts.assign(source: TPersistent);
var
  itm: TCEPersistentShortcut;
  i: Integer;
begin
  fCol.Clear;
  if source = CEMainForm then
    for i := 0 to CEMainForm.Actions.ActionCount-1 do
    begin
      if CEMainForm.Actions.Actions[i].Owner <> CEMainForm then
        continue;
      itm := TCEPersistentShortcut(fCol.Add);
      itm.shortcut := TAction(CEMainForm.Actions.Actions[i]).Shortcut;
      itm.actionName := CEMainForm.Actions.Actions[i].Name;
    end
  else inherited;
end;

procedure TCEPersistentMainShortcuts.assignTo(target: TPersistent);
var
  itm: TCEPersistentShortcut;
  i,j: Integer;
begin
  if target = CEMainForm then
    for i:= 0 to fCol.Count-1 do
    begin
      itm := TCEPersistentShortcut(fCol.Items[i]);
      for j := 0 to CEMainForm.Actions.ActionCount-1 do
        if CEMainForm.Actions.Actions[i].Name = itm.actionName then
        begin
          TAction(CEMainForm.Actions.Actions[i]).Shortcut := itm.shortcut;
          break;
        end;
    end
  else inherited;
end;
{$ENDREGION}

{$REGION TCEPersistentMainMrus -------------------------------------------------}
procedure TCEPersistentMainMrus.setProjMru(value: TCEMRUFileList);
begin
  fProjMruPt.assign(value);
end;

procedure TCEPersistentMainMrus.setFileMru(value: TCEMRUFileList);
begin
  fFileMruPt.assign(value);
end;

procedure TCEPersistentMainMrus.setProjectsGroupMru(value: TCEMRUFileList);
begin
  fPrjGrpMruPt.assign(value);
end;

procedure TCEPersistentMainMrus.setTargets(projs: TCEMRUFileList; files: TCEMRUFileList;
  group: TCEMRUFileList);
begin
  fFileMruPt := files;
  fProjMruPt := projs;
  fPrjGrpMruPt := group;
end;
{$ENDREGION}

{$REGION Standard Comp/Obj------------------------------------------------------}
constructor TCEMainForm.create(aOwner: TComponent);
begin
  inherited create(aOwner);
  fMainMenuSubj := TCEMainMenuSubject.create;
  fActionHandler := TCEActionProviderSubject.create;

  EntitiesConnector.addObserver(self);

  InitMRUs;
  InitWidgets;
  InitDocking;
  LoadSettings;
  layoutUpdateMenu;
  fMultidoc := getMultiDocHandler;
  OnDragDrop:= @ddHandler.DragDrop;
  OnDragOver:= @ddHandler.DragOver;

  updateMainMenuProviders;
  EntitiesConnector.forceUpdate;
  fSymStringExpander:= getSymStringExpander;
  fProjectGroup := getProjectGroup;

  getCMdParams;
  fAppliOpts.assignTo(self);

  fInitialized := true;
end;

procedure TCEMainForm.getCMdParams;
var
  value: string;
  lst: TStringList;
begin
  if application.ParamCount > 0 then
  begin
    value := application.Params[1];
    if value.isNotEmpty then
    begin
      lst := TStringList.Create;
      try
        lst.DelimitedText := value;
        for value in lst do
        begin
          if value.isEmpty then continue;
          if isEditable(value.extractFileExt) then
            openFile(value)
          else if isValidNativeProject(value) or isValidDubProject(value) then
          begin
            // so far CE can only open 1 project at a time
            openProj(value);
            fProjFromCommandLine := true;
            break;
          end
        end;
      finally
        lst.Free;
      end;
    end;
  end;
  value := application.GetOptionValue('p', 'project');
  if value.isNotEmpty and value.fileExists then
    openProj(value);
  value := application.GetOptionValue('f', 'files');
  if value.isNotEmpty then
  begin
    lst := TStringList.Create;
    try
      lst.DelimitedText := value;
      for value in lst do
      begin
        if value.fileExists then
          openFile(value);
      end;
    finally
      lst.Free;
    end;
  end;
end;

procedure TCEMainForm.InitMRUs;
begin
  fProjMru := TCEMRUProjectList.Create;
  fFileMru := TCEMRUDocumentList.Create;
  fPrjGrpMru:= TCEMRUProjectsGroupList.create;
  fProjMru.objectTag := mnuItemMruProj;
  fFileMru.objectTag := mnuItemMruFile;
  fPrjGrpMru.objectTag := mnuItemMruGroup;
  fProjMru.OnChange := @mruChange;
  fFileMru.OnChange := @mruChange;
  fPrjGrpMru.OnChange := @mruChange;
end;

procedure TCEMainForm.InitWidgets;
var
  widg: TCEWidget;
  act: TAction;
  itm: TMenuItem;
begin
  fWidgList   := TCEWidgetList.Create;
  fMesgWidg   := TCEMessagesWidget.create(self);
  fEditWidg   := TCEEditorWidget.create(self);
  fProjWidg   := TCEProjectInspectWidget.create(self);
  fPrjCfWidg  := TCEProjectConfigurationWidget.create(self);
  fFindWidg   := TCESearchWidget.create(self);
  fExplWidg   := TCEMiniExplorerWidget.create(self);
  fLibMWidg   := TCELibManEditorWidget.create(self);
  fTlsEdWidg  := TCEToolsEditorWidget.create(self);
  fPrInpWidg  := TCEProcInputWidget.create(self);
  fTodolWidg  := TCETodoListWidget.create(self);
  fOptEdWidg  := TCEOptionEditorWidget.create(self);
  fSymlWidg   := TCESymbolListWidget.create(self);
  fInfoWidg   := TCEInfoWidget.create(self);
  fDubProjWidg:= TCEDubProjectEditorWidget.create(self);
  fDfmtWidg   := TCEDfmtWidget.create(self);
  fPrjGrpWidg := TCEProjectGroupWidget.create(self);
  {$IFDEF UNIX}
  fGdbWidg    := TCEGdbWidget.create(self);
  {$ENDIF}

  getMessageDisplay(fMsgs);

  fWidgList.addWidget(@fMesgWidg);
  fWidgList.addWidget(@fEditWidg);
  fWidgList.addWidget(@fProjWidg);
  fWidgList.addWidget(@fPrjCfWidg);
  fWidgList.addWidget(@fFindWidg);
  fWidgList.addWidget(@fExplWidg);
  fWidgList.addWidget(@fLibMWidg);
  fWidgList.addWidget(@fTlsEdWidg);
  fWidgList.addWidget(@fPrInpWidg);
  fWidgList.addWidget(@fTodolWidg);
  fWidgList.addWidget(@fOptEdWidg);
  fWidgList.addWidget(@fSymlWidg);
  fWidgList.addWidget(@fInfoWidg);
  fWidgList.addWidget(@fDubProjWidg);
  fWidgList.addWidget(@fDfmtWidg);
  fWidgList.addWidget(@fPrjGrpWidg);
  {$IFDEF UNIX}
  fWidgList.addWidget(@fGdbWidg);
  {$ENDIF}

  fWidgList.sort(@CompareWidgCaption);

  for widg in fWidgList do
  begin
    act := TAction.Create(self);
    act.Category := 'Window';
    act.Caption := widg.Caption;
    act.OnExecute := @widgetShowFromAction;
    act.Tag := ptrInt(widg);
    act.ImageIndex := 25;
    act.OnUpdate:= @updateWidgetMenuEntry;
    itm := TMenuItem.Create(self);
    itm.Action := act;
    itm.Tag := ptrInt(widg);
    mnuItemWin.Add(itm);
  end;
end;

procedure TCEMainForm.LockTopWindow(Sender: TObject; var NewSize: Integer;
  var Accept: Boolean);
begin
  //TODO-cdocking: top splitter pos can change even if locked (e.g after resize)
  accept := GetKeyShiftState = [ssCtrl];
end;

//TODO-cdocking: remove the usage of TAnchorDockSplitterEx from Laz 1.8 RC1 (OnMouseWheel public)
procedure TCEMainForm.DockSplitterMw(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  offs: integer;
  splt: TAnchorDockSplitter;
begin
  offs := -240 * fAppliOpts.splitterScrollSpeed div WheelDelta;
  splt := TAnchorDockSplitter(sender);
  splt.MoveSplitter(offs);
  if splt.ResizeAnchor in [akLeft, akRight] then
    Mouse.CursorPos:= classes.Point(Mouse.CursorPos.X + offs, Mouse.CursorPos.Y)
  else
    Mouse.CursorPos:= classes.Point(Mouse.CursorPos.X, Mouse.CursorPos.Y + offs);
  Handled := true;
end;

procedure TCEMainForm.setSplitterWheelEvent;
var
  i: integer;
  widg: TCEWidget;
  site: TControl;
  anchl: TAnchorKind;
begin
  if csDestroying in ComponentState then
    exit;
  for i := 0 to fWidgList.Count-1 do
  begin
    widg := fWidgList.widget[i];
    if not widg.isDockable then
      continue;
    for anchl in [low(anchl) .. high(anchl)] do
      if GetDockSplitterOrParent(DockMaster.GetSite(widg), anchl, site) then
      begin
        if site is TAnchorDockHostSite then
        begin
          if TAnchorDockHostSite(site).BoundSplitter.isNotNil then
            TAnchorDockSplitterEx(TAnchorDockHostSite(site).BoundSplitter).OnMouseWheel:=@DockSplitterMw;
        end
        else if site is TAnchorDockSplitter then
          TAnchorDockSplitterEx(TAnchorDockSplitter(site)).OnMouseWheel:=@DockSplitterMw;
      end;
  end;
end;

procedure TCEMainForm.widgetDockingChanged(sender: TCEWidget; newState: TWidgetDockingState);
begin
  setSplitterWheelEvent;
end;

procedure TCEMainForm.InitDocking;
var
  i: Integer;
  widg: TCEWidget;
  aManager: TAnchorDockManager;
  topsite : TControl;
  topsplt : TAnchorDockSplitter;
begin
  DockMaster.MakeDockSite(Self, [akBottom], admrpChild);
  DockMaster.OnShowOptions := @ShowAnchorDockOptions;
  DockMaster.HeaderStyle := adhsPoints;
  DockMaster.HideHeaderCaptionFloatingControl := true;

  // this is a fix (?) copied from Laz.
  if DockManager is TAnchorDockManager then
  begin
    aManager:=TAnchorDockManager(DockManager);
    aManager.PreferredSiteSizeAsSiteMinimum:=false;
  end;

  // makes widget dockable
  for i := 0 to fWidgList.Count-1 do
  begin
    widg := fWidgList.widget[i];
    if not widg.isDockable then continue;
    DockMaster.MakeDockable(widg, true);
    DockMaster.GetAnchorSite(widg).Header.HeaderPosition := adlhpTop;
    widg.onDockingChanged:= @widgetDockingChanged;
  end;

  // load existing or default docking
  if FileExists(getCoeditDocPath + 'docking.xml') then
    LoadDocking
  else
  begin
    Height := 0;
    // center
    DockMaster.ManualDock(DockMaster.GetAnchorSite(fEditWidg), DockMaster.GetSite(Self), alBottom);
    DockMaster.ManualDock(DockMaster.GetAnchorSite(fMesgWidg), DockMaster.GetSite(fEditWidg), alBottom);
    DockMaster.ManualDock(DockMaster.GetAnchorSite(fLibMWidg), DockMaster.GetSite(fMesgWidg), alClient, fMesgWidg);
    DockMaster.ManualDock(DockMaster.GetAnchorSite(fTodolWidg), DockMaster.GetSite(fMesgWidg), alClient, fMesgWidg);
    fMesgWidg.showWidget;
    // left
    DockMaster.GetAnchorSite(fSymlWidg).Width := 120;
    DockMaster.GetAnchorSite(fFindWidg).Width := 120;
    DockMaster.ManualDock(DockMaster.GetAnchorSite(fSymlWidg), DockMaster.GetSite(fEditWidg), alLeft);
    DockMaster.ManualDock(DockMaster.GetAnchorSite(fFindWidg), DockMaster.GetAnchorSite(fSymlWidg), alBottom, fSymlWidg);
    DockMaster.ManualDock(DockMaster.GetAnchorSite(fPrInpWidg), DockMaster.GetAnchorSite(fFindWidg), alTop, fFindWidg);
    DockMaster.ManualDock(DockMaster.GetAnchorSite(fExplWidg), DockMaster.GetSite(fSymlWidg), alClient, fSymlWidg);
    if GetDockSplitter(DockMaster.GetSite(fFindWidg), akTop, topsplt) then
    begin
      topsplt.MoveSplitter(40);
      topsplt := nil;
    end;
    if GetDockSplitter(DockMaster.GetSite(fPrInpWidg), akTop, topsplt) then
    begin
      topsplt.MoveSplitter(40);
      topsplt := nil;
    end;
    fSymlWidg.showWidget;
    // right
    DockMaster.GetAnchorSite(fProjWidg).Width := 190;
    DockMaster.GetAnchorSite(fDubProjWidg).Width := 190;
    DockMaster.ManualDock(DockMaster.GetAnchorSite(fProjWidg), DockMaster.GetSite(fEditWidg), alRight);
    DockMaster.ManualDock(DockMaster.GetAnchorSite(fPrjGrpWidg), DockMaster.GetSite(fProjWidg), alBottom, fProjWidg);
    DockMaster.ManualDock(DockMaster.GetAnchorSite(fDubProjWidg), DockMaster.GetAnchorSite(fProjWidg), alClient, fProjWidg);
    fProjWidg.showWidget;
    // close remaining and header to top
    for i := 0 to fWidgList.Count-1 do
    begin
      widg := fWidgList.widget[i];
      if not widg.isDockable then
        continue;
      DockMaster.GetAnchorSite(widg).Header.HeaderPosition := adlhpTop;
      if not DockMaster.GetAnchorSite(widg).HasParent then
        DockMaster.GetAnchorSite(widg).Close;
    end;
    WindowState:= wsMaximized;
  end;

  // lock space between the menu and the widgets
  if GetDockSplitterOrParent(DockMaster.GetSite(fEditWidg), akTop, topsite) then
  begin
    if topsite is TAnchorDockHostSite then
      if TAnchorDockHostSite(topsite).BoundSplitter.isNotNil then
      begin
        TAnchorDockHostSite(topsite).BoundSplitter.MoveSplitter(-500);
        TAnchorDockHostSite(topsite).BoundSplitter.OnCanOffset:= @LockTopWindow;
      end;
  end else if GetDockSplitter(DockMaster.GetSite(fEditWidg), akTop, topsplt) then
  begin
    topsplt.MoveSplitter(-500);
    topsplt.OnCanOffset:= @LockTopWindow;
  end;
end;

procedure TCEMainForm.LoadSettings;
var
  fname: string;
begin
  // project and files MRU
  fname := getCoeditDocPath + 'mostrecent.txt';
  if fname.fileExists then with TCEPersistentMainMrus.create(nil) do
  try
    setTargets(fFileMru, fProjMru, fPrjGrpMru);
    loadFromFile(fname);
  finally
    Free;
  end;
  // shortcuts for the actions standing in the main action list
  fname := getCoeditDocPath + 'mainshortcuts.txt';
  if fname.fileExists then with TCEPersistentMainShortcuts.create(nil) do
  try
    loadFromFile(fname);
    assignTo(self);
  finally
    Free;
  end;
  // runnables opts
  fRunnablesOptions := TCEEditableRunnableOptions.create(self);
  fname := getCoeditDocPath + 'runnables.txt';
  if fname.fileExists then
    fRunnablesOptions.loadFromFile(fname);
  // globals opts
  fAppliOpts := TCEApplicationOptions.Create(self);
  fname := getCoeditDocPath + 'application.txt';
  if fname.fileExists then
  begin
    fAppliOpts.loadFromFile(fname);
    fAppliOpts.assignTo(self);
  end
  else fFirstTimeCoedit := true;
end;

procedure TCEMainForm.SaveSettings;
begin
  if not fInitialized then
    exit;
  // project and files MRU
  with TCEPersistentMainMrus.create(nil) do
  try
    setTargets(fFileMru, fProjMru, fPrjGrpMru);
    saveToFile(getCoeditDocPath + 'mostrecent.txt');
  finally
    Free;
  end;
  // shortcuts for the actions standing in the main action list
  with TCEPersistentMainShortcuts.create(nil) do
  try
    assign(self);
    saveToFile(getCoeditDocPath + 'mainshortcuts.txt');
  finally
    Free;
  end;
  // globals opts
  fAppliOpts.assign(self);
  fAppliOpts.saveToFile(getCoeditDocPath + 'application.txt');
  // runnables opts
  fRunnablesOptions.saveToFile(getCoeditDocPath + 'runnables.txt');
end;

procedure TCEMainForm.SaveDocking;
var
  xcfg: TXMLConfigStorage;
  i: integer;
begin
  if not fInitialized or not Visible then
    exit;

  DockMaster.RestoreLayouts.Clear;
  if WindowState = wsMinimized then WindowState := wsNormal;
  // does not save minimized/undocked windows to prevent bugs
  for i:= 0 to fWidgList.Count-1 do
  begin
    if not fWidgList.widget[i].isDockable then continue;
    if DockMaster.GetAnchorSite(fWidgList.widget[i]).WindowState = wsMinimized then
      DockMaster.GetAnchorSite(fWidgList.widget[i]).Close
    else if not DockMaster.GetAnchorSite(fWidgList.widget[i]).HasParent then
      DockMaster.GetAnchorSite(fWidgList.widget[i]).Close;
  end;

  forceDirectory(getCoeditDocPath);
  xcfg := TXMLConfigStorage.Create(getCoeditDocPath + 'docking.xml.tmp', false);
  try
    DockMaster.SaveLayoutToConfig(xcfg);
    xcfg.WriteToDisk;
    // TODO-cdocking: remove this when AnchorDocking wont save anymore invalid layout
    with TMemoryStream.Create do try
      LoadFromFile(getCoeditDocPath + 'docking.xml.tmp');
      if Size < 10000 then
      begin
        SaveToFile(getCoeditDocPath + 'docking.xml');
        SysUtils.DeleteFile(getCoeditDocPath + 'docking.xml.tmp');
      end;
    finally
      free;
    end;
  finally
    xcfg.Free;
  end;

  xcfg := TXMLConfigStorage.Create(getCoeditDocPath + 'dockingopts.xml',false);
  try
    DockMaster.SaveSettingsToConfig(xcfg);
    xcfg.WriteToDisk;
  finally
    xcfg.Free;
  end;
end;

procedure TCEMainForm.LoadDocking;
var
  xcfg: TXMLConfigStorage;
  str: TMemoryStream;
begin
  if fileExists(getCoeditDocPath + 'docking.xml') then
  begin
    xcfg := TXMLConfigStorage.Create(getCoeditDocPath + 'docking.xml', true);
    try
      try
        DockMaster.LoadLayoutFromConfig(xcfg, false);
      except
        exit;
      end;
      str := TMemoryStream.Create;
      try
        xcfg.SaveToStream(str);
        str.saveToFile(getCoeditDocPath + 'docking.bak')
      finally
        str.Free;
      end;
    finally
      xcfg.Free;
    end;
  end;
  if fileExists(getCoeditDocPath + 'dockingopts.xml') then
  begin
    xcfg := TXMLConfigStorage.Create(getCoeditDocPath + 'dockingopts.xml', true);
    try
      try
        DockMaster.LoadSettingsFromConfig(xcfg);
      except
        exit;
      end;
      str := TMemoryStream.Create;
      try
        xcfg.SaveToStream(str);
        str.saveToFile(getCoeditDocPath + 'dockingopts.bak')
      finally
        str.Free;
      end;
    finally
      xcfg.Free;
    end;
  end;
end;

procedure TCEMainForm.FreeRunnableProc;
var
  fname: string;
begin
  if fRunProc.isNil then
    exit;

  fname := fRunProc.Executable;
  if getprocInputHandler.process = fRunProc  then
  begin
    getMessageDisplay.message('the execution of a runnable module ' +
      'has been implicitly aborted', fDoc, amcEdit, amkWarn);
    getprocInputHandler.addProcess(nil);
  end;
  killProcess(fRunProc);
  if fname.fileExists and (fname.extractFilePath = GetTempDir(false)) then
    sysutils.DeleteFile(fname);
end;

procedure TCEMainForm.SaveLastDocsAndProj;
begin
  with TCELastDocsAndProjs.create(nil) do
  try
    assign(self);
    saveToFile(getCoeditDocPath + 'lastdocsandproj.txt');
  finally
    free;
  end;
end;

procedure TCEMainForm.LoadLastDocsAndProj;
begin
  with TCELastDocsAndProjs.create(nil) do
  try
    loadFromFile(getCoeditDocPath + 'lastdocsandproj.txt');
    assignTo(self);
  finally
    free;
  end;
end;

function checkForUpdate: string;
var
  prs: TJSONParser = nil;
  dat: TJSONData = nil;
  tgg: TJSONData = nil;
  url: TJSONData = nil;
  str: string;
  mn0: byte = 0;
  mj0: byte;
  kd0: string;
  mn1: byte = 0;
  mj1: byte;
  kd1: string;
  can: boolean = false;
  rng: TStringRange = (ptr:nil; pos:0; len: 0);
  cli: TFPHTTPClient;
  lst: TStringList = nil;
  res: TResourceStream = nil;
begin
  result := '';
  cli := TFPHTTPClient.Create(nil);
  try
    try
      cli.AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)');
      str := cli.Get('https://api.github.com/repos/BBasile/Coedit/releases/latest');
      prs := TJSONParser.Create(str, [joUTF8, joIgnoreTrailingComma]);
      dat := prs.Parse;
      if dat.isNotNil then
      begin
        url := dat.FindPath('html_url');
        tgg := dat.FindPath('tag_name');
        if url.isNotNil and tgg.isNotNil then
        begin
          res:= TResourceStream.Create(HINSTANCE, 'VERSION', RT_RCDATA);
          lst := TstringList.Create;
          lst.LoadFromStream(res);
          str := lst.Text;
          if str.length < 6 then
            raise Exception.Create('');

          rng.init(str);
          mj0 := rng.takeWhile(['0'..'9']).yield.toIntNoExcept;
          rng.popWhile('_');
          kd0 := rng.takeWhile(['a'..'z']).yield;
          rng.popWhile('_');
          mn0 := rng.takeWhile(['0'..'9']).yield.toIntNoExcept;

          str := tgg.AsString;
          rng.init(str);
          mj1 := rng.takeWhile(['0'..'9']).yield.toIntNoExcept;
          rng.popWhile('_');
          kd1 := rng.takeWhile(['a'..'z']).yield;
          rng.popWhile('_');
          mn1 := rng.takeWhile(['0'..'9']).yield.toIntNoExcept;

          if mj1 > mj0 then
            can := true
          else if mj1 < mj0 then
            can := false
          else if kd0 = kd1 then
            can := (mj1 = mj0) and (mn1 > mn0)
          else if (kd0 = 'alpha') and (kd1 <> 'alpha') then
            can := (mj1 = mj0) and (mn1 > mn0)
          else if (kd0 = 'beta') and (kd1 <> 'alpha') and (kd1 <> 'beta') then
            can := (mj1 = mj0) and (mn1 > mn0)
          else if (kd0 = 'gold') and (kd1 = 'update') and (kd1 <> 'beta') then
            can := (mj1 = mj0) and (mn1 > mn0);
          if can then
            result := url.AsString;
        end;
      end;
    except
      dlgOkError('The latest release cannot be determined');
    end;
  finally
    cli.free;
    prs.free;
    dat.free;
    lst.free;
    res.free;
  end;
end;

procedure TCEMainForm.DoFirstShow;
var
  url: string;
begin
  inherited;
  // TODO-cbetterfix: clipboard doesn't work first time it's used on a reloaded doc.
  // see: http://forum.lazarus.freepascal.org/index.php/topic,30616.0.htm
  if fAppliOpts.reloadLastDocuments then
    LoadLastDocsAndProj;
  if not assigned(fProject) then
    newDubProj;

  DockMaster.ResetSplitters;
  setSplitterWheelEvent;

  if fFirstTimeCoedit then
    actFileNewRun.Execute;

  if fAppliOpts.autoCheckUpdates then
  begin
    url := checkForUpdate;
    if url <> '' then
    begin
      if dlgYesNo('An new release is available, do you wish to visit the release page ?' +
        lineEnding + '(' + url +')') = mrYes then
          openUrl(url);
    end;
  end;
end;

procedure TCEMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // saving doesnt work when csDestroying in comp.state (i.e in destroy)
  if CloseAction = caFree then
    SaveDocking;
end;

procedure TCEMainForm.MenuItem77Click(Sender: TObject);
begin
  fOptEdWidg.showWidget;
end;

destructor TCEMainForm.destroy;
begin
  SaveSettings;
  //
  fWidgList.Free;
  fProjMru.Free;
  fFileMru.Free;
  fPrjGrpMru.Free;
  FreeRunnableProc;
  //
  fMainMenuSubj.Free;
  fActionHandler.Free;
  EntitiesConnector.removeObserver(self);
  inherited;
end;

procedure TCEMainForm.UpdateDockCaption(Exclude: TControl = nil);
begin
  // otherwise dockmaster puts the widget list.
  Caption := 'Coedit';
end;

procedure TCEMainForm.ApplicationProperties1Exception(Sender: TObject;E: Exception);
begin
  if fMesgWidg.isNil then
    dlgOkError(E.Message)
  else
    fMsgs.message(E.Message, nil, amcApp, amkErr);
end;

procedure TCEMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  i: Integer;
begin
  canClose := false;
  SaveLastDocsAndProj;
  if assigned(fFreeProj) then
  begin
    if fFreeProj.modified and
      (dlgFileChangeClose(fFreeProj.filename, UnsavedProj) = mrCancel) then
        exit;
    fFreeProj.getProject.Free;
    fFreeProj := nil;
  end;
  for i := fMultidoc.documentCount-1 downto 0 do
    if not fMultidoc.closeDocument(i) then
      exit;
  if fProjectGroup.groupModified and
    (dlgFileChangeClose(fProjectGroup.groupFilename, UnsavedPGrp) = mrCancel) then
      exit;
  canClose := true;
  fProjectGroup.closeGroup;
end;

procedure TCEMainForm.updateDocumentBasedAction(sender: TObject);
begin
  TAction(sender).Enabled := fDoc.isNotNil;
end;

procedure TCEMainForm.updateProjectBasedAction(sender: TObject);
begin
  TAction(sender).Enabled := assigned(fProject) {and not fProjActionsLock};
end;

procedure TCEMainForm.updateDocEditBasedAction(sender: TObject);
begin
  if fDoc.isNotNil and fDoc.Focused then
    TAction(sender).Enabled := true
  else
    TAction(sender).Enabled := false;
end;

procedure TCEMainForm.ActionsUpdate(AAction: TBasicAction; var Handled: Boolean);
begin
  Handled := false;
  if fUpdateCount > 0 then
    exit;
  Inc(fUpdateCount);
  try
    clearActProviderEntries;
    collectedActProviderEntries;
    if AAction.isNotNil and not AAction.Update then
      TAction(AAction).enabled := true;
    updateMainMenuProviders;
  finally
    Dec(fUpdateCount);
  end;
end;

procedure TCEMainForm.updateMainMenuProviders;
var
  i, j: Integer;
  itm: TMenuItem;
  doneUpdate: boolean = false;
begin
  if not assigned(mainMenu.Images) then
    exit;
  for j := 0 to fMainMenuSubj.observersCount-1 do
  begin
    // try to update existing entry.
    for i := 0 to mainMenu.Items.Count-1 do
      if PtrInt(fMainMenuSubj.observers[j]) = mainMenu.Items[i].Tag then
      begin
        (fMainMenuSubj.observers[j] as ICEMainMenuProvider).menuUpdate(mainMenu.Items[i]);
        doneUpdate := true;
        break;
      end;
    if doneUpdate then
      continue;
    // otherwise propose to create a new entry
    itm := TMenuItem.Create(Self);
    mainMenu.Items.Add(itm);
    (fMainMenuSubj.observers[j] as ICEMainMenuProvider).menuDeclare(itm);
    itm.Tag:= PtrInt(fMainMenuSubj.observers[j]);
    case itm.Count > 0 of
      true: ;
      false: itm.Free;
    end;
  end;
end;

procedure TCEMainForm.mruChange(Sender: TObject);
var
  srcLst: TCEMruFileList;
  trgMnu: TMenuItem;
  itm: TMenuItem;
  fname: string;
  clickTrg: TNotifyEvent;
begin
  srcLst := TCEMruFileList(Sender);
  if srcLst.isNil then
    exit;
  trgMnu := TMenuItem(srcLst.objectTag);
  if trgMnu.isNil then
    exit;

  if fUpdateCount > 0 then exit;
  Inc(fUpdateCount);
  try
    if srcLst = fFileMru then
      clickTrg := @mruFileItemClick
    else if srcLst = fProjMru then
      clickTrg := @mruProjItemClick
    else if srcLst = fPrjGrpMru then
      clickTrg:= @mruProjGroupItemClick;

    trgMnu.Clear;

    for fname in srcLst do
    begin
      itm := TMenuItem.Create(trgMnu);
      itm.Hint := fname;
      itm.Caption := fname.extractFileName + ' - (' + fname + ')';
      itm.OnClick := clickTrg;
      trgMnu.Add(itm);
    end;

    trgMnu.AddSeparator;
    itm := TMenuItem.Create(trgMnu);
    itm.Caption := 'Clear';
    itm.OnClick := @mruClearClick;
    itm.Tag := PtrInt(srcLst);
    trgMnu.Add(itm);

  finally
    Dec(fUpdateCount);
  end;
end;

procedure TCEMainForm.mruClearClick(Sender: TObject);
var
  srcLst: TCEMruFileList;
begin
  srcLst := TCEMruFileList(TmenuItem(Sender).Tag);
  if srcLst.isNotNil then
    srcLst.Clear;
end;

{$ENDREGION}

{$REGION ICEMultiDocMonitor ----------------------------------------------------}
procedure TCEMainForm.docNew(document: TCESynMemo);
begin
  fDoc := document;
end;

procedure TCEMainForm.docClosing(document: TCESynMemo);
begin
  if document <> fDoc then
    exit;
  fDoc := nil;
end;

procedure TCEMainForm.docFocused(document: TCESynMemo);
begin
  fDoc := document;
end;

procedure TCEMainForm.docChanged(document: TCESynMemo);
begin
  fDoc := document;
end;
{$ENDREGION}

{$REGION ICEProjectObserver ----------------------------------------------------}
procedure TCEMainForm.projNew(project: ICECommonProject);
begin
  fProject := project;
  case fProject.getFormat of
    pfCE: fNativeProject := TCENativeProject(fProject.getProject);
    pfDUB: fDubProject := TCEDubProject(fProject.getProject);
  end;
  if not fProject.inGroup then
    fFreeProj := project;
end;

procedure TCEMainForm.projChanged(project: ICECommonProject);
begin
  showProjTitle;
end;

procedure TCEMainForm.projClosing(project: ICECommonProject);
begin
  if project = fFreeProj then
    fFreeProj := nil;
  if fProject <> project then
    exit;
  fProject := nil;
  fDubProject := nil;
  fNativeProject := nil;
  showProjTitle;
end;

procedure TCEMainForm.projFocused(project: ICECommonProject);
begin
  fProject := project;
  case fProject.getFormat of
    pfCE: fNativeProject := TCENativeProject(fProject.getProject);
    pfDUB: fDubProject := TCEDubProject(fProject.getProject);
  end;
  if not fProject.inGroup then
    fFreeProj := project
  else if project = fFreeProj then
    fFreeProj := nil;

  showProjTitle;
end;

procedure TCEMainForm.projCompiling(project: ICECommonProject);
begin
  fProjActionsLock := true;
end;

procedure TCEMainForm.projCompiled(project: ICECommonProject; success: boolean);
var
  runArgs: string = '';
  runprev: boolean = true;
  i: integer;
begin
  fProjActionsLock := false;
  if not fIsCompilingGroup then
  begin
    if fAppliOpts.showBuildDuration then
    begin
      fCompStart := Time - fCompStart;
      fMsgs.message('Build duration: ' + TimeToStr(fCompStart), project, amcProj, amkInf);
    end;
    if fRunProjAfterCompile and assigned(fProject) then
    begin
      if not success then
        runprev := dlgYesNo('last build failed, continue and run ?') = mrYes;
      if runprev then
      begin
        if fRunProjAfterCompArg and
          not InputQuery('Execution arguments', '', runargs) then
            runargs := '';
        fProject.run(runargs);
      end;
    end;
    fRunProjAfterCompile := false;
    fRunProjAfterCompArg := false;
  end
  else begin
    fGroupCompilationCnt += 1;
    if (fGroupCompilationCnt = fProjectGroup.projectCount) then
    begin
      for i:= 0 to fProjectGroup.projectCount-1 do
        if not fProjectGroup.getProject(i).compiled then
        begin
          fMsgs.message('error, the project group is not fully compiled', nil, amcAll, amkErr);
          exit;
        end;
      fMsgs.message('the project group is successfully compiled', nil, amcAll, amkInf);
      if fAppliOpts.showBuildDuration then
      begin
        fCompStart := Time - fCompStart;
        fMsgs.message('Group build duration: ' + TimeToStr(fCompStart), nil, amcAll, amkInf);
      end;
    end;
  end;
end;
{$ENDREGION}

{$REGION ICEEditableShortCut ---------------------------------------------------}
function TCEMainForm.scedWantFirst: boolean;
begin
  fScCollectCount := 0;
  result := true;
end;

function TCEMainForm.scedWantNext(out category, identifier: string; out aShortcut: TShortcut): boolean;
var
  act: TCustomAction;
begin
  act := TCustomAction(Actions.Actions[fScCollectCount]);
  category := act.Category;
  identifier := act.Caption;
  aShortcut := act.ShortCut;
  fScCollectCount += 1;
  result := fScCollectCount < actions.ActionCount;
end;

procedure TCEMainForm.scedSendItem(const category, identifier: string; aShortcut: TShortcut);
var
  act: TCustomAction;
  i: integer;
begin
  for i:= 0 to Actions.ActionCount-1 do
  begin
    act := TCustomAction(Actions.Actions[i]);
    if (act.Category = category) and (act.Caption = identifier) then
      act.ShortCut := aShortcut;
  end;
end;

procedure TCEMainForm.scedSendDone;
begin
end;
{$ENDREGION}

{$REGION TCEActionProviderHandler ----------------------------------------------}
procedure TCEMainForm.clearActProviderEntries;
var
  prov: ICEActionProvider;
  act: TContainedAction;
  i, j: Integer;
begin
  for i:= 0 to fActionHandler.observersCount-1 do
  begin
    prov := fActionHandler[i] as ICEActionProvider;
    if not prov.actHandlerWantRecollect then
      continue;
    for j := Actions.ActionCount-1 downto 0 do
    begin
      act := Actions.Actions[j];
      if (act.Owner <> Self) and (act.Tag = PtrInt(prov)) then
        act.ActionList := nil;
    end;
  end;
end;

procedure TCEMainForm.collectedActProviderEntries;
var
  prov: ICEActionProvider;
  act: TCustomAction;
  cat: string;
  i: Integer;
  procedure addAction;
  begin
    act.ActionList := Actions;
    act.Tag := ptrInt(prov);
    act.Category := cat;

    act := nil;
    cat := '';
  end;
begin
  for i:= 0 to fActionHandler.observersCount-1 do
  begin
    prov := fActionHandler[i] as ICEActionProvider;
    if not prov.actHandlerWantFirst then
      continue;

    act := nil;
    cat := '';
    while prov.actHandlerWantNext(cat, act) do
      addAction;
    addAction;
  end;
end;
{$ENDREGION}

{$REGION file ------------------------------------------------------------------}
procedure TCEMainForm.actFileHtmlExportExecute(Sender: TObject);
var
  exp: TSynExporterHTML;
begin
  if fDoc.isNil then
    exit;
  exp := TSynExporterHTML.Create(nil);
  try
    with TOpenDialog.Create(nil) do
    try
      if Execute then
      begin
        exp.Highlighter := fDoc.Highlighter;
        exp.Title := fDoc.fileName;
        exp.ExportAsText:=true;
        exp.ExportAll(fDoc.Lines);
        exp.SaveToFile(filename);
      end;
    finally
      Free;
    end;
  finally
    exp.Free;
  end;
end;

procedure TCEMainForm.newFile;
begin
  TCESynMemo.Create(nil);
end;

procedure TCEMainForm.openFile(const fname: string);
begin
  fMultidoc.openDocument(fname);
end;

procedure TCEMainForm.saveFile(document: TCESynMemo);
begin
  if (document.Highlighter = LfmSyn) or (document.Highlighter = JsSyn) then
    saveProjSource(document)
  else if document.fileName.fileExists then
    document.save;
end;

procedure TCEMainForm.mruFileItemClick(Sender: TObject);
begin
  openFile(TMenuItem(Sender).Hint);
end;

//TODO-cbugfix: OpenDialogs, double path separator when shortcuts resolved
// i.e when ofNoDereferenceLinks is not set.

procedure TCEMainForm.actFileOpenExecute(Sender: TObject);
var
  fname: string;
begin
  with TOpenDialog.Create(nil) do
  try
    if fDoc.isNotNil and not fDoc.isTemporary and fDoc.fileName.fileExists then
      initialDir := fDoc.fileName.extractFileDir;
    options := options + [ofAllowMultiSelect];
    filter := DdiagFilter;
    if execute then
      for fname in files do
        openFile(fname);
  finally
    free;
  end;
end;

procedure TCEMainForm.actProjOpenContFoldExecute(Sender: TObject);
begin
  if not assigned(fProject) or not fProject.filename.fileExists then
    exit;
  DockMaster.GetAnchorSite(fExplWidg).Show;
  getExplorer.browse(fProject.filename.extractFilePath);
end;

procedure TCEMainForm.actFileNewExecute(Sender: TObject);
begin
  newFile;
  fDoc.setFocus;
end;

procedure TCEMainForm.actFileNewRunExecute(Sender: TObject);
const
  body: array[boolean] of string =
  (
    LineEnding,
    '    // this file can be directly executed using menu file/compile & run' + LineEnding +
    '    // phobos and libman imports are allowed' + LineEnding +
    '    writeln("hello runnable module");' + LineEnding
  );
begin
  newFile;
  fDoc.Text :=
  'module runnable;' + LineEnding +
  LineEnding +
  'import std.stdio;' + LineEnding +
  LineEnding +
  'void main(string[] args)' + LineEnding +
  '{' + LineEnding +
      body[fFirstTimeCoedit] +
  '}';
  fDoc.setFocus;
end;

procedure TCEMainForm.actFileSaveAsExecute(Sender: TObject);
begin
  if fDoc.isNil then
    exit;
  with TSaveDialog.Create(nil) do
  try
    Filter := DdiagFilter;
    if not fDoc.isTemporary and fDoc.fileName.fileExists then
      InitialDir := fDoc.fileName.extractFileDir;
    if execute then
      fDoc.saveToFile(filename);
  finally
    free;
  end;
end;

procedure TCEMainForm.actFileSaveExecute(Sender: TObject);
var
  str: string;
begin
  if fDoc.isNil then
    exit;

  str := fDoc.fileName;
  if (str <> fDoc.tempFilename) and str.fileExists then
    saveFile(fDoc)
  else
    actFileSaveAs.Execute;
end;

procedure TCEMainForm.actFileAddToProjExecute(Sender: TObject);
begin
  if fDoc.isNil or not assigned(fProject) then
    exit;
  if fProject.filename = fDoc.fileName then
    exit;

  if fProject.getFormat = pfCE then
  begin
    if fDoc.fileName.fileExists and not fDoc.isTemporary then
      fNativeProject.addSource(fDoc.fileName)
    else dlgOkInfo('the file has not been added to the project because it does not exist');
  end else
    getMessageDisplay.message('use the DUB project editor to add a source to a DUB project',
      nil, amcApp, amkHint);
end;

procedure TCEMainForm.actFileCloseExecute(Sender: TObject);
begin
  if fDoc.isNotNil then
    getMultiDocHandler.closeDocument(fDoc);
end;

procedure TCEMainForm.actFileSaveAllExecute(Sender: TObject);
var
  i: Integer;
begin
  for i:= 0 to fMultidoc.documentCount-1 do
    saveFile(fMultidoc.document[i]);
end;

procedure TCEMainForm.FormDropFiles(Sender: TObject;const fnames: array of string);
var
  fname: string;
begin
  for fname in fnames do
  begin
    if isEditable(fname) then
      openFile(fname)
    else if isValidNativeProject(fname) or isValidDubProject(fname) then
    begin
      openProj(fname);
      break;
    end
    else openFile(fname);
  end;
end;

procedure TCEMainForm.actFileSaveCopyAsExecute(Sender: TObject);
var
  str: TStringList;
begin
  if fDoc.isNil then
    exit;
  with TSaveDialog.create(nil) do
  try
    if fDoc.isDSource then
      Filter := DdiagFilter;
    if fDoc.fileName.fileExists and not fDoc.isTemporary then
      InitialDir := fDoc.fileName.extractFileDir;
    if execute then
    begin
      str := TStringList.create;
      try
        str.assign(fDoc.Lines);
        str.saveToFile(FileName);
      finally
        str.free;
      end;
    end;
  finally
    free;
  end;
end;
{$ENDREGION}

{$REGION edit ------------------------------------------------------------------}
procedure TCEMainForm.actEdCopyExecute(Sender: TObject);
begin
  if fDoc.isNotNil then
    fDoc.CopyToClipboard;
end;

procedure TCEMainForm.actEdCutExecute(Sender: TObject);
begin
  if fDoc.isNotNil then
    fDoc.CutToClipboard;
end;

procedure TCEMainForm.actEdPasteExecute(Sender: TObject);
begin
  if fDoc.isNotNil then
    fDoc.PasteFromClipboard;
end;

procedure TCEMainForm.actEdUndoExecute(Sender: TObject);
begin
  if fDoc.isNotNil then
    fDoc.Undo;
end;

procedure TCEMainForm.actEdRedoExecute(Sender: TObject);
begin
  if fDoc.isNotNil then
    fDoc.Redo;
end;

procedure TCEMainForm.actEdMacPlayExecute(Sender: TObject);
begin
  if fDoc.isNotNil then
    fEditWidg.macRecorder.PlaybackMacro(fDoc);
end;

procedure TCEMainForm.actEdMacStartStopExecute(Sender: TObject);
begin
  if fDoc.isNotNil then
  begin
    if fEditWidg.macRecorder.State = msRecording then
      fEditWidg.macRecorder.Stop
    else fEditWidg.macRecorder.RecordMacro(fDoc);
  end;
end;

procedure TCEMainForm.actEdIndentExecute(Sender: TObject);
begin
  if fDoc.isNotNil then
    fDoc.ExecuteCommand(ecBlockIndent, '', nil);
end;

procedure TCEMainForm.actEdUnIndentExecute(Sender: TObject);
begin
  if fDoc.isNotNil then
    fDoc.ExecuteCommand(ecBlockUnIndent, '', nil);
end;

procedure TCEMainForm.actEdFindExecute(Sender: TObject);
var
  win: TAnchorDockHostSite;
  str: string;
begin
  win := DockMaster.GetAnchorSite(fFindWidg);
  if win.isNil then exit;
  win.Show;
  win.BringToFront;
  if fDoc.isNil then exit;
  //
  if fDoc.SelAvail then
    str := fDoc.SelText
  else
    str := fDoc.Identifier;
  ffindwidg.cbToFind.Text := str;
  ffindwidg.cbToFindChange(nil);
end;

procedure TCEMainForm.actEdFindNextExecute(Sender: TObject);
begin
  ffindwidg.actFindNextExecute(nil);
end;
{$ENDREGION}

{$REGION run -------------------------------------------------------------------}
function TCEMainForm.runnableExename: string;
var
  ofr: string;
begin
  result := '';
  if fDoc.isNil then
    exit;

  result := fDoc.fileName.stripFileExt + exeExt;
  if fDoc.isTemporary then
    exit;
  ofr := fRunnablesOptions.outputFolder;
  if ofr.isNotEmpty then
  begin
    if not fRunnablesOptions.alwaysToFolder and assigned(fProject)
      and not fProject.isSource(fDoc.fileName) then
        exit;
    if FilenameIsAbsolute(ofr) then
    begin
      if ofr.dirExists then
        result := ofr + fDoc.fileName.extractFileName.stripFileExt + exeExt;
    end else
    begin
      result := fDoc.fileName.extractFilePath + ofr
        + fDoc.fileName.extractFileName.stripFileExt + exeExt;
    end;
  end;
end;

procedure TCEMainForm.asyncprocOutput(sender: TObject);
var
  proc: TCEProcess;
  lst: TStringList;
  str: string;
begin
  proc := TCEProcess(sender);
  lst := TStringList.Create;
  try
    proc.getFullLines(lst);
    if proc = fRunProc then for str in lst do
      fMsgs.message(str, fDoc, amcEdit, amkBub)
    else // dmd used to compile runnable
      for str in lst do
        fMsgs.message(str, fDoc, amcEdit, amkAuto);
  finally
    lst.Free;
  end;
end;

procedure TCEMainForm.asyncprocTerminate(sender: TObject);
var
  proc: TCEProcess;
  inph: TObject;
begin
  proc := TCEProcess(sender);
  asyncprocOutput(sender);
  inph := EntitiesConnector.getSingleService('ICEProcInputHandler');
  if inph.isNotNil then
    (inph as ICEProcInputHandler).removeProcess(proc);
  if (proc.ExitStatus <> 0) then
    fMsgs.message(format('error: the process (%s) has returned the signal %d',
      [proc.Executable, proc.ExitStatus]), fDoc, amcEdit, amkErr);
end;

procedure TCEMainForm.actSetRunnableSwExecute(Sender: TObject);
var
  form: TForm;
  memo: TMemo;
begin
  if fRunnablesOptions.fStaticSwitches.Count = 0 then
    fRunnablesOptions.setDefaultSwitches;
  form := TForm.Create(nil);
  form.BorderIcons:= [biSystemMenu];
  memo := TMemo.Create(form);
  memo.Align := alClient;
  memo.BorderSpacing.Around:=4;
  memo.Lines.Assign(fRunnablesOptions.staticSwitches);
  memo.Parent := form;
  form.ShowModal;

  fRunnablesOptions.staticSwitches.Assign(memo.Lines);
  fRunnablesOptions.sanitizeSwitches;

  form.Free;
end;

function TCEMainForm.compileRunnable(unittest: boolean = false): boolean;
var
  i: integer;
  fname: string;
  dmdproc: TCEProcess;
  lst: TStringList = nil;
  firstLineFlags: string = '';
  asObj: boolean = false;
  hasMain: THasMain;
  rng: TStringRange = (ptr:nil; pos:0; len: 0);
begin

  result := false;
  fMsgs.clearByData(fDoc);
  FreeRunnableProc;
  if fDoc.isNil or (fDoc.Lines.Count = 0) then
    exit;

  firstlineFlags := fDoc.Lines[0];
  rng.init(firstLineFlags);
  if rng.startsWith('#!') then
  begin
    rng.popFrontN(2)^
      .popWhile([' ', #9])^
      .popUntil([' ', #9, ':'])^
      .popWhile([' ', #9, ':']);
    firstlineFlags := rng.takeUntil(#0).yield;
    firstlineFlags := fSymStringExpander.expand(firstlineFlags);
    lst := TStringList.Create;
    CommandToList(firstlineFlags, lst);
    for i:= lst.Count-1 downto 0 do
    begin
      if (lst[i].length > 2) and (lst[i][1..3] = '-of') then
      begin
        lst.Delete(i);
        fMsgs.message('the option "-of" is not be handled in the runnable modules',
          fDoc, amcEdit, amkWarn);
      end
      else if lst[i] = '-c' then
      begin
        if not unittest then
          asObj:=true
        else
        begin
          lst.Delete(i);
          fMsgs.message('the option "-c" is not be handled when a module is tested',
            fDoc, amcEdit, amkWarn);
        end;
      end
      else if lst[i] = '-run' then
        lst.Delete(i);
    end;
  end;

  dmdproc := TCEProcess.Create(nil);
  try
    fMsgs.message('compiling ' + shortenPath(fDoc.fileName, 25), fDoc, amcEdit, amkInf);
    if fDoc.fileName.fileExists then
      fDoc.save
    else
      fDoc.saveTempFile;
    fname := runnableExename.stripFileExt;

    if fRunnablesOptions.staticSwitches.Count = 0 then
      fRunnablesOptions.setDefaultSwitches;
    {$IFDEF RELEASE}
    dmdProc.ShowWindow := swoHIDE;
    {$ENDIF}
  	dmdproc.OnReadData := @asyncprocOutput;
  	dmdproc.OnTerminate:= @asyncprocTerminate;
    dmdproc.Options := [poUsePipes, poStderrToOutPut];
    case fRunnablesOptions.compiler of
      dmd: dmdProc.Executable :='dmd'   + exeExt;
      ldc: dmdProc.Executable :='ldmd2' + exeExt;
      gdc: dmdProc.Executable :='gdmd'  + exeExt;
    end;
    dmdproc.Parameters.Add(fDoc.fileName);
    if not asObj then
      dmdproc.Parameters.Add('-of' + fname + exeExt)
    else
      dmdproc.Parameters.Add('-of' + fname + objExt);
    dmdproc.Parameters.Add('-J' + fDoc.fileName.extractFilePath);
    dmdproc.Parameters.AddStrings(fRunnablesOptions.staticSwitches);
    if lst.isNotNil and (lst.Count <> 0) then
      dmdproc.Parameters.AddStrings(lst);
    if fRunnablesOptions.detectMain then
    begin
      hasMain := fDoc.implementMain;
      case hasMain of
        mainNo:
          dmdproc.Parameters.Add('-main');
        mainDefaultBehavior:
          if unittest then
            dmdproc.Parameters.Add('-main');
      end;
    end;
    if unittest then
    begin
      if not fRunnablesOptions.detectMain then
        dmdproc.Parameters.Add('-main');
      dmdproc.Parameters.Add('-unittest');
      if fCovModUt then
        dmdproc.Parameters.Add('-cov');
    end
    else dmdproc.Parameters.Add('-version=runnable_module');

    if fRunnablesOptions.detectLibraries then
      LibMan.getLibsForSource(fDoc.Lines, dmdproc.Parameters, dmdproc.Parameters)
    else
    begin
      LibMan.getLibFiles(nil, dmdproc.Parameters);
      LibMan.getLibSourcePath(nil, dmdproc.Parameters);
    end;
    deleteDups(dmdproc.Parameters);
    dmdproc.Execute;
    while dmdproc.Running do
      application.ProcessMessages;
    if not asObj then
      sysutils.DeleteFile(fname + objExt);
    if (dmdProc.ExitStatus = 0) then
    begin
      result := true;
      fMsgs.message(shortenPath(fDoc.fileName, 25) + ' successfully compiled',
        fDoc, amcEdit, amkInf);
    end
    else begin
      fMsgs.message(format('error: the process (%s) has returned the signal %d',
        [dmdproc.Executable, dmdproc.ExitStatus]), fDoc, amcEdit, amkErr);
      fMsgs.message(shortenPath(fDoc.fileName, 25) + ' has not been compiled',
        fDoc, amcEdit, amkErr);
    end;

  finally
    dmdproc.Free;
    if lst.isNotNil then
      lst.Free;
  end;
end;

procedure TCEMainForm.executeRunnable(unittest: boolean = false; redirect: boolean = true;
	const runArgs: string = '');
var
  lst: TStringList;
  fname: string;
begin
  if fDoc.isNil then
    exit;
  fname := runnableExename;
  if not fname.fileExists then
    exit;

  fRunProc := TCEProcess.Create(nil);
  if redirect then
  begin
  	fRunProc.Options := [poStderrToOutPut, poUsePipes];
  	fRunProc.ShowWindow := swoHIDE;
  	fRunProc.OnReadData := @asyncprocOutput;
  	fRunProc.OnTerminate:= @asyncprocTerminate;
  end
  else
  begin
    {$IFNDEF WINDOWS}
    fRunProc.Options := fRunProc.Options + [poNewConsole];
    {$ENDIF}
  end;
  lst := TStringList.Create;
  try
    fRunProc.CurrentDirectory := fRunProc.Executable.extractFileDir;
    if runArgs.isNotEmpty then
    begin
      CommandToList(fSymStringExpander.expand(runArgs), lst);
      fRunProc.Parameters.AddStrings(lst);
    end;
    fRunProc.Executable := fname;
    if unittest and fCovModUt then
      fRunProc.OnTerminate:=@unittestDone;
    if redirect then
      getprocInputHandler.addProcess(fRunProc);
    fRunProc.Execute;
  finally
    lst.Free;
  end;
end;

procedure TCEMainForm.unittestDone(Sender: TObject);
var
  fullcov: boolean;
  fname, covname: string;
  lst: TStringList;
  i: integer;
begin
  asyncprocTerminate(sender);
  if fCovModUt and assigned(fRunProc) and (fRunProc.ExitStatus = 0) then
  begin
    fname   := fDoc.fileName.stripFileExt;
    fullcov := true;
    covname := ReplaceStr(fname + '.lst', DirectorySeparator, '-');
    {$IFDEF WINDOWS}
    covname := ReplaceStr(covname, DriveSeparator, '-');
    {$ENDIF}
    if covname.fileExists then
    begin
      lst := TStringList.Create;
      try
        lst.LoadFromFile(covname);
        for i := 0 to lst.Count-1 do
          if lst[i][1..7] = '0000000' then
          begin
            fMsgs.message(format('%s(%d): %s', [fDoc.fileName, i+1,
              'not covered by the unittests']), fDoc, amcEdit, amkWarn);
            fullcov := false;
          end;
        sysutils.DeleteFile(covname);
        sysutils.DeleteFile('__main.lst');
        if fullcov then fMsgs.message(shortenPath(fDoc.fileName, 25)
          + ' is 100% covered by the unittests', fDoc, amcEdit, amkInf);
      finally
        lst.free;
      end;
    end else
      fMsgs.message('the coverage file cannot be found', fDoc, amcEdit, amkWarn);
  end;
end;

procedure TCEMainForm.actFileUnittestExecute(Sender: TObject);
begin
  if fDoc.isNil then
    exit;
  if compileRunnable(true) then
    executeRunnable(true, true);
end;

procedure TCEMainForm.actFileCompAndRunExecute(Sender: TObject);
begin
  if fDoc.isNil then
    exit;
  if compileRunnable(false) then
    executeRunnable(false, true);
end;

procedure TCEMainForm.actFileCompileAndRunOutExecute(Sender: TObject);
begin
  if fDoc.isNil then
    exit;
  if compileRunnable(false) then
    executeRunnable(false, false);
end;

procedure TCEMainForm.actFileCompAndRunWithArgsExecute(Sender: TObject);
var
  runargs: string = '';
begin
  if fDoc.isNil then
    exit;
  if not InputQuery('Execution arguments', '', runargs) then
    exit;
  if compileRunnable(false) then
    executeRunnable(false, true, runargs);
end;

procedure TCEMainForm.actFileCompileExecute(Sender: TObject);
begin
  compileRunnable(false);
end;

procedure TCEMainForm.actFileDscannerExecute(Sender: TObject);
var
  lst: TStringList;
  prc: TProcess;
  pth: string;
  msg: string;
begin
  if fDoc.isNil then
    exit;
  if fDoc.isTemporary and fDoc.modified then
    fDoc.saveTempFile;
  pth := exeFullName('dscanner' + exeExt);
  if not pth.fileExists then
    exit;
  prc := TProcess.Create(nil);
  lst := TStringList.Create;
  try
    prc.Executable:=pth;
    prc.Options := [poUsePipes, poStderrToOutPut {$IFDEF WINDOWS}, poNewConsole{$ENDIF}];
    prc.ShowWindow:= swoHIDE;
    prc.Parameters.Add(fDoc.fileName);
    prc.Parameters.Add('-S');
    if not fDscanUnittests then
      prc.Parameters.Add('--skipTests');
    prc.Execute;
    processOutputToStrings(prc, lst);
    while prc.Running do;
    for msg in lst do
      fMsgs.message(msg, fDoc, amcEdit, amkAuto);
  finally
    prc.Free;
    lst.Free;
  end;
end;

procedure TCEMainForm.actFileNewDubScriptExecute(Sender: TObject);
begin
  newFile;
  fDoc.Text :=
  '/+ dub.sdl:' + LineEnding +
  '   name "dub_script" +/' + LineEnding +
  'module dub_script;' + LineEnding +
  LineEnding +
  'import std.stdio;' + LineEnding +
  LineEnding +
  'void main(string[] args)' + LineEnding +
  '{' + LineEnding + '}';
  fDoc.setFocus;
end;

procedure TCEMainForm.actFileRunDubExecute(Sender: TObject);
begin
  dubFile(false);
end;

procedure TCEMainForm.actFileRunDubOutExecute(Sender: TObject);
begin
  dubFile(true);
end;

procedure TCEMainForm.dubFile(outside: boolean);
begin
  if fDoc.isNil then
    exit;
  FreeRunnableProc;
  fRunProc := TCEProcess.Create(nil);
  if fDoc.fileName.fileExists then
    fDoc.save
  else
    fDoc.saveTempFile;
  fRunProc.Executable:= exeFullName('dub' + exeExt);
  fRunProc.Parameters.Add(fDoc.fileName);
  if not outside then
  begin
	  fRunProc.Options := [poStderrToOutPut, poUsePipes];
	  fRunProc.ShowWindow := swoHIDE;
	  fRunProc.OnReadData := @asyncprocOutput;
	  fRunProc.OnTerminate:= @asyncprocTerminate;
    getprocInputHandler.addProcess(fRunProc);
  end
  else
  begin
    {$IFNDEF WINDOWS}
    fRunProc.Options := fRunProc.Options + [poNewConsole];
    {$ENDIF}
  end;
  case fRunnablesOptions.compiler of
    gdc: fRunProc.Parameters.add('--compiler=gdc');
    ldc: fRunProc.Parameters.add('--compiler=ldc2');
  end;
  fRunProc.execute;
end;

procedure TCEMainForm.runFile(outside: boolean);
var
  fname: string;
  older: boolean = false;
  exist: boolean = false;
const
  messg: string = 'Either the runnable does not exist or it is older than its source.' +
    LineEnding +  'Do you wish to recompile it ?';
begin
  if fDoc.isNil then
    exit;
  FreeRunnableProc;
  fname := runnableExename;
  if fname.fileExists then
  begin
    exist := true;
    older := FileAge(fname) < FileAge(fDoc.fileName);
  end;
  if (not exist) or (older) then
  begin
    if dlgYesNo(messg) = mrYes then
      compileRunnable
    else if not exist then
      exit;
  end;
  if fname.fileExists then
    executeRunnable(false, not outside);
end;

procedure TCEMainForm.actFileRunExecute(Sender: TObject);
begin
  runFile(false);
end;

procedure TCEMainForm.actFileRunOutExecute(Sender: TObject);
begin
  runFile(true);
end;

procedure TCEMainForm.actFileOpenContFoldExecute(Sender: TObject);
begin
  if fDoc.isNil or not fDoc.fileName.fileExists then
    exit;
  DockMaster.GetAnchorSite(fExplWidg).Show;
  getExplorer.browse(fDoc.fileName.extractFilePath);
end;

procedure TCEMainForm.actProjCompileExecute(Sender: TObject);
begin
  if fAppliOpts.autoSaveProjectFiles then
    saveModifiedProjectFiles(fProject);
  if fAppliOpts.showBuildDuration then
    fCompStart := Time;
  fProject.compile;
end;

procedure TCEMainForm.actProjCompileAndRunExecute(Sender: TObject);
begin
  fRunProjAfterCompile := true;
  if fAppliOpts.autoSaveProjectFiles then
    saveModifiedProjectFiles(fProject);
  if fAppliOpts.showBuildDuration then
    fCompStart := Time;
  fProject.compile;
end;

procedure TCEMainForm.actProjCompAndRunWithArgsExecute(Sender: TObject);
begin
  fRunProjAfterCompile := true;
  fRunProjAfterCompArg := true;
  if fAppliOpts.autoSaveProjectFiles then
    saveModifiedProjectFiles(fProject);
  if fAppliOpts.showBuildDuration then
    fCompStart := Time;
  fProject.compile;
end;

procedure TCEMainForm.actProjRunExecute(Sender: TObject);
begin
  if fProject.binaryKind <> executable then
    dlgOkInfo('Non executable projects cant be run')
  else
  begin
    if (not fProject.targetUpToDate) then if
      dlgYesNo('The project output is not up-to-date, rebuild ?') = mrYes then
      begin
        if fAppliOpts.autoSaveProjectFiles then
          saveModifiedProjectFiles(fProject);
        if fAppliOpts.showBuildDuration then
          fCompStart := Time;
        fProject.compile;
      end;
    if fProject.outputFilename.fileExists or (fProject.getFormat = pfDUB) then
      fProject.run;
  end;
end;

procedure TCEMainForm.actProjRunWithArgsExecute(Sender: TObject);
var
  runargs: string = '';
begin
  if InputQuery('Execution arguments', '', runargs) then
    fProject.run(runargs);
end;
{$ENDREGION}

{$REGION view ------------------------------------------------------------------}
procedure TCEMainForm.updateWidgetMenuEntry(sender: TObject);
var
  widg: TCEWidget;
  act: TAction;
begin
  if sender.isNil then
    exit;
  act := TAction(sender);
  if act.Tag = 0 then
    exit;

  widg := TCEWidget(act.Tag);
  if widg.isDockable then
  begin
    if DockMaster.GetAnchorSite(widg).GetTopParent = DockMaster.GetAnchorSite(widg) then
      act.Enabled := true
    else
      act.Enabled := not widg.Parent.IsVisible
  end
  else act.Enabled := not widg.IsVisible;
end;

procedure TCEMainForm.widgetShowFromAction(sender: TObject);
var
  widg: TCEWidget;
begin
  widg := TCEWidget( TComponent(sender).tag );
  if widg.isNotNil then
    widg.showWidget;
end;

procedure TCEMainForm.layoutLoadFromFile(const fname: string);
var
  xcfg: TXMLConfigStorage;
begin
  if not fname.fileExists then
    exit;
  xcfg := TXMLConfigStorage.Create(fname, true);
  try
    DockMaster.RestoreLayouts.Clear;
    DockMaster.LoadLayoutFromConfig(xcfg, false);
  finally
    xcfg.Free;
  end;
end;

procedure TCEMainForm.layoutSaveToFile(const fname: string);
var
  xcfg: TXMLConfigStorage;
  i: integer;
begin
  DockMaster.RestoreLayouts.Clear;
  for i:= 0 to fWidgList.Count-1 do
  begin
    if not fWidgList.widget[i].isDockable then continue;
    if DockMaster.GetAnchorSite(fWidgList.widget[i]).WindowState = wsMinimized then
      DockMaster.GetAnchorSite(fWidgList.widget[i]).Close
    else if not DockMaster.GetAnchorSite(fWidgList.widget[i]).HasParent then
      DockMaster.GetAnchorSite(fWidgList.widget[i]).Close;
  end;
  //
  forceDirectory(fname.extractFilePath);
  xcfg := TXMLConfigStorage.Create(fname + '.tmp', false);
  try
    DockMaster.SaveLayoutToConfig(xcfg);
    xcfg.WriteToDisk;
    // prevent any invalid layout to be saved (AnchorDocking bug)
    // TODO-cdocking: remove this when AnchorDocking wont save anymore invalid layout
    with TMemoryStream.Create do try
      LoadFromFile(fname + '.tmp');
      if Size < 10000 then
      begin
        SaveToFile(fname);
        SysUtils.DeleteFile(fname + '.tmp');
      end else
        getMessageDisplay.message('prevented an invalid layout to be saved', nil,
          amcApp, amkWarn);
    finally
      free;
    end;
  finally
    xcfg.Free;
  end;
end;

procedure TCEMainForm.layoutUpdateMenu;
var
  lst: TStringList;
  itm: TMenuItem;
  i: integer;
begin
  itm := TMenuItem.Create(self);
  itm.Action := actLayoutSave;
  mnuLayout.Add(itm);
  mnuLayout.AddSeparator;

  lst := TStringList.Create;
  try
    listFiles(lst, getCoeditDocPath + 'layouts' + DirectorySeparator);
    for i := 0 to lst.Count-1 do
    begin
      itm := TMenuItem.Create(self);
      itm.Caption := lst[i].extractFileName;
      itm.Caption := itm.Caption.stripFileExt;
      itm.OnClick := @layoutMnuItemClick;
      itm.ImageIndex := 32;
      mnuLayout.Add(itm);
    end;
  finally
    lst.Free;
  end;
end;

procedure TCEMainForm.layoutMnuItemClick(sender: TObject);
begin
  layoutLoadFromFile(getCoeditDocPath + 'layouts' + DirectorySeparator +
    TMenuItem(sender).Caption + '.xml');
end;

procedure TCEMainForm.actLayoutSaveExecute(Sender: TObject);
var
  fname: string = '';
begin
  if not InputQuery('New layout name', '', fname) then
    exit;

  fname := fname.extractFileName;
  if fname.extractFileExt <> '.xml' then
    fname += '.xml';

  layoutSaveToFile(getCoeditDocPath + 'layouts' + DirectorySeparator + fname);
  layoutUpdateMenu;
end;

procedure TCEMainForm.updateFloatingWidgetOnTop(onTop: boolean);
var
  widg: TCEWidget;
const
  fstyle: array[boolean] of TFormStyle = (fsNormal, fsStayOnTop);
begin
  for widg in fWidgList do if widg.Parent.isNotNil and
    widg.Parent.Parent.isNil and widg.isDockable then
  begin
    TForm(widg.Parent).FormStyle := fstyle[onTop];
    //TODO-cbugfix: floating widg on top from true to false, widg remains on top
    // OK on linux (LCL 1.6.0), initially observed on win & LCL 1.4.2
    if TForm(widg.Parent).Visible then if not onTop then
      TForm(widg.Parent).SendToBack;
  end;
end;
{$ENDREGION}

{$REGION project ---------------------------------------------------------------}
function  TCEMainForm.checkProjectLock(message: boolean = true): boolean;
begin
  result := false;
  if fProjActionsLock then
  begin
    result := true;
    if message then
      dlgOkInfo('This action is disabled while a project compiles',
        'Project lock warning');
  end
end;

procedure TCEMainForm.showProjTitle;
begin
  if assigned(fProject) and fProject.filename.fileExists then
    caption := format('Coedit - %s', [shortenPath(fProject.filename, 30)])
  else
    caption := 'Coedit';
end;

procedure TCEMainForm.saveProjSource(const document: TCESynMemo);
var
  fname: string;
begin
  if not assigned(fProject) or checkProjectLock or
    (fProject.filename <> document.fileName) then
      exit;

  fname := fProject.filename;
  document.saveToFile(fname);
  fProject.reload;
end;

function TCEMainForm.closeProj: boolean;
begin
  if not assigned(fProject) then
    exit(true);

  result := false;
  if fProject = fFreeProj then
  begin
    if checkProjectLock then
      exit;
    fProject.getProject.Free;
    fFreeProj := nil;
  end;
  fProject := nil;
  fNativeProject := nil;
  fDubProject := nil;
  showProjTitle;
  result := true;
end;

procedure TCEMainForm.actProjNewDubJsonExecute(Sender: TObject);
begin
  if assigned(fProject) and not fProject.inGroup and fProject.modified and
    (dlgFileChangeClose(fProject.filename, UnsavedProj) = mrCancel) then
      exit;
  if not closeProj then
    exit;
  newDubProj;
end;

procedure TCEMainForm.actProjNewNativeExecute(Sender: TObject);
begin
  if assigned(fProject) and not fProject.inGroup and fProject.modified and
    (dlgFileChangeClose(fProject.filename, UnsavedProj) = mrCancel) then
      exit;
  if not closeProj then
    exit;
  newNativeProj;
end;

procedure TCEMainForm.newNativeProj;
begin
  fNativeProject := TCENativeProject.Create(nil);
  fNativeProject.Name := 'CurrentProject';
  fProject := fNativeProject as ICECommonProject;
  showProjTitle;
end;

procedure TCEMainForm.newDubProj;
begin
  fDubProject := TCEDubProject.create(nil);
  fProject := fDubProject as ICECommonProject;
  showProjTitle;
end;

procedure TCEMainForm.saveProj;
begin
  fProject.saveToFile(fProject.filename);
end;

procedure TCEMainForm.saveProjAs(const fname: string);
begin
  fProject.saveToFile(fname);
  showProjTitle;
end;

procedure TCEMainForm.openProj(const fname: string);
var
  ext: string;
begin
  if not closeProj then
    exit;
  ext := fname.extractFileExt.upperCase;
  if (ext = '.JSON') or (ext = '.SDL') then
    newDubProj
  else
    newNativeProj;

  fProject.loadFromFile(fname);
  showProjTitle;
end;

procedure TCEMainForm.mruProjItemClick(Sender: TObject);
begin
  if checkProjectLock then
    exit;
  if assigned(fProject) and not fProject.inGroup and fProject.modified and
    (dlgFileChangeClose(fProject.filename, UnsavedProj) = mrCancel) then
      exit;
  openProj(TMenuItem(Sender).Hint);
end;

procedure TCEMainForm.mruProjGroupItemClick(Sender: TObject);
begin
  if checkProjectLock then
    exit;
  if fProjectGroup.groupModified and (dlgFileChangeClose(
    fProjectGroup.groupFilename, UnsavedPGrp) = mrCancel) then
      exit;
  fProjectGroup.closeGroup;
  fProjectGroup.openGroup(TMenuItem(Sender).Hint);
end;

procedure TCEMainForm.actProjCloseExecute(Sender: TObject);
begin
  if assigned(fProject) and not fProject.inGroup and fProject.modified and
    (dlgFileChangeClose(fProject.filename, UnsavedProj) = mrCancel) then
      exit;
  closeProj;
end;

procedure TCEMainForm.actProjSaveAsExecute(Sender: TObject);
begin
  if checkProjectLock then
    exit;
  if (fProject.getFormat = pfDUB) and TCEDubProject(fProject.getProject).isSDL then
  begin
    fMsgs.message(DubSdlWarning, fProject, amcProj, amkWarn);
    exit;
  end;
  with TSaveDialog.Create(nil) do
  try
    if fProject.filename.fileExists then
      InitialDir := fproject.filename.extractFileDir;
    if execute then
      saveProjAs(filename);
  finally
    Free;
  end;
end;

procedure TCEMainForm.actProjSaveExecute(Sender: TObject);
begin
  if not assigned(fProject) then
    exit;
  if (fProject.getFormat = pfDUB) and TCEDubProject(fProject.getProject).isSDL then
  begin
    fMsgs.message(DubSdlWarning, fProject, amcProj, amkWarn);
    exit;
  end;
  if checkProjectLock then
    exit;
  if fProject.filename.isNotEmpty then
    saveProj
  else
    actProjSaveAs.Execute;
end;

procedure TCEMainForm.actProjOpenExecute(Sender: TObject);
begin
  if checkProjectLock then
      exit;
  if assigned(fProject) and fProject.modified and
    (dlgFileChangeClose(fProject.filename, UnsavedProj) = mrCancel) then exit;
  with TOpenDialog.Create(nil) do
  try
    if execute then openProj(filename);
  finally
    Free;
  end;
end;

procedure TCEMainForm.actProjOptsExecute(Sender: TObject);
var
  win: TControl = nil;
begin
  if assigned(fProject) then case fProject.getFormat of
    pfDUB: win := DockMaster.GetAnchorSite(fDubProjWidg);
    pfCE: win := DockMaster.GetAnchorSite(fPrjCfWidg);
  end
  else win := DockMaster.GetAnchorSite(fPrjCfWidg);
  if win.isNotNil then
  begin
    win.Show;
    win.BringToFront;
  end;
end;

procedure TCEMainForm.actProjSourceExecute(Sender: TObject);
begin
  if not assigned(fProject) or not fProject.filename.fileExists then
    exit;

  if (fProject.getFormat = pfDUB) and TCEDubProject(fProject.getProject).isSDL then
  begin
    fMsgs.message(DubSdlWarning, fProject, amcProj, amkWarn);
    exit;
  end;

  openFile(fProject.filename);
  fDoc.isProjectDescription := true;
  if fProject.getFormat = pfCE then
    fDoc.Highlighter := LfmSyn
  else
    fDoc.Highlighter := JsSyn;
end;

procedure TCEMainForm.actProjOptViewExecute(Sender: TObject);
begin
  if not assigned(fProject) then
    exit;
  dlgOkInfo(fProject.getCommandLine, 'Compilation command line');
end;

procedure TCEMainForm.actProjOpenGroupExecute(Sender: TObject);
begin
  if fProjectGroup.groupModified then
  begin
    if dlgFileChangeClose(fProjectGroup.groupFilename, UnsavedPGrp) = mrCancel then
      exit;
  end;
  with TOpenDialog.Create(nil) do
  try
    if execute then
    begin
      fProjectGroup.closeGroup;
      fProjectGroup.openGroup(filename);
      fPrjGrpMru.Insert(0, filename);
    end;
  finally
    free;
  end;
end;

procedure TCEMainForm.actProjSaveGroupAsExecute(Sender: TObject);
begin
  with TSaveDialog.Create(nil) do
  try
    if fProjectGroup.groupFilename.fileExists then
      InitialDir := fProjectGroup.groupFilename.extractFileDir;
    if execute then
      fProjectGroup.saveGroup(filename);
  finally
    free;
  end;
end;

procedure TCEMainForm.actProjSaveGroupExecute(Sender: TObject);
begin
  if not fProjectGroup.groupFilename.fileExists then
    actProjSaveGroupAs.Execute
  else
    fProjectGroup.saveGroup(fProjectGroup.groupFilename);
end;

procedure TCEMainForm.actProjSelUngroupedExecute(Sender: TObject);
begin
  if assigned(fFreeProj) then
    fFreeProj.activate;
end;

procedure TCEMainForm.actNewGroupExecute(Sender: TObject);
begin
  if fProjectGroup.groupModified then
  begin
    if dlgFileChangeClose(fProjectGroup.groupFilename, UnsavedPGrp) = mrCancel then
      exit;
  end;
  fProjectGroup.closeGroup;
end;

procedure TCEMainForm.actProjAddToGroupExecute(Sender: TObject);
begin
  if not assigned(fFreeProj) or fFreeProj.inGroup or
    not fFreeProj.filename.fileExists then
      exit;
  fProjectGroup.addProject(fFreeProj);
  fFreeProj := nil;
end;

// TODO-cprojectsgroup: add a "out of mem" protection in async mode.

procedure TCEMainForm.compileGroup(async: TAsynWait);
var
  i, j: integer;
begin
  if checkProjectLock then
    exit;
  if fProjectGroup.projectCount = 0 then
    exit;
  fGroupCompilationCnt := 0;
  fIsCompilingGroup := true;
  fMsgs.message('start compiling a project group...', nil, amcAll, amkInf);
  if fAppliOpts.showBuildDuration then
    fCompStart := Time;
  for i:= 0 to fProjectGroup.projectCount-1 do
  begin
    fProjectGroup.getProject(i).activate;
    // customized async mode: wait
    if not fProjectGroup.projectIsAsync(i) and (async = awCustom) then
    begin
      while fGroupCompilationCnt <> i do
        Application.ProcessMessages;
      for j:= 0 to i-1 do
        if not fProjectGroup.getProject(j).compiled then
      begin
        fMsgs.message('group compilation has stopped because of a failure',
          nil, amcAll, amkErr);
        fIsCompilingGroup := false;
        break;
      end;
    end;
    fProject.compile;
    // sequential
    if (async = awNo) then
    begin
      while fProjActionsLock do
        Application.ProcessMessages;
      if not fProject.compiled then
      begin
        fMsgs.message('group compilation has stopped because of a failure',
          nil, amcAll, amkErr);
        fIsCompilingGroup := false;
        break;
      end;
    end
  end;
end;

procedure TCEMainForm.actProjGroupCompileExecute(Sender: TObject);
begin
  compileGroup(awYes);
end;

procedure TCEMainForm.actProjGroupCompileSyncExecute(Sender: TObject);
begin
  compileGroup(awNo);
end;

procedure TCEMainForm.actProjGroupCompileCustomSyncExecute(Sender: TObject);
begin
  compileGroup(awCustom);
end;

procedure TCEMainForm.actProjNewGroupExecute(Sender: TObject);
begin
  if fProjectGroup.groupModified then
  begin
    if dlgFileChangeClose(fProjectGroup.groupFilename, UnsavedPGrp) = mrCancel then
      exit;
  end;
  fProjectGroup.closeGroup;
end;
{$ENDREGION}

initialization
  registerClasses([TCEPersistentMainShortcuts, TCEPersistentMainMrus,
    TCELastDocsAndProjs, TCEApplicationOptionsBase, TCEApplicationOptions]);
end.
