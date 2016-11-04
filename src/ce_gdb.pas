unit ce_gdb;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, RegExpr, ComCtrls,
  PropEdits, GraphPropEdits, RTTIGrids, Dialogs, ExtCtrls, Menus, Buttons,
  StdCtrls, ValEdit, process, fpjson, typinfo, {$IFDEF UNIX}Unix,{$ENDIF}
  ce_common, ce_interfaces, ce_widget, ce_processes, ce_observer, ce_synmemo,
  ce_sharedres, ce_stringrange, ce_dsgncontrols, ce_dialogs, ce_dbgitf,
  ce_ddemangle, ce_writableComponent;

type

  {$IFDEF CPU64}
  TCpuRegister = (rax, rbx, rcx, rdx, rsi, rdi, rbp, rsp, r8, r9, r10, r11, r12, r13,
    r14, r15, rip);

  const stOffset = 24;
type
  TFpuRegister = (st0, st1, st2, st3, st4, st5, st6, st7);
  {$ENDIF}

  {$IFDEF CPU32}
  TCpuRegister = (eax, ebx, ecx, edx, esi, edi, ebp, esp, eip);

  const stOffset = 16;
type
  TFpuRegister = (st0, st1, st2, st3, st4, st5, st6, st7);
  {$ENDIF}


  TFLAG = (CF, PF, AF, ZF, SF, TF, IF_, DF, OF_);

  const FlagValues: array[TFlag] of word = (1, 4, 16, 64, 128, 256, 512, 1024, 2048);

type

  TFLAGS = set of TFLAG;

  TSegRegister = (CS, SS, DS, ES, FS, GS);
  {$IFDEF CPU64}
  const segOffset = 18;
  const flagOffset = 17;
  {$ELSE}
  const segOffset = 10;
  const flagOffset = 9;
  {$ENDIF}

type

  // aliased to get hex display in object inspector.
  TCpuGprValue = type PtrUInt;

  // aliased to get hex display in object inspector.
  TCPUSegValue = type word;

  // displays a TCpuRegValue in hex
  TCpuRegValueEditor = class(TIntegerProperty)
  public
    function GetValue: ansistring; override;
    procedure SetValue(const NewValue: ansistring); override;
  end;

  // displays a TCPUSegValue in hex
  TCpuSegValueEditor = class(TIntegerProperty)
  public
    function GetValue: ansistring; override;
    procedure SetValue(const NewValue: ansistring); override;
  end;

  TSetGprEvent = procedure(reg: TCpuRegister; val: TCpuGprValue) of object;

  // Makes a category for the general purpose registers in a object inspector
  TInspectableGPR = class(TPersistent)
  private
    fRegisters: array[TCpuRegister] of TCpuGprValue;
    fSetGprEvent: TSetGprEvent;
    procedure setRegister(index: TCpuRegister; value: TCpuGprValue);
  published
    {$IFDEF CPU64}
    property RAX: TCpuGprValue index TCpuRegister.rax read fRegisters[TCpuRegister.rax] write setRegister;
    property RBX: TCpuGprValue index TCpuRegister.rbx read fRegisters[TCpuRegister.rbx] write setRegister;
    property RCX: TCpuGprValue index TCpuRegister.rcx read fRegisters[TCpuRegister.rcx] write setRegister;
    property RDX: TCpuGprValue index TCpuRegister.rdx read fRegisters[TCpuRegister.rdx] write setRegister;
    property RSI: TCpuGprValue index TCpuRegister.rsi read fRegisters[TCpuRegister.rsi] write setRegister;
    property RDI: TCpuGprValue index TCpuRegister.rdi read fRegisters[TCpuRegister.rdi] write setRegister;
    property RBP: TCpuGprValue index TCpuRegister.rbp read fRegisters[TCpuRegister.rbp] write setRegister;
    property RSP: TCpuGprValue index TCpuRegister.rsp read fRegisters[TCpuRegister.rsp] write setRegister;
    property R8:  TCpuGprValue index TCpuRegister.r8  read fRegisters[TCpuRegister.r8] write setRegister;
    property R9:  TCpuGprValue index TCpuRegister.r9  read fRegisters[TCpuRegister.r9] write setRegister;
    property R10: TCpuGprValue index TCpuRegister.r10 read fRegisters[TCpuRegister.r10] write setRegister;
    property R11: TCpuGprValue index TCpuRegister.r11 read fRegisters[TCpuRegister.r11] write setRegister;
    property R12: TCpuGprValue index TCpuRegister.r12 read fRegisters[TCpuRegister.r12] write setRegister;
    property R13: TCpuGprValue index TCpuRegister.r13 read fRegisters[TCpuRegister.r13] write setRegister;
    property R14: TCpuGprValue index TCpuRegister.r14 read fRegisters[TCpuRegister.r14] write setRegister;
    property R15: TCpuGprValue index TCpuRegister.r15 read fRegisters[TCpuRegister.r15] write setRegister;
    property RIP: TCpuGprValue index TCpuRegister.rip read fRegisters[TCpuRegister.rip] write setRegister;
    {$ELSE}
    property EAX: TCpuGprValue index TCpuRegister.eax read fRegisters[TCpuRegister.eax] write setRegister;
    property EBX: TCpuGprValue index TCpuRegister.ebx read fRegisters[TCpuRegister.ebx] write setRegister;
    property ECX: TCpuGprValue index TCpuRegister.ecx read fRegisters[TCpuRegister.ecx] write setRegister;
    property EDX: TCpuGprValue index TCpuRegister.edx read fRegisters[TCpuRegister.edx] write setRegister;
    property ESI: TCpuGprValue index TCpuRegister.esi read fRegisters[TCpuRegister.esi] write setRegister;
    property EDI: TCpuGprValue index TCpuRegister.edi read fRegisters[TCpuRegister.edi] write setRegister;
    property EBP: TCpuGprValue index TCpuRegister.ebp read fRegisters[TCpuRegister.ebp] write setRegister;
    property ESP: TCpuGprValue index TCpuRegister.esp read fRegisters[TCpuRegister.esp] write setRegister;
    property EIP: TCpuGprValue index TCpuRegister.eip read fRegisters[TCpuRegister.eip] write setRegister;
    {$ENDIF}
  public
    constructor create(eventGPR: TSetGprEvent);
    procedure setInspectableRegister(index: TCpuRegister; value: PtrUInt);
  end;

  TSetSsrEvent = procedure(reg: TSegRegister; val: TCPUSegValue) of object;

  // Makes a category for the segment registers in a object inspector
  TInspectableSSR = class(TPersistent)
  private
    fRegisters: array[TSegRegister] of TCPUSegValue;
    fSetSsrEvent: TSetSsrEvent;
    procedure setRegister(index: TSegRegister; value: TCPUSegValue);
  published
    property CS: TCPUSegValue index TSegRegister.CS read fRegisters[TSegRegister.CS] write setRegister;
    property SS: TCPUSegValue index TSegRegister.SS read fRegisters[TSegRegister.SS] write setRegister;
    property DS: TCPUSegValue index TSegRegister.DS read fRegisters[TSegRegister.DS] write setRegister;
    property ES: TCPUSegValue index TSegRegister.ES read fRegisters[TSegRegister.ES] write setRegister;
    property FS: TCPUSegValue index TSegRegister.FS read fRegisters[TSegRegister.FS] write setRegister;
    property GS: TCPUSegValue index TSegRegister.GS read fRegisters[TSegRegister.GS] write setRegister;
  public
    constructor create(eventSSR: TSetSsrEvent);
    procedure setInspectableRegister(index: TSegRegister; value: TCPUSegValue);
  end;

  TSetFlagEvent = procedure(val: PtrUint) of object;

  TSetFprEvent = procedure(reg: TFpuRegister; val: extended) of object;

  // Makes a category for the floating point unit registers in a object inspector
  TInspectableFPR = class(TPersistent)
  private
    fRegisters: array[TFpuRegister] of extended;
    fSetFprEvent: TSetFprEvent;
    procedure setRegister(index: TFpuRegister; value: extended);
  published
    property ST0: extended index TFpuRegister.st0 read fRegisters[TFpuRegister.st0] write setRegister;
    property ST1: extended index TFpuRegister.st1 read fRegisters[TFpuRegister.st1] write setRegister;
    property ST2: extended index TFpuRegister.st2 read fRegisters[TFpuRegister.st2] write setRegister;
    property ST3: extended index TFpuRegister.st3 read fRegisters[TFpuRegister.st3] write setRegister;
    property ST4: extended index TFpuRegister.st4 read fRegisters[TFpuRegister.st4] write setRegister;
    property ST5: extended index TFpuRegister.st5 read fRegisters[TFpuRegister.st5] write setRegister;
    property ST6: extended index TFpuRegister.st6 read fRegisters[TFpuRegister.st6] write setRegister;
    property ST7: extended index TFpuRegister.st7 read fRegisters[TFpuRegister.st7] write setRegister;
  public
    constructor create(event: TSetFprEvent);
    procedure setInspectableRegister(index: TFpuRegister; value: extended);
  end;

  // Makes a category for the SSE registers in a object inspector
  TInspectableSSE = class(TPersistent)
    // interpretation is a problem:
    // 4 int ? 2 double ? 4 single ? ...
  end;

  // Stores the registers content, to be displayable in an object inspector.
  TInspectableCPU = class(TPersistent)
  private
    fFullFlags: PtrUint;
    fFlags: TFlags;
    fSetFlagEvent: TSetFlagEvent;
    fGpr: TInspectableGPR;
    fFpr: TInspectableFPR;
    fSsr: TInspectableSSR;
    procedure setFlag(value: TFlags);
  published
    property CPU: TInspectableGPR read fGpr;
    property FPU: TInspectableFPR read fFpr;
    property SSR: TInspectableSSR read fSsr;
    property FLAGS: TFlags read fFlags write setFlag;
  public
    constructor create(setGprEvent: TSetGprEvent; setSsrEvent: TSetSsrEvent;
      setFlagEvent: TSetFlagEvent; setFprEvent: TSetFprEvent);
    destructor destroy; override;
    procedure setInspectableFlags(value: PtrUint);
  end;

  // Represents an item in the call stack
  TStackItem = class(TCollectionItem)
  strict private
    fFilename: string;
    fFname: string;
    fAddress: PtrUInt;
    fLine: integer;
  public
    procedure setProperties(addr: PtrUint; fname, nme: string; lne: integer);
    property address: ptruint read fAddress;
    property filename: string read fFilename;
    property line: integer read fLine;
    property name: string read fFname;
  end;

  // The call stack
  TStackItems = class
  strict private
    fItems: TCollection;
  public
    constructor create;
    destructor destroy; override;
    procedure assignToList(list: TListView);
    procedure addItem(addr: PtrUint; fname, nme: string; lne: integer);
    procedure clear;
  end;

  // TODO-cGDB: assembly view

  // serializable breakpoint
  TPersistentBreakPoint = class(TCollectionItem)
  strict private
    fFilename: string;
    fLine: integer;
    fKind: TBreakPointKind;
  published
    property filename: string read fFilename write fFilename;
    property line: integer read fLine write fLine;
    property kind: TBreakPointKind read fKind write fKind;
  end;

  // allow to retrieve the breakpoints even if source is not openened.
  TPersistentBreakPoints = class(TWritableLfmTextComponent)
  strict private
    fItems: TOwnedCollection;
    procedure setItems(value: TOwnedCollection);
    function getItem(index: integer): TPersistentBreakPoint;
    function find(const fname: string; line: integer; kind: TBreakPointKind): boolean;
  published
    property items: TOwnedCollection read fItems write setItems;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    function count: integer;
    procedure clearFile(const fname: string);
    procedure deleteItem(const fname: string; line: integer; kind: TBreakPointKind);
    procedure addItem(const fname: string; line: integer; kind: TBreakPointKind);
    property item[index: integer]: TPersistentBreakPoint read getItem; default;
  end;

  // Makes a category for the shortcuts in the option editor.
  TCEDebugShortcuts = class(TPersistent)
  private
    fStart, fStop, fPause, fContinue, fStep, fStepOver, fStack, fRegs,
      fVariables: TShortCut;
  published
    property start: TShortCut read fStart write fStart;
    property stop: TShortCut read fStop write fStop;
    property pause: TShortCut read fPause write fPause;
    property continue: TShortcut read fContinue write fContinue;
    property step: TShortCut read fStep write fStep;
    property stepOver: TShortCut read fStepOver write fStepOver;
    property updateStack: TShortCut read fStack write fStack;
    property updateRegisters: TShortCut read fRegs write fRegs;
    property updateVariables: TShortCut read fVariables write fVariables;
  public
    procedure assign(source: TPersistent); override;
  end;

  TCEDebugOptionsBase = class(TWritableLfmTextComponent)
  private
    fAutoDemangle: boolean;
    fAutoGetCallStack: boolean;
    fAutoGetRegisters: boolean;
    fAutoGetVariables: boolean;
    fCommandsHistory: TStringList;
    fIgnoredSignals: TStringList;
    fShowGdbOutput: boolean;
    fShowOutput: boolean;
    fShortcuts: TCEDebugShortcuts;
    procedure setIgnoredSignals(value: TStringList);
    procedure setCommandsHistory(value: TStringList);
    procedure setShortcuts(value: TCEDebugShortcuts);
  published
    property autoDemangle: boolean read fAutoDemangle write fAutoDemangle;
    property autoGetCallStack: boolean read fAutoGetCallStack write fAutoGetCallStack;
    property autoGetRegisters: boolean read fAutoGetRegisters write fAutoGetRegisters;
    property autoGetVariables: boolean read fAutoGetVariables write fAutoGetVariables;
    property commandsHistory: TStringList read fCommandsHistory write setCommandsHistory;
    property ignoredSignals: TStringList read fIgnoredSignals write setIgnoredSignals;
    property shortcuts: TCEDebugShortcuts read fShortcuts write setShortcuts;
    property showGdbOutput: boolean read fShowGdbOutput write fShowGdbOutput;
    property showOutput: boolean read fShowOutput write fShowOutput;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    procedure assign(source: TPersistent); override;
  end;

  TCEDebugOptions = class(TCEDebugOptionsBase, ICEEditableOptions)
  private
    FonChangesApplied: TNotifyEvent;
    fBackup: TCEDebugOptionsBase;
    function optionedWantCategory(): string;
    function optionedWantEditorKind: TOptionEditorKind;
    function optionedWantContainer: TPersistent;
    procedure optionedEvent(event: TOptionEditorEvent);
    function optionedOptionsModified: boolean;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    property onChangesApplied: TNotifyEvent read FonChangesApplied write FonChangesApplied;
  end;

  TGdbState = (gsNone, gsRunning, gsPaused);

  TAddWatchPointKind = (wpkRead, wpkWrite, wpkReadWrite);

  { TCEGdbWidget }
  TCEGdbWidget = class(TCEWidget, ICEProjectObserver, ICEDocumentObserver, ICEDebugger, ICEMainMenuProvider)
    btnContinue: TCEToolButton;
    btnVariables: TCEToolButton;
    btnNext: TCEToolButton;
    btnOver: TCEToolButton;
    btnPause: TCEToolButton;
    btnReg: TCEToolButton;
    btnStack: TCEToolButton;
    btnStop: TCEToolButton;
    btnStart: TCEToolButton;
    btnWatch: TCEToolButton;
    button4: TCEToolButton;
    Edit1: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    lstCallStack: TListView;
    mnuReadW: TMenuItem;
    mnuWriteW: TMenuItem;
    mnuReadWriteW: TMenuItem;
    mnuSelProj: TMenuItem;
    mnuSelRunnable: TMenuItem;
    Panel1: TPanel;
    Panel3: TPanel;
    btnSendCom: TSpeedButton;
    cpuVIewer: TTIPropertyGrid;
    mnuProjRunnable: TPopupMenu;
    mnuWatch: TPopupMenu;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    Splitter4: TSplitter;
    ValueListEditor1: TValueListEditor;
    procedure btnContClick(Sender: TObject);
    procedure btnVariablesClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnOverClick(Sender: TObject);
    procedure btnPauseClick(Sender: TObject);
    procedure btnRegClick(Sender: TObject);
    procedure btnSendComClick(Sender: TObject);
    procedure btnStackClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnWatchClick(Sender: TObject);
    procedure Edit1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lstCallStackDblClick(Sender: TObject);
    procedure mnuReadWClick(Sender: TObject);
    procedure mnuReadWriteWClick(Sender: TObject);
    procedure mnuSelProjClick(Sender: TObject);
    procedure mnuSelRunnableClick(Sender: TObject);
    procedure mnuWriteWClick(Sender: TObject);
  protected
    procedure setToolBarFlat(value: boolean); override;
  private
    fSyms: ICESymStringExpander;
    fExe: string;
    fOutputName: string;
    fInputName: string;
    fShowFromCustomCommand: boolean;
    fUpdateMenu: boolean;
    fGdbState: TGdbState;
    fSubj: TCEDebugObserverSubject;
    fDoc: TCESynMemo;
    fDbgRunnable: boolean;
    fProj: ICECommonProject;
    fJson: TJsonObject;
    fLog: TStringList;
    fDocHandler: ICEMultiDocHandler;
    fMsg: ICEMessagesDisplay;
    fGdb: TCEProcess;
    fOutput: TFileStream;
    fInput: TFileStream;
    fInspState: TInspectableCPU;
    fStackItems: TStackItems;
    fCatchPause: boolean;
    fOptions: TCEDebugOptions;
    fAddWatchPointKind: TAddWatchPointKind;
    fBreakPoints: TPersistentBreakPoints;
    //
    procedure optionsChangesApplied(sender: TObject);
    procedure menuDeclare(item: TMenuItem);
    procedure menuUpdate(item: TMenuItem);
    //
    procedure disableEditor;
    procedure setState(value: TGdbState);
    procedure updateButtonsState;
    procedure startDebugging;
    procedure killGdb;
    procedure storeObserversBreakpoints;
    // GDB output processors
    procedure gdboutQuiet(sender: TObject);
    procedure gdboutJsonize(sender: TObject);
    procedure interpretJson;
    // GDB commands & actions
    procedure gdbCommand(aCommand: string; gdbOutProcessor: TNotifyEvent = nil);
    procedure infoRegs;
    procedure infoStack;
    procedure infoVariables;
    procedure sendCustomCommand;
    procedure setGpr(reg: TCpuRegister; val: TCpuGprValue);
    procedure setFpr(reg: TFpuRegister; val: extended);
    procedure setSsr(reg: TSegRegister; val: TCPUSegValue);
    procedure setFlag(val: PtrUint);
    procedure readOutput;
    //
    procedure projNew(project: ICECommonProject);
    procedure projChanged(project: ICECommonProject);
    procedure projClosing(project: ICECommonProject);
    procedure projFocused(project: ICECommonProject);
    procedure projCompiling(project: ICECommonProject);
    procedure projCompiled(project: ICECommonProject; success: boolean);
    //
    procedure docNew(document: TCESynMemo);
    procedure docFocused(document: TCESynMemo);
    procedure docChanged(document: TCESynMemo);
    procedure docClosing(document: TCESynMemo);
    //
    function running: boolean;
    function singleServiceName: string;
    procedure addBreakPoint(const fname: string; line: integer;
      kind: TBreakPointKind = bpkBReak);
    procedure removeBreakPoint(const fname: string; line: integer;
      kind: TBreakPointKind = bpkBreak);
    procedure executeFromShortcut(sender: TObject);
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
  end;

  // Perfect static hash-set that detect a GDB stop reason
  stopReasons = record
  public
  private
    const fWords: array [0..15] of string =
    (
      'exec', 'access-watchpoint-trigger', 'location-reached', 'syscall-return',
      'vfork', 'syscall-entry', 'watchpoint-trigger', '', 'read-watchpoint-trigger',
      'breakpoint-hit', 'end-stepping-range', 'function-finished', '', 'fork',
      'solib-event', ''
    );
    const fHasEntry: array [0..15] of boolean =
    (
      true, true, true, true, true, true, true, false, true, true, true, true,
      false, true, true, false);
    const fCoeffs: array[0..255] of Byte =
    (
      3, 89, 191, 109, 130, 168, 79, 148, 40, 57, 142, 200, 143, 109, 144, 128,
      90, 119, 44, 16, 170, 160, 252, 48, 142, 79, 188, 18, 124, 157, 218, 14,
      108, 236, 45, 97, 190, 119, 37, 44, 103, 121, 20, 36, 149, 200, 122, 188,
      222, 195, 201, 15, 30, 183, 145, 110, 21, 186, 66, 71, 229, 247, 179, 169,
      169, 212, 55, 0, 44, 189, 223, 250, 253, 23, 140, 204, 155, 114, 139, 39,
      189, 35, 218, 30, 222, 168, 40, 203, 20, 208, 146, 226, 122, 200, 28, 223,
      116, 208, 151, 27, 16, 253, 107, 207, 71, 102, 215, 69, 202, 175, 103, 240,
      179, 198, 173, 120, 47, 48, 199, 52, 203, 207, 21, 80, 103, 33, 254, 46,
      218, 25, 47, 131, 221, 239, 44, 33, 165, 73, 143, 121, 73, 23, 76, 159, 199,
      172, 144, 236, 161, 249, 178, 45, 49, 157, 246, 43, 227, 145, 139, 161, 185,
      15, 68, 254, 112, 132, 133, 5, 41, 149, 116, 82, 57, 156, 193, 250, 73, 108,
      126, 1, 44, 151, 211, 197, 23, 225, 119, 247, 59, 20, 225, 241, 232, 192,
      241, 1, 7, 12, 73, 160, 157, 14, 203, 109, 9, 17, 43, 20, 14, 174, 233, 108,
      254, 204, 78, 224, 16, 15, 133, 251, 254, 204, 191, 12, 0, 131, 19, 252,
      104, 178, 231, 176, 22, 234, 104, 181, 167, 17, 103, 23, 156, 197, 249, 237,
      109, 53, 170, 237, 57, 126, 48, 119, 175, 238, 141, 188
    );
    class function hash(const w: string): Byte; static; {$IFNDEF DEBUG}inline;{$ENDIF}
  public
    class function match(const w: string): PString; static; {$IFNDEF DEBUG}inline;{$ENDIF}
  end;

implementation
{$R *.lfm}

{$REGION TCEDebugOption --------------------------------------------------------}
const optFname = 'gdbcommander.txt';
const bpFname = 'breakpoints.txt';


procedure TCEDebugShortcuts.assign(source: TPersistent);
var
  src: TCEDebugShortcuts;
begin
  if source is TCEDebugShortcuts then
  begin
    src := TCEDebugShortcuts(source);
    fStart    := src.fStart;
    fStop     := src.fStop;
    fPause    := src.fPause;
    fContinue := src.fContinue;
    fStep     := src.fStep;
    fStepOver := src.fStepOver;
    fStack    := src.fStack;
    fRegs     := src.fRegs;
    fVariables:= src.fVariables;
  end
  else inherited;
end;

constructor TCEDebugOptionsBase.create(aOwner: TComponent);
begin
  inherited;
  fAutoDemangle := true;
  fAutoGetCallStack:= true;
  fAutoGetRegisters:= true;
  fAutoGetVariables:= true;
  fShowGdbOutput:=true;
  fIgnoredSignals := TStringList.Create;
  fIgnoredSignals.Duplicates:= dupIgnore;
  fIgnoredSignals.Sorted:=true;
  fCommandsHistory := TStringList.Create;
  fCommandsHistory.Duplicates:= dupIgnore;
  fCommandsHistory.Sorted:=true;
  fShortcuts := TCEDebugShortcuts.Create;
end;

destructor TCEDebugOptionsBase.destroy;
begin
  fIgnoredSignals.Free;
  fCommandsHistory.Free;
  fShortcuts.Free;
  inherited;
end;

procedure TCEDebugOptionsBase.setIgnoredSignals(value: TStringList);
begin
  fIgnoredSignals.Assign(value);
end;

procedure TCEDebugOptionsBase.setCommandsHistory(value: TStringList);
begin
  fCommandsHistory.Assign(value);
end;

procedure TCEDebugOptionsBase.setShortcuts(value: TCEDebugShortcuts);
begin
  fShortcuts.assign(value);
end;

procedure TCEDebugOptionsBase.assign(source: TPersistent);
var
  src: TCEDebugOptionsBase;
begin
  if source is TCEDebugOptionsBase then
  begin
    src := TCEDebugOptionsBase(source);
    fAutoDemangle:=src.fAutoDemangle;
    fAutoGetCallStack:=src.fAutoGetCallStack;
    fAutoGetRegisters:=src.fAutoGetRegisters;
    fAutoGetVariables:=src.autoGetVariables;
    fShowGdbOutput:=src.fShowGdbOutput;
    fShowOutput:=src.fShowOutput;
    fIgnoredSignals.Assign(src.fIgnoredSignals);
    fCommandsHistory.Assign(src.fCommandsHistory);
    fShortcuts.assign(src.fShortcuts);
  end
  else inherited;
end;

constructor TCEDebugOptions.create(aOwner: TComponent);
var
  fname: string;
begin
  inherited;
  fBackup := TCEDebugOptionsBase.create(self);
  fname := getCoeditDocPath + optFname;
  if fname.fileExists then
    loadFromFile(fname);
  EntitiesConnector.addObserver(self);
  fShowGdbOutput:=false;
  fShowOutput:= true;
  fAutoDemangle:= true;
  fAutoGetCallStack:= true;
  fAutoGetRegisters:= true;
  fAutoGetVariables:= true;
end;

destructor TCEDebugOptions.destroy;
begin
  saveToFile(getCoeditDocPath + optFname);
  EntitiesConnector.removeObserver(self);
  inherited;
end;

function TCEDebugOptions.optionedWantCategory(): string;
begin
  exit('Debugger');
end;

function TCEDebugOptions.optionedWantEditorKind: TOptionEditorKind;
begin
  exit(oekGeneric);
end;

function TCEDebugOptions.optionedWantContainer: TPersistent;
begin
  exit(self);
end;

procedure TCEDebugOptions.optionedEvent(event: TOptionEditorEvent);
begin
  case event of
    oeeSelectCat: fBackup.assign(self);
    oeeCancel: assign(fBackup);
    oeeAccept:
    begin
      fBackup.assign(self);
      if assigned(FonChangesApplied) then
        FonChangesApplied(self);
    end;
  end;
end;

function TCEDebugOptions.optionedOptionsModified: boolean;
begin
  exit(false);
end;
{$ENDREGION}

{$REGION TPersistentBreakpoints ------------------------------------------------}

constructor TPersistentBreakPoints.create(aOwner: TComponent);
var
  fname: string;
begin
  Inherited;
  fItems := TOwnedCollection.Create(self, TPersistentBreakPoint);
  fname := getCoeditDocPath + bpFname;
  if fname.fileExists then
    loadFromFile(fname);
end;

destructor TPersistentBreakPoints.destroy;
begin
  if fItems.Count > 0 then
    saveToFile(getCoeditDocPath + bpFname);
  inherited;
end;

procedure TPersistentBreakPoints.setItems(value: TOwnedCollection);
begin
  fItems.Assign(value);
end;

function TPersistentBreakPoints.getItem(index: integer): TPersistentBreakPoint;
begin
  exit(TPersistentBreakPoint(fItems.Items[index]));
end;

function TPersistentBreakPoints.count: integer;
begin
  exit(fItems.Count);
end;

function TPersistentBreakPoints.find(const fname: string; line: integer; kind: TBreakPointKind): boolean;
var
  i: integer;
  b: TPersistentBreakPoint;
begin
  result := false;
  for i := 0 to fItems.Count-1 do
  begin
    b := item[i];
    if (b.filename = fname) and (b.line = line) and (b.kind = kind) then
      exit(true);
  end;
end;

procedure TPersistentBreakPoints.addItem(const fname: string; line: integer; kind: TBreakPointKind);
var
  b: TPersistentBreakPoint;
begin
  if not find(fname, line, kind) then
  begin
    b := TPersistentBreakPoint(fItems.Add);
    b.filename:=fname;
    b.line:=line;
    b.kind:=kind;
  end;
end;

procedure TPersistentBreakPoints.deleteItem(const fname: string; line: integer; kind: TBreakPointKind);
var
  i: integer;
  b: TPersistentBreakPoint;
begin
  for i := fItems.Count-1 downto 0 do
  begin
    b := item[i];
    if (b.filename = fname) and (b.line = line) and (b.kind = kind) then
    begin
      fItems.Delete(i);
      break;
    end;
  end;
end;

procedure TPersistentBreakPoints.clearFile(const fname: string);
var
  i: integer;
  b: TPersistentBreakPoint;
begin
  for i:= fItems.Count-1 downto 0 do
  begin
    b := item[i];
    if b.filename = fname then
      fItems.Delete(i);
  end;
end;
{$ENDREGION}

{$REGION TStackItem/TStackItems ------------------------------------------------}
procedure TStackItem.setProperties(addr: PtrUint; fname, nme: string; lne: integer);
begin
  fAddress:=addr;
  fLine:=lne;
  fFilename:=fname;
  fFname:= nme;
end;

constructor TStackItems.create;
begin
  fItems := TCollection.Create(TStackItem);
end;

destructor TStackItems.destroy;
begin
  fItems.Free;
  inherited;
end;

procedure TStackItems.assignToList(list: TListView);
var
  i: integer;
  litm: TListItem;
  sitm: TStackItem;
begin
  list.Clear;
  for i:= 0 to fItems.Count-1 do
  begin
    litm := list.Items.Add;
    sitm := TStackItem(fItems.Items[i]);
    litm.Caption := sitm.name;
    {$IFDEF CPU64}
    litm.SubItems.Add(format('0x%.16X', [sitm.address]));
    {$ELSE}
    litm.SubItems.Add(format('0x%.8X', [sitm.address]));
    {$ENDIF}
    litm.SubItems.Add(shortenPath(sitm.filename));
    litm.Data:=sitm;
  end;
end;

procedure TStackItems.addItem(addr: PtrUint; fname, nme: string; lne: integer);
begin
  TStackItem(fItems.Add).setProperties(addr, fname, nme, lne);
end;

procedure TStackItems.clear;
begin
  fItems.Clear;
end;
{$ENDREGION}

{$REGION TInspectableCPU -------------------------------------------------------}
function TCpuRegValueEditor.GetValue: ansistring;
begin
  {$IFDEF CPU64}
  result := '0x' + IntToHex(GetInt64Value, 16);
  {$ELSE}
  result := '0x' + IntToHex(GetOrdValue, 8);
  {$ENDIF}
end;

procedure TCpuRegValueEditor.SetValue(const NewValue: ansistring);
begin
  try
    {$IFDEF CPU64}
    SetInt64Value(StrToQWord(NewValue));
    {$ELSE}
    SetOrdValue(StrToInt(NewValue));
    {$ENDIF}
  except
  end;
end;

function TCpuSegValueEditor.GetValue: ansistring;
begin
  result := '0x' + IntToHex(GetOrdValue, 4);
end;

procedure TCpuSegValueEditor.SetValue(const NewValue: ansistring);
begin
  try
    SetOrdValue(StrToInt(NewValue));
  except
  end;
end;

constructor TInspectableGPR.create(eventGPR: TSetGprEvent);
begin
  fSetGprEvent:=eventGPR;
end;

procedure TInspectableGPR.setInspectableRegister(index: TCpuRegister; value: PtrUInt);
begin
  fRegisters[index] := value;
end;

procedure TInspectableGPR.setRegister(index: TCpuRegister; value: TCpuGprValue);
begin
  fSetGprEvent(index, value);
  fRegisters[index] := value;
end;

constructor TInspectableSSR.create(eventSSR: TSetSsrEvent);
begin
  fSetSsrEvent:=eventSSR;
end;

procedure TInspectableSSR.setInspectableRegister(index: TSegRegister; value: TCPUSegValue);
begin
  fRegisters[index] := value;
end;

procedure TInspectableSSR.setRegister(index: TSegRegister; value: TCPUSegValue);
begin
  fSetSsrEvent(index, value);
  fRegisters[index] := value;
end;

constructor TInspectableFPR.create(event: TSetFprEvent);
begin
  fSetFprEvent:=event;
end;

procedure TInspectableFPR.setInspectableRegister(index: TFpuRegister; value: extended);
begin
  fRegisters[index] := value;
end;

procedure TInspectableFPR.setRegister(index: TFpuRegister; value: extended);
begin
  fSetFprEvent(index, value);
  fRegisters[index] := value;
end;

constructor TInspectableCPU.create(setGprEvent: TSetGprEvent; setSsrEvent: TSetSsrEvent;
  setFlagEvent: TSetFlagEvent; setFprEvent: TSetFprEvent);
begin
  fSetFlagEvent:=setFlagEvent;
  fGpr := TInspectableGPR.create(setGprEvent);
  fSsr := TInspectableSSR.create(setSsrEvent);
  fFpr := TInspectableFPR.create(setFprEvent);
end;

destructor TInspectableCPU.destroy;
begin
  fGpr.Free;
  fFPr.Free;
  fSSr.Free;
  inherited;
end;

procedure TInspectableCPU.setInspectableFlags(value: PtrUint);
var
  flg: TFlag;
begin
  if fFullFlags = value then
    exit;
  fFullFlags:=value;
  fFlags:= [];
  for flg in TFlag do
    if (value and FlagValues[flg]) >= FlagValues[flg] then
      fFlags += [flg];
end;

procedure TInspectableCPU.setFlag(value: TFlags);
var
  flg: TFlag;
begin
  if fFlags = value then
    exit;
  for flg in TFlag do
    if (flg in value) <> (flg in fFlags) then
      fFullFlags:= fFullFlags xor FlagValues[flg];
  fFlags := value;
  fSetFlagEvent(fFullFlags);
end;
{$ENDREGION}

{$REGION StopReasons -----------------------------------------------------------}
{$IFDEF DEBUG}{$PUSH}{$R-}{$ENDIF}
class function stopReasons.hash(const w: string): Byte;
var
  i: integer;
begin
  Result := 0;
  for i := 1 to length(w) do
    Result += fCoeffs[Byte(w[i])];
  Result := Result and $F;
end;
{$IFDEF DEBUG}{$POP}{$ENDIF}

class function stopReasons.match(const w: string): PString;
var
  h: Byte;
begin
  result := nil;
  if (length(w) < 4) or (length(w) > 25) then
    exit;
  h := hash(w);
  if fHasEntry[h] and (fWords[h] = w) then
    result := @fWords[h];
end;
{$ENDREGION}

{$REGION Common/standard comp --------------------------------------------------}
constructor TCEGdbWidget.create(aOwner: TComponent);
begin
  inherited;
  EntitiesConnector.addObserver(self);
  EntitiesConnector.addSingleService(self);
  fSyms := getSymStringExpander;
  fDocHandler:= getMultiDocHandler;
  fMsg:= getMessageDisplay;
  fLog := TStringList.Create;
  fInspState := TInspectableCPU.Create(@setGpr, @setSsr, @setFlag, @setFpr);
  cpuVIewer.TIObject := fInspState;
  fJson := TJsonObject.Create;
  fStackItems := TStackItems.create;
  fSubj:= TCEDebugObserverSubject.Create;
  fOptions:= TCEDebugOptions.create(self);
  fOptions.onChangesApplied:=@optionsChangesApplied;
  Edit1.Items.Assign(fOptions.commandsHistory);
  fAddWatchPointKind := wpkWrite;
  fBreakPoints := TPersistentBreakPoints.create(self);
  //
  AssignPng(btnSendCom, 'ACCEPT');
  setState(gsNone);
end;

destructor TCEGdbWidget.destroy;
begin
  fOutput.Free;
  fOptions.commandsHistory.Assign(edit1.Items);
  fOptions.Free;
  fLog.Free;
  killGdb;
  fInspState.Free;
  fJson.Free;
  fStackItems.Free;
  EntitiesConnector.removeObserver(self);
  fSubj.free;
  inherited;
end;

procedure TCEGdbWidget.setToolBarFlat(value: boolean);
begin
  inherited setToolBarFLat(value);
  btnSendCom.Flat:=value;
end;

procedure TCEGdbWidget.menuDeclare(item: TMenuItem);
var
  itm: TMenuItem;
  bmp: TBitmap;
  i: integer;
begin
  item.Caption:='Debugger';
  item.Clear;

  bmp := TBitmap.Create;

  itm := TMenuItem.Create(item);
  itm.ShortCut:=fOptions.shortcuts.start;
  itm.Caption:='Start';
  itm.OnClick:= @executeFromShortcut;
  itm.Tag:=0;
  item.Add(itm);
  btnStart.toBitmap(bmp);
  itm.Bitmap.Assign(bmp);
  i := item.GetImageList.Add(bmp, nil);
  itm.ImageIndex:= i;

  itm := TMenuItem.Create(item);
  itm.ShortCut:=fOptions.shortcuts.stop;
  itm.Caption:='Stop';
  itm.OnClick:= @executeFromShortcut;
  itm.Tag:=1;
  item.Add(itm);
  btnStop.toBitmap(bmp);
  itm.Bitmap.Assign(bmp);
  i := item.GetImageList.Add(bmp, nil);
  itm.ImageIndex:= i;

  itm := TMenuItem.Create(item);
  itm.ShortCut:=fOptions.shortcuts.pause;
  itm.Caption:='Pause';
  itm.OnClick:= @executeFromShortcut;
  itm.Tag:=2;
  item.Add(itm);
  btnPause.toBitmap(bmp);
  itm.Bitmap.Assign(bmp);
  i := item.GetImageList.Add(bmp, nil);
  itm.ImageIndex:= i;

  itm := TMenuItem.Create(item);
  itm.ShortCut:=fOptions.shortcuts.continue;
  itm.Caption:='Continue';
  itm.OnClick:= @executeFromShortcut;
  itm.Tag:=3;
  item.Add(itm);
  btnContinue.toBitmap(bmp);
  itm.Bitmap.Assign(bmp);
  i := item.GetImageList.Add(bmp, nil);
  itm.ImageIndex:= i;

  itm := TMenuItem.Create(item);
  itm.ShortCut:=fOptions.shortcuts.step;
  itm.Caption:='Step';
  itm.OnClick:= @executeFromShortcut;
  itm.Tag:=4;
  item.Add(itm);
  btnNext.toBitmap(bmp);
  itm.Bitmap.Assign(bmp);
  i := item.GetImageList.Add(bmp, nil);
  itm.ImageIndex:= i;

  itm := TMenuItem.Create(item);
  itm.ShortCut:=fOptions.shortcuts.stepOver;
  itm.Caption:='Step over';
  itm.OnClick:= @executeFromShortcut;
  itm.Tag:=5;
  item.Add(itm);
  btnOver.toBitmap(bmp);
  itm.Bitmap.Assign(bmp);
  i := item.GetImageList.Add(bmp, nil);
  itm.ImageIndex:= i;

  itm := TMenuItem.Create(item);
  itm.Caption:= '-';
  itm.Tag:=-1;
  item.Add(itm);

  itm := TMenuItem.Create(item);
  itm.ShortCut:=fOptions.shortcuts.updateRegisters;
  itm.Caption:='Update registers';
  itm.OnClick:= @executeFromShortcut;
  itm.Tag:=6;
  item.Add(itm);
  btnReg.toBitmap(bmp);
  itm.Bitmap.Assign(bmp);
  i := item.GetImageList.Add(bmp, nil);
  itm.ImageIndex:= i;

  itm := TMenuItem.Create(item);
  itm.ShortCut:=fOptions.shortcuts.updateStack;
  itm.Caption:='Update call stack';
  itm.OnClick:= @executeFromShortcut;
  itm.Tag:=7;
  item.Add(itm);
  btnStack.toBitmap(bmp);
  itm.Bitmap.Assign(bmp);
  i := item.GetImageList.Add(bmp, nil);
  itm.ImageIndex:= i;

  itm := TMenuItem.Create(item);
  itm.ShortCut:=fOptions.shortcuts.updateVariables;
  itm.Caption:='Update the variables';
  itm.OnClick:= @executeFromShortcut;
  itm.Tag:=8;
  item.Add(itm);
  btnVariables.toBitmap(bmp);
  itm.Bitmap.Assign(bmp);
  i := item.GetImageList.Add(bmp, nil);
  itm.ImageIndex:= i;

  bmp.Free;
end;

procedure TCEGdbWidget.menuUpdate(item: TMenuItem);
var
  i: integer;
  itm: TMenuItem;
begin
  if item.isNil or not fUpdateMenu then
    exit;
  fUpdateMenu := false;
  for i:= 0 to item.Count-1 do
  begin
    itm := item.Items[i];
    case itm.Tag of
      0: itm.ShortCut:=fOptions.shortcuts.start;
      1: itm.ShortCut:=fOptions.shortcuts.stop;
      2: itm.ShortCut:=fOptions.shortcuts.pause;
      3: itm.ShortCut:=fOptions.shortcuts.continue;
      4: itm.ShortCut:=fOptions.shortcuts.step;
      5: itm.ShortCut:=fOptions.shortcuts.stepOver;
      6: itm.ShortCut:=fOptions.shortcuts.updateRegisters;
      7: itm.ShortCut:=fOptions.shortcuts.updateStack;
      8: itm.ShortCut:=fOptions.shortcuts.updateVariables;
    end;
  end;
end;

procedure TCEGdbWidget.optionsChangesApplied(sender: TObject);
begin
  fUpdateMenu:=true;
end;

procedure TCEGdbWidget.executeFromShortcut(sender: TObject);
begin
  case TMenuItem(sender).Tag of
    0: btnStart.Click;
    1: btnStop.Click;
    2: btnPause.Click;
    3: btnContinue.Click;
    4: btnNext.Click;
    5: btnOver.Click;
    6: btnReg.Click;
    7: btnStack.Click;
    8: btnVariables.Click;
  end;
end;
{$ENDREGION}

{$REGION ICEProjectObserver ----------------------------------------------------}
procedure TCEGdbWidget.projNew(project: ICECommonProject);
begin
  fProj := project;
end;

procedure TCEGdbWidget.projChanged(project: ICECommonProject);
begin
  if fProj <> project then
    exit;
end;

procedure TCEGdbWidget.projClosing(project: ICECommonProject);
begin
  if fProj <> project then
    exit;
  fProj := nil;
  if not fDbgRunnable then
  begin
    if fOutputName.fileExists then
      deleteFile(fOutputName);
    if fInputName.fileExists then
      deleteFile(fInputName);
  end;
end;

procedure TCEGdbWidget.projFocused(project: ICECommonProject);
begin
  fProj := project;
end;

procedure TCEGdbWidget.projCompiling(project: ICECommonProject);
begin
end;

procedure TCEGdbWidget.projCompiled(project: ICECommonProject; success: boolean);
begin
end;
{$ENDREGION}

{$REGION ICEDocumentObserver ---------------------------------------------------}
procedure TCEGdbWidget.docNew(document: TCESynMemo);
begin
end;

procedure TCEGdbWidget.docFocused(document: TCESynMemo);
begin
  fDoc := document;
end;

procedure TCEGdbWidget.docChanged(document: TCESynMemo);
begin
end;

procedure TCEGdbWidget.docClosing(document: TCESynMemo);
begin
  if fDoc <> document then
    exit;
  fDoc := nil;
  if fDbgRunnable then
  begin
    if fOutputName.fileExists then
      deleteFile(fOutputName);
    if fInputName.fileExists then
      deleteFile(fInputName);
  end;
end;
{$ENDREGION}

{$REGION Unsorted Debugging things ---------------------------------------------}
function TCEGdbWidget.running: boolean;
begin
  if assigned(fGdb) then
    exit(fGdb.Running)
  else
    exit(false);
end;

function TCEGdbWidget.singleServiceName: string;
begin
  exit('ICEDebugger');
end;

procedure TCEGdbWidget.killGdb;
begin
  if not assigned(fGdb) then
    exit;
  if fGdb.Running then
    fGdb.Terminate(0);
  FreeAndNil(fGdb);
end;

procedure TCEGdbWidget.storeObserversBreakpoints;
var
  i,j: integer;
  obs: ICEDebugObserver;
  nme: string;
  lne: integer;
  knd: TBreakPointKind;
begin
  for i:= 0 to fSubj.observersCount-1 do
  begin
    obs := fSubj.observers[i] as ICEDebugObserver;
    for j := 0 to obs.debugQueryBpCount-1 do
    begin
      obs.debugQueryBreakPoint(j, nme, lne, knd);
      fBreakPoints.addItem(nme, lne, knd);
    end;
  end;
end;

procedure TCEGdbWidget.addBreakPoint(const fname: string; line: integer;
  kind: TBreakPointKind = bpkBreak);
begin
  if assigned(fBreakPoints) then
    fBreakPoints.addItem(fname, line, kind);
  if fGdb.isNil or not fGdb.Running then
    exit;
  gdbCommand('break ' + fname + ':' + intToStr(line));
end;

procedure TCEGdbWidget.removeBreakPoint(const fname: string; line: integer;
  kind: TBreakPointKind = bpkBreak);
begin
  if assigned(fBreakPoints) then
    fBreakPoints.deleteItem(fname, line, kind);
  if fGdb.isNil or not fGdb.Running then
    exit;
  gdbCommand('clear ' + fname + ':' + intToStr(line));
end;

procedure TCEGdbWidget.setState(value: TGdbState);
begin
  if fGdbState = value then
    exit;
  fGdbState:=value;
  updateButtonsState;
end;

procedure TCEGdbWidget.updateButtonsState;
begin
  case fGdbState of
    gsNone:
    begin
      btnStart.Enabled:=true;
      btnStop.Enabled:=false;
      btnPause.Enabled:=false;
      btnContinue.Enabled:=false;
      btnNext.Enabled:=false;
      btnOver.Enabled:=false;
      btnReg.Enabled:=false;
      btnVariables.Enabled:=false;
      btnStack.Enabled:=false;
    end;
    gsPaused:
    begin
      btnStart.Enabled:=false;
      btnStop.Enabled:=true;
      btnPause.Enabled:=false;
      btnContinue.Enabled:=true;
      btnNext.Enabled:=true;
      btnOver.Enabled:=true;
      btnReg.Enabled:=true;
      btnVariables.Enabled:=true;
      btnStack.Enabled:=true;
    end;
    gsRunning:
    begin
      btnStart.Enabled:=false;
      btnStop.Enabled:=true;
      btnPause.Enabled:=true;
      btnContinue.Enabled:=false;
      btnNext.Enabled:=false;
      btnOver.Enabled:=false;
      btnReg.Enabled:=false;
      btnVariables.Enabled:=false;
      btnStack.Enabled:=false;
    end;
  end;
end;

procedure TCEGdbWidget.mnuSelProjClick(Sender: TObject);
begin
  fDbgRunnable := false;
  mnuSelRunnable.Checked:=false;
end;

procedure TCEGdbWidget.mnuSelRunnableClick(Sender: TObject);
begin
  fDbgRunnable := true;
  mnuSelProj.Checked:=false;
end;

procedure TCEGdbWidget.mnuWriteWClick(Sender: TObject);
begin
  fAddWatchPointKind := wpkWrite;
  mnuReadW.Checked:=false;
  mnuReadWriteW.Checked:=false;
end;

procedure TCEGdbWidget.disableEditor;
begin
  cpuVIewer.ItemIndex:=-1;
end;

procedure TCEGdbWidget.startDebugging;
var
  str: string;
  gdb: string;
  i: integer;
  b: TPersistentBreakPoint;
begin
  if not fDbgRunnable and (fProj = nil) then
    exit;
  if fDbgRunnable and fDoc.isNil then
    exit;
  if fProj.binaryKind <> executable then
    exit;
  if not fDbgRunnable then
    fExe := fProj.outputFilename
  else
    fExe := fDoc.fileName.stripFileExt + exeExt;
  //
  fOutputName := fExe + '.inferiorout';
  fInputName  := fExe + '.inferiorin';
  FreeAndNil(fInput);
  if fInputName.fileExists then
    deletefile(fInputName);
  fInput:= TFileStream.Create(fInputName, fmCreate or fmShareExclusive);
  FreeAndNil(fOutput);
  //
  if not fExe.fileExists then
  begin
    if fDbgRunnable then
      dlgOkInfo('Either the runnable is not compiled or it cannot be found' +
        LineEnding +  'Note that the runnable option "outputFolder" is not supported',
          'GDB commander')
    else
      dlgOkInfo('The project binary is missing, cannot debug', 'GDB commander');
    exit;
  end;
  gdb := exeFullName('gdb');
  if not gdb.fileExists then
    exit;
  subjDebugStart(fSubj, self as ICEDebugger);
  // gdb process
  killGdb;
  fGdb := TCEProcess.create(nil);
  fGdb.Executable:= gdb;
  fgdb.Options:= [poUsePipes, poStderrToOutPut];
  fgdb.Parameters.Add(fExe);

  //TODO-cGDB: debugee environment
  //TODO-cGDB: debugee command line

  fgdb.Parameters.Add('--interpreter=mi');
  fGdb.OnReadData:= @gdboutQuiet;
  fGdb.OnTerminate:= @gdboutJsonize;
  fgdb.execute;
  // file:line breakpoints
  storeObserversBreakpoints;
  for i:= 0 to fBreakPoints.Count-1 do
  begin
    b := fBreakPoints[i];
    case b.kind of
      bpkBreak: str := 'break ' + b.filename + ':' + intToStr(b.line) + #10;
      bpkWatch: {TODO-cGDB: put watchpoint from persistent};
    end;

    fGdb.Input.Write(str[1], str.length);
  end;
  // break on druntime exceptions + any throw'
  gdbCommand('break onAssertError');
  gdbCommand('break onAssertErrorMsg');
  gdbCommand('break onUnittestErrorMsg');
  gdbCommand('break onRangeError');
  gdbCommand('break onFinalizeError');
  gdbCommand('break onHiddenFuncError');
  gdbCommand('break onOutOfMemoryError');
  gdbCommand('break onInvalidMemoryOperationError');
  gdbCommand('break onSwitchError');
  gdbCommand('break onUnicodeError');
  gdbCommand('break _d_throwc');
  gdbCommand('break _d_throwdwarf');
  gdbCommand('break _d_assertm');
  gdbCommand('break _d_assert');
  gdbCommand('break _d_assert_msg');
  gdbCommand('break _d_array_bounds');
  gdbCommand('break _d_arraybounds');
  gdbCommand('break _d_switch_error');
  gdbCommand('-gdb-set mi-async on');
  fGdb.OnReadData := @gdboutJsonize;
  // launch
  gdbCommand('run >' + fOutputName + '< ' + fInputName);
  setState(gsRunning);
end;
{$ENDREGION}

{$REGION GDB output processors -------------------------------------------------}
procedure parseGdbout(const str: string; var json: TJSONObject);

  procedure parseProperty(node: TJSONObject; r: PStringRange); forward;
  procedure parseProperty(node: TJSONArray; r: PStringRange); forward;

  procedure parseCLI(node: TJSONObject; r: PStringRange);
  var
    lne: TStringRange;
    msg: string = '';
  begin
    if r^.front = '"' then
      r^.popFront;
    while true do
    begin
      lne := r^.takeUntil(['\', '"']);
      if (r^.empty) then
        break
      else if r^.front = '\' then
      begin
        r^.popFront;
        if r^.front = 'n' then
        begin
          r^.popFront;
          node.Arrays['CLI'].Add(msg + lne.yield);
          msg := '';
        end else
          msg += lne.yield;
      end
      else if r^.front = '"' then
      begin
        r^.popFront;
        if r^.front = #10 then
        begin
          r^.popFront;
          break;
        end;
      end;
    end;
  end;

  procedure parseProperty(node: TJSONArray; r: PStringRange);
  var
    c: char;
  begin
    while true do
    begin
      if r^.empty then
        exit;
      c := r^.front;
      case c of
        'a'..'z':
        begin
          r^.takeUntil('=').yield;
          r^.popFront;
        end;
        '"':
        begin
          r^.popFront;
          node.Strings[node.Count] := r^.takeUntil('"').yield;
          r^.popFront;
        end;
        '{':
        begin
          r^.popFront;
          node.Objects[node.Count] := TJSONObject.Create;
          parseProperty(node.Objects[node.Count-1], r);
        end;
        ']':
        begin
          r^.popFront;
          exit;
        end;
        ',': r^.popFront;
        #10:
        begin
          r^.popFront;
          exit;
        end;
      end;
    end;
  end;

  procedure parseProperty(node: TJSONObject; r: PStringRange);
  var
    idt,v: string;
    c: char;
  begin
    while true do
    begin
      if r^.empty then
        exit;
      c := r^.front;
      case c of
        ',':
        begin
          r^.popFront;
        end;
        'a'..'z':
        begin
          idt := r^.takeUntil('=').yield;
          r^.popFront;
        end;
        '"':
        begin
          v := '';
          r^.popFront;
          while true do
          begin
            v += r^.takeUntil(['"','\']).yield;
            if r^.front = '\' then
            begin
              v += '\';
              r^.popFront;
              if r^.front = '"' then
              begin
                r^.popFront;
                v += '"';
              end;
            end else
              break;
          end;
          node.Strings[idt] := v;
          r^.popFront;
        end;
        '{':
        begin
          r^.popFront;
          node.Objects[idt] := TJSONObject.Create;
          parseProperty(node.Objects[idt], r);
        end;
        '[':
        begin
          r^.popFront;
          node.Arrays[idt] := TJSONArray.Create;
          parseProperty(node.Arrays[idt], r);
        end;
        '}', ']':
        begin
          r^.popFront;
          exit;
        end;
        ' ', #9:
          r^.popFront;
        #10:
        begin
          r^.popFront;
          exit;
        end;
      end;
    end;
  end;

var
  rng: TStringRange = (ptr: nil; pos: 0; len: 0);
begin
  json.Clear;
  if str.length = 0 then
    exit;
  rng.init(str);
  json.Arrays['OUT'] := TJSONArray.Create;
  json.Arrays['CLI'] := TJSONArray.Create;
  while true do
  begin
    if rng.empty then
      exit;
    case rng.front of
      // event
      '*':
      begin
        parseProperty(json, rng.popUntil(',')^.popFront);
      end;
      // command answer (can be a simple '^done')
      '^':
      begin
        parseProperty(json, rng.popUntil([',', #10]));
      end;
      // what would be output in a console by gdb
      '~':
      begin
        parseCLI(json, rng.popFront);
      end;
      // internal gdb messages
      '&':
      begin
        rng.popUntil(#10);
        if not rng.empty then
          rng.popFront;
      end;
      // async notify / status / out stream when remote (@)
      '=', '+','@':
      begin
        rng.popUntil(#10);
        if not rng.empty then
          rng.popFront;
      end
      else
      begin
        rng.popUntil(#10);
        if not rng.empty then
          rng.popFront;
      end;
    end;
  end;
end;

procedure TCEGdbWidget.interpretJson;

  procedure autoGetStuff;
  begin
    if fOptions.autoGetCallStack then
      infoStack;
    if fOptions.autoGetRegisters then
      infoRegs;
    if fOptions.autoGetVariables then
      infoVariables;
  end;

var
  r: PString;
  i: integer;
  val: TJSONData;
  obj: TJSONObject;
  arr: TJSONArray;
  // common data
  nme: string;
  reason: string;
  addr: PtrUint = 0;
  fullname: string = '';
  func:string = '';
  line: integer = -1;
  // registers data
  number: integer = 0;
  // signal data
  sigmean: string;
  signame: string;
  brkreason: TCEDebugBreakReason;
  // FPU
  fFpuExtended: extended;
  fFpuRaw: array[0..9] of Byte absolute fFpuExtended;
begin

  val := fJson.Find('reason');
  if val.isNotNil then
  begin
    reason := val.AsString;

    r := stopReasons.match(reason);
    if assigned(r) then
    begin
      case r^ of
        'breakpoint-hit': brkreason := dbBreakPoint;
        'watchpoint-trigger', 'access-watchpoint-trigger', 'read-watchpoint-trigger':
          brkreason:= dbWatch;
        else brkreason := dbStep;
      end;
      if brkreason = dbWatch then
      begin
        obj := TJSONObject(fJson.Find('wpt'));
        if obj.isNotNil and (obj.JSONType = jtObject) then
        begin
          val := obj.Find('exp');
          if val.isNotNil then
          begin
            ValueListEditor1.FindRow(val.AsString, i);
            if i <> -1 then
              ValueListEditor1.Row:=i;
          end;
        end;
      end;
      obj := TJSONObject(fJson.Find('frame'));
      if obj.isNotNil and (obj.JSONType = jtObject) then
      begin
        val := obj.Find('fullname');
        if val.isNotNil then
          fullname := val.AsString;
        val := obj.Find('line');
        if val.isNotNil then
          line := val.AsInteger;
        if fDocHandler.findDocument(fullname).isNil and fullname.fileExists then
          fDocHandler.openDocument(fullname);
        setState(gsPaused);
        autoGetStuff;
        readOutput;
        subjDebugBreak(fSubj, fullname, line, brkreason);
      end;
    end

    else if reason = 'watchpoint-scope' then
    begin
      gdbCommand('continue', @gdboutJsonize);
    end

    else if reason = 'signal-received' then
    begin
      signame := 'unknown signal';
      sigmean := 'unknown meaning';
      val := fJson.Find('signal-name');
      if val.isNotNil then
        signame := val.AsString;
      if (fOptions.ignoredSignals.Count <> 0) and
        (fOptions.ignoredSignals.IndexOf(signame) <> -1) then
          exit;
      val := fJson.Find('signal-meaning');
      if val.isNotNil then
        sigmean := val.AsString;
      obj := TJSONObject(fJson.Find('frame'));
      if obj.isNotNil and (obj.JSONType = jtObject) then
      begin
        val := obj.Find('fullname');
        if val.isNotNil then
          fullname := val.AsString;
        val := obj.Find('line');
        if val.isNotNil then
          line := val.AsInteger;
      end;
      if fCatchPause then
      begin
        fCatchPause := false;
        if  fDocHandler.findDocument(fullname).isNil and fullname.fileExists then
          fDocHandler.openDocument(fullname);
        autoGetStuff;
        setState(gsPaused);
        readOutput;
        subjDebugBreak(fSubj, fullname, line, dbSignal);
      end
      else
      begin
        if dlgYesNo(format('The signal %s (%s) was received on line %d of file %s .'
        + LineEnding + 'Do you wish to pause execution ?', [signame, sigmean, line, fullname]),
        'Unexpected signal received') = mrNo then
        begin
          gdbCommand('continue', @gdboutJsonize);
          setState(gsRunning);
        end
        else
        begin
          if not fDocHandler.findDocument(fullname).isNil and fullname.fileExists then
            fDocHandler.openDocument(fullname);
          autoGetStuff;
          setState(gsPaused);
          readOutput;
          subjDebugBreak(fSubj, fullname, line, dbSignal);
        end;
      end;
    end

    else if (reason = 'exited-normally') or (reason = 'exited-signalled')
      or (reason = 'exited')
    then
    begin
      readOutput;
      if not fOptions.showGdbOutput then
        fMsg.message('debugging terminated: ' + reason, nil, amcMisc, amkInf);
      setState(gsNone);
      subjDebugStop(fSubj);
    end;

  end;

  val := fJson.Find('msg');
  if val.isNotNil then
  begin
    fMsg.message(val.AsString, nil, amcMisc, amkAuto);
  end;

  val := fJson.Find('register-values');
  if val.isNotNil and (val.JSONType = jtArray) then
  begin
    arr := TJSONArray(val);
    for i := 0 to arr.Count-1 do
    begin
      obj := TJSONObject(arr.Objects[i]);
      if obj.isNil then
        break
      else
      begin
        val := obj.Find('number');
        if val.isNotNil then
          number := val.AsInteger;
        val := obj.Find('value');
        if val.isNotNil then case number of
            0..integer(high(TCpuRegister)):
            begin
              fInspState.CPU.setInspectableRegister
                (TCpuRegister(number), {$IFDEF CPU64}val.AsInt64{$ELSE}val.AsInteger{$ENDIF});
            end;
            flagOffset:
            begin
              fInspState.setInspectableFlags({$IFDEF CPU64}val.AsInt64{$ELSE}val.AsInteger{$ENDIF});
            end;
            segOffset..segOffset+5:
            begin
              fInspState.SSR.setInspectableRegister
                (TSegRegister(number - segOffset), val.AsInteger);
            end;
            stOffset..stOffset+7:
            begin
              fFpuRaw[9] := StrToInt('$' + val.AsString[3..4]);
              fFpuRaw[8] := StrToInt('$' + val.AsString[5..6]);
              fFpuRaw[7] := StrToInt('$' + val.AsString[7..8]);
              fFpuRaw[6] := StrToInt('$' + val.AsString[9..10]);
              fFpuRaw[5] := StrToInt('$' + val.AsString[11..12]);
              fFpuRaw[4] := StrToInt('$' + val.AsString[13..14]);
              fFpuRaw[3] := StrToInt('$' + val.AsString[15..16]);
              fFpuRaw[2] := StrToInt('$' + val.AsString[17..18]);
              fFpuRaw[1] := StrToInt('$' + val.AsString[19..20]);
              fFpuRaw[0] := StrToInt('$' + val.AsString[21..22]);
              fInspState.FPU.setInspectableRegister
                (TFpuRegister(number - stOffset), fFpuExtended);
            end;
        end;
        // TODO-cGDB: get SSE registers
      end;
    end;
    cpuVIewer.RefreshPropertyValues;
  end;

  val := fJson.Find('stack');
  if val.isNotNil and (val.JSONType = jtArray) then
  begin
    fStackItems.clear;
    lstCallStack.Clear;
    arr := TJSONArray(val);
    for i := 0 to arr.Count-1 do
    begin
      obj := arr.Objects[i];
      if obj.isNil then
        break;
      val := obj.Find('fullname');
      if val.isNotNil then
        fullname:= val.AsString;
      val := obj.Find('func');
      if val.isNotNil then
      begin
        if fOptions.autoDemangle then
          func:= demangle(val.AsString)
        else
          func := val.AsString;
      end;
      val := obj.Find('addr');
      if val.isNotNil then
        addr := val.AsInt64;
      val := obj.Find('line');
      if val.isNotNil then
        line := val.AsInteger;
      fStackItems.addItem(addr, fullname, func, line);
    end;
    fStackItems.assignToList(lstCallStack);
  end;

  val := fJson.Find('variables');
  if val.isNotNil and (val.JSONType = jtArray) then
  begin
    i := ValueListEditor1.Row;
    ValueListEditor1.Clear;
    arr := TJSONArray(val);
    for i := 0 to arr.Count-1 do
    begin
      val := arr.Items[i];
      if val.JSONType <> jtObject then
        continue;
      obj := TJSONObject(val);
      val := obj.Find('name');
      if val.isNil then
        continue;
      nme := val.AsString;
      val := obj.Find('value');
      if val.isNil then
        continue;
      ValueListEditor1.InsertRow(nme, val.AsString, false);
    end;
    if (i <> -1) and (i <= ValueListEditor1.RowCount) then
      ValueListEditor1.Row:=i;
  end;

  if fOptions.showGdbOutput or fShowFromCustomCommand then
  begin
    fShowFromCustomCommand := false;
    arr := TJSONArray(fJson.Find('CLI'));
    if arr.isNotNil then
      for i := 0 to arr.Count-1 do
        fMsg.message(arr.Strings[i], nil, amcMisc, amkBub);
  end;

end;

procedure TCEGdbWidget.gdboutJsonize(sender: TObject);
var
  str: string;
  //lst: TStringList;
begin
  if fMsg = nil then
    exit;

  fLog.Clear;
  fGdb.getFullLines(fLog);
  for str in fLog do
    fMsg.message(str, nil, amcMisc, amkAuto);

  if flog.Text.isEmpty then
    exit;

  parseGdbout(fLog.Text, fJson);
  interpretJson;

  //lst := TStringList.Create;
  //try
  //  str := fJson.FormatJSON(DefaultFormat,2);
  //  lst.Text:= str;
  //  lst.SaveToFile('/home/basile/gdbmessage.json');
  //finally
  //  lst.Free;
  //end;

end;

procedure TCEGdbWidget.readOutput;
var
  str: TMemoryStream;
  lst: TStringList;
  lne: string;
begin
  if (fGdbState = gsNone) or not fOptions.showOutput then
    exit;
  if fOutput.isNil and fOutputName.fileExists then
  try
    fOutput := TFileStream.Create(fOutputName, 0);
  except
    if fOutput.isNotNil then
      FreeAndNil(fOutput);
  end;
  if fOutput.isNil then
    exit;
  str := TMemoryStream.Create;
  lst := TStringList.Create;
  try
    str.size := fOutput.Size - fOutput.Position;
    fOutput.Read(str.Memory^, str.Size);
    lst.LoadFromStream(str);
    for lne in lst do
      fMsg.message(lne, nil, amcMisc, amkBub);
  finally
    lst.Free;
    str.Free;
  end;
end;

procedure TCEGdbWidget.gdboutQuiet(sender: TObject);
begin
  fGdb.OutputStack.Clear;
  fGdb.OnReadData:=@gdboutJsonize;
end;
{$ENDREGION}

{$REGION GDB commands & actions ------------------------------------------------}
procedure TCEGdbWidget.gdbCommand(aCommand: string; gdbOutProcessor: TNotifyEvent = nil);
begin
  if fGdb.isNil or not fGdb.Running then
    exit;
  aCommand += #10;
  if assigned(gdbOutProcessor) then
    fGdb.OnReadData := gdbOutProcessor;
  fGdb.Input.Write(aCommand[1], aCommand.length);
end;

procedure TCEGdbWidget.infoRegs;
begin
  disableEditor;
  gdbCommand('-data-list-register-values r', @gdboutJsonize);
end;

procedure TCEGdbWidget.infoStack;
begin
  gdbCommand('-stack-list-frames', @gdboutJsonize);
end;

procedure TCEGdbWidget.infoVariables;
begin
  gdbCommand('-stack-list-variables 1');
end;

procedure TCEGdbWidget.btnStartClick(Sender: TObject);
begin
  startDebugging;
end;

procedure TCEGdbWidget.btnContClick(Sender: TObject);
begin
  gdbCommand('-exec-continue --all', @gdboutJsonize);
  if assigned(fGdb) and fgdb.Running then
    setState(gsRunning);
end;

procedure TCEGdbWidget.btnVariablesClick(Sender: TObject);
begin
  infoVariables;
end;

procedure TCEGdbWidget.btnNextClick(Sender: TObject);
begin
  gdbCommand('step', @gdboutJsonize);
  if assigned(fGdb) and fgdb.Running then
    setState(gsRunning);
end;

procedure TCEGdbWidget.btnOverClick(Sender: TObject);
begin
  gdbCommand('next', @gdboutJsonize);
  if assigned(fGdb) and fgdb.Running then
    setState(gsRunning);
end;

procedure TCEGdbWidget.btnPauseClick(Sender: TObject);
begin
  if assigned(fGdb) and fGdb.Running then
    fCatchPause:=true;
  gdbCommand('-exec-interrupt --all', @gdboutJsonize);
end;

procedure TCEGdbWidget.btnRegClick(Sender: TObject);
begin
  infoRegs;
end;

procedure TCEGdbWidget.btnStackClick(Sender: TObject);
begin
  infoStack;
end;

procedure TCEGdbWidget.btnStopClick(Sender: TObject);
begin
  gdbCommand('kill', @gdboutJsonize);
  subjDebugStop(fSubj);
  setState(gsNone);
end;

procedure TCEGdbWidget.btnWatchClick(Sender: TObject);
const
  cmd: array[TAddWatchPointKind] of string = (
    '-break-watch -r ','-break-watch ','-break-watch -a ');
var
  nme: string;
begin
  if ValueListEditor1.Row = -1 then
    exit;
  nme := ValueListEditor1.Keys[ValueListEditor1.Row];
  gdbCommand(cmd[fAddWatchPointKind] + nme);
end;

procedure TCEGdbWidget.btnSendComClick(Sender: TObject);
begin
  sendCustomCommand;
end;

procedure TCEGdbWidget.Edit1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = byte(#13) then
    sendCustomCommand;
end;

procedure TCEGdbWidget.lstCallStackDblClick(Sender: TObject);
var
  itm: TStackItem;
  nme: string;
  doc: TCESynMemo;
begin
  if lstCallStack.Selected.isNil or lstCallStack.Selected.Data.isNil then
    exit;
  itm := TStackItem(lstCallStack.Selected.Data);
  nme := itm.filename;
  if not nme.fileExists then
    exit;
  fDocHandler.openDocument(nme);
  doc := fDocHandler.findDocument(nme);
  if doc.isNotNil then
    doc.CaretY:= itm.line;
  gdbCommand('-stack-select-frame ' + intToStr(lstCallStack.ItemIndex));
  if fOptions.autoGetVariables then
    infoVariables;
  if fOptions.autoGetRegisters then
    infoRegs;
end;

procedure TCEGdbWidget.mnuReadWClick(Sender: TObject);
begin
  fAddWatchPointKind := wpkRead;
  mnuWriteW.Checked:=false;
  mnuReadWriteW.Checked:=false;
end;

procedure TCEGdbWidget.mnuReadWriteWClick(Sender: TObject);
begin
  fAddWatchPointKind := wpkReadWrite;
  mnuReadW.Checked:=false;
  mnuWriteW.Checked:=false;
end;

procedure TCEGdbWidget.sendCustomCommand;
var
  cmd: string;
begin
  cmd := edit1.Text;
  if cmd.isEmpty then
    exit;
  if edit1.Items.IndexOf(cmd) = -1 then
    edit1.Items.Add(cmd);
  cmd := fSyms.expand(cmd);
  if (cmd.length > 1) and (cmd[1] = '>') and assigned(fInput) then
  begin
    cmd := cmd[2..cmd.length] + #10;
    fInput.Write(cmd[1], cmd.length);
    {$IFDEF UNIX}
    fpfsync(fInput.Handle);
    {$ELSE}
    FlushFileBuffers(fInput.Handle);
    {$ENDIF}
    sleep(100);
    readOutput;
  end
  else
  begin
    fShowFromCustomCommand := true;
    gdbCommand(cmd, @gdboutJsonize);
  end;
  edit1.Text := '';
end;


//TODO-cGDB: copy from call stack list
//TODO-cGDB: replace value list editor by TListView
//to set focus on variable of a triggered watchpoint.

procedure TCEGdbWidget.setGpr(reg: TCpuRegister; val: TCpuGprValue);
const
  spec = 'set $%s = 0x%X';
var
  cmd : string;
begin
  cmd := format(spec, [GetEnumName(typeinfo(TCpuRegister),integer(reg)), val]);
  gdbCommand(cmd);
end;

procedure TCEGdbWidget.setSsr(reg: TSegRegister; val: TCPUSegValue);
const
  spec = 'set $%s = 0x%X';
var
  cmd : string;
begin
  cmd := format(spec, [GetEnumName(typeinfo(TSegRegister),integer(reg)), val]);
  gdbCommand(cmd);
end;

procedure TCEGdbWidget.setFlag(val: PtrUint);
const
  spec = 'set $eflags = 0x%X';
var
  cmd: string;
begin
  cmd := format(spec, [val]);
  gdbCommand(cmd);
end;

procedure TCEGdbWidget.setFpr(reg: TFpuRegister; val: extended);
const
  spec = 'set $%s = %.18g';
var
  cmd : string;
begin
  cmd := format(spec, [GetEnumName(typeinfo(TFpuRegister),integer(reg)), val]);
  gdbCommand(cmd);
end;
{$ENDREGION}

initialization
  RegisterPropertyEditor(TypeInfo(TCpuGprValue), nil, '', TCpuRegValueEditor);
  RegisterPropertyEditor(TypeInfo(TCpuSegValue), nil, '', TCpuSegValueEditor);
end.

