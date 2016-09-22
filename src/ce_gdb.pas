unit ce_gdb;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, RegExpr, ComCtrls,
  PropEdits, GraphPropEdits, RTTIGrids, Dialogs, ExtCtrls, Menus, Buttons,
  StdCtrls, process, fpjson, typinfo,
  ce_common, ce_interfaces, ce_widget, ce_processes, ce_observer, ce_synmemo,
  ce_sharedres, ce_stringrange, ce_dsgncontrols, ce_dialogs, ce_dbgitf,
  ce_ddemangle, ce_writableComponent;

type

  {$IFDEF CPU64}
  TCpuRegister = (rax, rbx, rcx, rdx, rsi, rdi, rbp, rsp, r8, r9, r10, r11, r12, r13,
    r14, r15, rip);
  {$ENDIF}

  {$IFDEF CPU32}
  TCpuRegister = (eax, ebx, ecx, edx, esi, edi, ebp, esp, eip);
  {$ENDIF}

  TFpuRegister = (st0, st1, st2, st3, st4, st5, st6, st7);

  TFLAG = (CS, PF, AF, ZF, SF, TF, IF_, DF, OF_, NT, RF, VM, AC, VIF, VIP, ID);
  TEFLAG = set of TFLAG;

  TSegmentRegister = (S_CS, S_SS, S_DS, S_ES, S_FS, S_GS);

  // aliased to get hex display in object inspector.
  TCpuRegValue = type PtrUInt;

  // displays a TCpuRegValue in hex
  TCpuRegValueEditor = class(TIntegerProperty)
  public
    function GetValue: ansistring; override;
    procedure SetValue(const NewValue: ansistring); override;
  end;

  TSetGprEvent = procedure(reg: TCpuRegister; val: TCpuRegValue) of object;

  // Makes a category for the general purpose registers in a project inspector
  TInspectableGPR = class(TPersistent)
  private
    fRegisters: array[TCpuRegister] of TCpuRegValue;
    fSetGprEvent: TSetGprEvent;
    procedure setRegister(index: TCpuRegister; value: TCpuRegValue);
  published
    {$IFDEF CPU64}
    property RAX: TCpuRegValue index TCpuRegister.rax read fRegisters[TCpuRegister.rax] write setRegister;
    property RBX: TCpuRegValue index TCpuRegister.rbx read fRegisters[TCpuRegister.rbx] write setRegister;
    property RCX: TCpuRegValue index TCpuRegister.rcx read fRegisters[TCpuRegister.rcx] write setRegister;
    property RDX: TCpuRegValue index TCpuRegister.rdx read fRegisters[TCpuRegister.rdx] write setRegister;
    property RSI: TCpuRegValue index TCpuRegister.rsi read fRegisters[TCpuRegister.rsi] write setRegister;
    property RDI: TCpuRegValue index TCpuRegister.rdi read fRegisters[TCpuRegister.rdi] write setRegister;
    property RBP: TCpuRegValue index TCpuRegister.rbp read fRegisters[TCpuRegister.rbp] write setRegister;
    property RSP: TCpuRegValue index TCpuRegister.rsp read fRegisters[TCpuRegister.rsp] write setRegister;
    property R8:  TCpuRegValue index TCpuRegister.r8  read fRegisters[TCpuRegister.r8] write setRegister;
    property R9:  TCpuRegValue index TCpuRegister.r9  read fRegisters[TCpuRegister.r9] write setRegister;
    property R10: TCpuRegValue index TCpuRegister.r10 read fRegisters[TCpuRegister.r10] write setRegister;
    property R11: TCpuRegValue index TCpuRegister.r11 read fRegisters[TCpuRegister.r11] write setRegister;
    property R12: TCpuRegValue index TCpuRegister.r12 read fRegisters[TCpuRegister.r12] write setRegister;
    property R13: TCpuRegValue index TCpuRegister.r13 read fRegisters[TCpuRegister.r13] write setRegister;
    property R14: TCpuRegValue index TCpuRegister.r14 read fRegisters[TCpuRegister.r14] write setRegister;
    property R15: TCpuRegValue index TCpuRegister.r15 read fRegisters[TCpuRegister.r15] write setRegister;
    property RIP: TCpuRegValue index TCpuRegister.rip read fRegisters[TCpuRegister.rip] write setRegister;
    {$ELSE}
    property EAX: TCpuRegValue index TCpuRegister.eax read fRegisters[TCpuRegister.eax] write setRegister;
    property EBX: TCpuRegValue index TCpuRegister.ebx read fRegisters[TCpuRegister.ebx] write setRegister;
    property ECX: TCpuRegValue index TCpuRegister.ecx read fRegisters[TCpuRegister.ecx] write setRegister;
    property EDX: TCpuRegValue index TCpuRegister.edx read fRegisters[TCpuRegister.edx] write setRegister;
    property ESI: TCpuRegValue index TCpuRegister.esi read fRegisters[TCpuRegister.esi] write setRegister;
    property EDI: TCpuRegValue index TCpuRegister.edi read fRegisters[TCpuRegister.edi] write setRegister;
    property EBP: TCpuRegValue index TCpuRegister.ebp read fRegisters[TCpuRegister.ebp] write setRegister;
    property ESP: TCpuRegValue index TCpuRegister.esp read fRegisters[TCpuRegister.esp] write setRegister;
    property EIP: TCpuRegValue index TCpuRegister.eip read fRegisters[TCpuRegister.eip] write setRegister;
    {$ENDIF}
  public
    constructor create(event: TSetGprEvent);
    procedure setInspectableRegister(index: TCpuRegister; value: PtrUInt);
  end;

  // Makes a category for the floating point coprocessor registers in a project inspector
  TInspectableFPR = class(TPersistent)
  private
    fRegisters: array[TFpuRegister] of double;
  published
    property ST0: double read fRegisters[TFpuRegister.st0];
    property ST1: double read fRegisters[TFpuRegister.st1];
    property ST2: double read fRegisters[TFpuRegister.st2];
    property ST3: double read fRegisters[TFpuRegister.st3];
    property ST4: double read fRegisters[TFpuRegister.st4];
    property ST5: double read fRegisters[TFpuRegister.st5];
    property ST6: double read fRegisters[TFpuRegister.st6];
    property ST7: double read fRegisters[TFpuRegister.st7];
  public
    procedure setInspectableRegister(index: TFpuRegister; value: double);
  end;

  // Makes a category for the SSE registers in a project inspector
  TInspectableSSER = class(TPersistent)
    // interpretation is a problem:
    // 4 int ? 2 double ? 4 single ? ...
  end;

  // Stores the registers content, to be displayable in an object inspector.
  TInspectableState = class(TPersistent)
  private
    fFlags: TEFLAG;
    fSegment: array[TSegmentRegister] of byte;
    fGpr: TInspectableGPR;
    fFpr: TInspectableFPR;
  published
    property CPU: TInspectableGPR read fGpr;
    //
    property EFLAGS: TEFLAG read fFlags;
    //
    property CS: byte read fSegment[TSegmentRegister.S_CS];
    property DS: byte read fSegment[TSegmentRegister.S_DS];
    property ES: byte read fSegment[TSegmentRegister.S_ES];
    property FS: byte read fSegment[TSegmentRegister.S_FS];
    property GS: byte read fSegment[TSegmentRegister.S_GS];
    property SS: byte read fSegment[TSegmentRegister.S_SS];
  public
    constructor create(setGprEvent: TSetGprEvent);
    destructor destroy; override;
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
    procedure listDblClick(sender: TObject);
  public
    constructor create;
    destructor destroy; override;
    procedure assignToList(list: TListView);
    procedure addItem(addr: PtrUint; fname, nme: string; lne: integer);
    procedure clear;
  end;

  // TODO-cGDB: shortcuts
  // TODO-cGDB: assembly view
  // TODO-cGDB: show locals

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
    procedure setIgnoredSignals(value: TStringList);
    procedure setCommandsHistory(value: TStringList);
  published
    property autoDemangle: boolean read fAutoDemangle write fAutoDemangle;
    property autoGetCallStack: boolean read fAutoGetCallStack write fAutoGetCallStack;
    property autoGetRegisters: boolean read fAutoGetRegisters write fAutoGetRegisters;
    property autoGetVariables: boolean read fAutoGetVariables write fAutoGetVariables;
    property commandsHistory: TStringList read fCommandsHistory write setCommandsHistory;
    property ignoredSignals: TStringList read fIgnoredSignals write setIgnoredSignals;
    property showGdbOutput: boolean read fShowGdbOutput write fShowGdbOutput;
    property showOutput: boolean read fShowOutput write fShowOutput;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    procedure assign(source: TPersistent); override;
  end;

  TCEDebugOptions = class(TCEDebugOptionsBase, ICEEditableOptions)
  private
    fBackup: TCEDebugOptionsBase;
    function optionedWantCategory(): string;
    function optionedWantEditorKind: TOptionEditorKind;
    function optionedWantContainer: TPersistent;
    procedure optionedEvent(event: TOptionEditorEvent);
    function optionedOptionsModified: boolean;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
  end;

  { TCEGdbWidget }
  TCEGdbWidget = class(TCEWidget, ICEProjectObserver, ICEDocumentObserver, ICEDebugger)
    btnContinue: TCEToolButton;
    btnNext: TCEToolButton;
    btnOver: TCEToolButton;
    btnPause: TCEToolButton;
    btnReg: TCEToolButton;
    btnStack: TCEToolButton;
    btnStart: TCEToolButton;
    btnStop: TCEToolButton;
    button4: TCEToolButton;
    Edit1: TComboBox;
    lstCallStack: TListView;
    Panel2: TPanel;
    Panel3: TPanel;
    btnSendCom: TSpeedButton;
    stateViewer: TTIPropertyGrid;
    procedure btnContClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnOverClick(Sender: TObject);
    procedure btnPauseClick(Sender: TObject);
    procedure btnRegClick(Sender: TObject);
    procedure btnSendComClick(Sender: TObject);
    procedure btnStackClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure Edit1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected
    procedure setToolBarFlat(value: boolean); override;
  private
    fSubj: TCEDebugObserverSubject;
    fDoc: TCESynMemo;
    fProj: ICECommonProject;
    fJson: TJsonObject;
    fLog: TStringList;
    fFileLineBrks: TStringList;
    fDocHandler: ICEMultiDocHandler;
    fMsg: ICEMessagesDisplay;
    fGdb: TCEProcess;
    fInspState: TInspectableState;
    fStackItems: TStackItems;
    fCatchPause: boolean;
    fOptions: TCEDebugOptions;
    //
    procedure startDebugging;
    procedure pauseDebugee;
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
    procedure sendCustomCommand;
    procedure setGpr(reg: TCpuRegister; val: TCpuRegValue);
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
    procedure addBreakPoint(const fname: string; line: integer; kind: TBreakPointKind);
    procedure removeBreakPoint(const fname: string; line: integer);
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
  end;


implementation
{$R *.lfm}

{$REGION TCEDebugOption --------------------------------------------------------}
const optFname = 'gdbcommander.txt';

constructor TCEDebugOptionsBase.create(aOwner: TComponent);
begin
  inherited;
  fAutoDemangle := true;
  fAutoGetCallStack:= true;
  fAutoGetRegisters:= true;
  fAutoGetVariables:= true;
  fShowGdbOutput:=true;
  fIgnoredSignals := TStringList.Create;
  fCommandsHistory := TStringList.Create;
end;

destructor TCEDebugOptionsBase.destroy;
begin
  fIgnoredSignals.Free;
  fCommandsHistory.Free;
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
    oeeAccept: fBackup.assign(self);
  end;
end;

function TCEDebugOptions.optionedOptionsModified: boolean;
begin
  exit(false);
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
  list.ReadOnly:=true;
  list.GridLines:=true;
  list.ViewStyle:= TViewStyle.vsReport;
  if list.ColumnCount <> 3 then
  begin
    list.Columns.Clear;
    list.Columns.Add;
    list.Columns.Add;
    list.Columns.Add;
  end;
  list.Column[0].MaxWidth:= 250;
  list.Column[0].Caption:= 'function';
  list.Column[1].Caption:= 'address';
  list.Column[2].Caption:= 'filename';
  list.Column[0].AutoSize:= true;
  list.Column[1].AutoSize:= true;
  list.Column[2].AutoSize:= true;
  list.OnDblClick:= @listDblClick;
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

procedure TStackItems.listDblClick(sender: TObject);
var
  lst: TListView;
  itm: TStackItem;
  nme: string;
  doc: TCESynMemo;
begin
  if (sender.isNil) or not (sender is TListView) then
    exit;
  lst := TListView(sender);
  if lst.Selected.isNil or lst.Selected.Data.isNil then
    exit;
  itm := TStackItem(lst.Selected.Data);
  nme := itm.filename;
  if not nme.fileExists then
    exit;
  getMultiDocHandler.openDocument(nme);
  doc := getMultiDocHandler.findDocument(nme);
  if doc.isNotNil then
    doc.CaretY:= itm.line;
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

{$REGION TInspectableState -----------------------------------------------------}
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

constructor TInspectableGPR.create(event: TSetGprEvent);
begin
  fSetGprEvent:=event;
end;

procedure TInspectableGPR.setInspectableRegister(index: TCpuRegister; value: PtrUInt);
begin
  fRegisters[index] := value;
end;

procedure TInspectableGPR.setRegister(index: TCpuRegister; value: TCpuRegValue);
begin
  fSetGprEvent(index, value);
  fRegisters[index] := value;
end;

procedure TInspectableFPR.setInspectableRegister(index: TFpuRegister; value: double);
begin
  fRegisters[index] := value;
end;

constructor TInspectableState.create(setGprEvent: TSetGprEvent);
begin
  fGpr := TInspectableGPR.Create(setGprEvent);
end;

destructor TInspectableState.destroy;
begin
  fGpr.Free;
  inherited;
end;
{$ENDREGION}

{$REGION Common/standard comp --------------------------------------------------}
constructor TCEGdbWidget.create(aOwner: TComponent);
begin
  inherited;
  EntitiesConnector.addObserver(self);
  EntitiesConnector.addSingleService(self);
  fDocHandler:= getMultiDocHandler;
  fMsg:= getMessageDisplay;
  fFileLineBrks:= TStringList.Create;
  fLog := TStringList.Create;
  fInspState := TInspectableState.Create(@setGpr);
  stateViewer.TIObject := fInspState;
  fJson := TJsonObject.Create;
  fStackItems := TStackItems.create;
  fSubj:= TCEDebugObserverSubject.Create;
  fOptions:= TCEDebugOptions.create(self);
  Edit1.Items.Assign(fOptions.commandsHistory);
  //
  AssignPng(btnSendCom, 'ACCEPT');
end;

destructor TCEGdbWidget.destroy;
begin
  fOptions.commandsHistory.Assign(edit1.Items);
  fOptions.Free;
  fFileLineBrks.Free;
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
  if fDoc = document then
    fDoc := nil;
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
  fFileLineBrks.Clear;
  for i:= 0 to fSubj.observersCount-1 do
  begin
    obs := fSubj.observers[i] as ICEDebugObserver;
    for j := 0 to obs.debugQueryBpCount-1 do
    begin
      obs.debugQueryBreakPoint(j, nme, lne, knd);
      {$PUSH}{$WARNINGS OFF}{$HINTS OFF}
      fFileLineBrks.AddObject(nme, TObject(pointer(lne)));
      {$POP}
    end;
  end;
end;

procedure TCEGdbWidget.addBreakPoint(const fname: string; line: integer; kind: TBreakPointKind);
begin
  if fGdb.isNil or not fGdb.Running then
    exit;
  //TODO-cGDB: handle trace points
  gdbCommand('break ' + fname + ':' + intToStr(line));
end;

procedure TCEGdbWidget.removeBreakPoint(const fname: string; line: integer);
begin
  if fGdb.isNil or not fGdb.Running then
    exit;
  gdbCommand('clear ' + fname + ':' + intToStr(line));
end;

procedure TCEGdbWidget.startDebugging;
var
  str: string;
  gdb: string;
  i: integer;
begin
  // protect
  if fProj = nil then
    exit;
  if fProj.binaryKind <> executable then
    exit;
  str := fProj.outputFilename;
  if not str.fileExists then
    exit;
  gdb := exeFullName('gdb');
  if not gdb.fileExists then
    exit;
  subjDebugStart(fSubj, self as ICEDebugger);
  // gdb process
  killGdb;
  fGdb := TCEProcess.create(nil);
  fGdb.Executable:= gdb;
  fgdb.Options:= [poUsePipes, poStderrToOutPut];
  fgdb.Parameters.Add(str);

  //TODO-cGDB: debugee environment
  //TODO-cGDB: debugee command line
  //TODO-cGDB: pass input to debugee

  fgdb.Parameters.Add('--interpreter=mi');
  fGdb.OnReadData:= @gdboutQuiet;
  fGdb.OnTerminate:= @gdboutJsonize;
  fgdb.execute;
  // file:line breakpoints
  storeObserversBreakpoints;
  for i:= 0 to fFileLineBrks.Count-1 do
  begin
    str := 'break ' + fFileLineBrks.Strings[i] + ':' + intToStr(PtrUInt(fFileLineBrks.Objects[i])) + #10;
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
  gdbCommand('run');
end;

procedure TCEGdbWidget.pauseDebugee;
var
  proc: TProcess;
begin
  // TODO-cGDB: pause using the PID is safer
  proc := TProcess.Create(nil);
  try
    fCatchPause := true;
    proc.Executable:= 'kill';
    proc.ShowWindow:= swoHIDE;
    proc.Parameters.Add('-s');
    proc.Parameters.Add('USR1');
    proc.Parameters.Add(fProj.outputFilename.extractFileName);
    proc.Execute;
    while proc.Running do;
  finally
    proc.Free;
  end;
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

  procedure parseInferior(node: TJSONObject; r: PStringRange);
  begin
    while true do
    begin
      // TODO-cGDB: detect invalid command after GDB prefix, maybe inferior output
      if r^.empty or (r^.front in ['~','^','*','=','&',(*'+',*)'@']) then
        break;
      node.Arrays['OUT'].Add(r^.takeUntil(#10).yield);
      if not r^.empty then
        r^.popFront;
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
    idt: string;
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
          node.Strings[idt] := r^.popFront^.takeUntil('"').yield;
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
      '=', (*'+',*)'@':
      begin
        rng.popUntil(#10);
        if not rng.empty then
          rng.popFront;
      end
      else
      begin
        if rng.startsWith('(gdb)') then
          rng.popFrontN(7)
        // empty line, inferior output
        else
          parseInferior(json, @rng);
      end;
    end;
  end;
end;

// ^done,register-values=[{number="0",value="0"},{number="1",value="140737488347232"},{number="2",value="140737349740925"},{number="3",value="140737488346960"},{number="4",value="140737488346960"},{number="5",value="1"},{number="6",value="140737488346752"},{number="7",value="140737488343952"},{number="8",value="0"},{number="9",value="140737488346176"},{number="10",value="8"},{number="11",value="518"},{number="12",value="140737488347040"},{number="13",value="140737488347024"},{number="14",value="1"},{number="15",value="140737488346960"},{number="16",value="4848450"},{number="17",value="582"},{number="18",value="51"},{number="19",value="43"},{number="20",value="0"},{number="21",value="0"},{number="22",value="0"},{number="23",value="0"},{number="24",value="0"},{number="25",value="0"},{number="26",value="0"},{number="27",value="0"},{number="28",value="0"},{number="29",value="0"},{number="30",value="0"},{number="31",value="0"},{number="32",value="895"},{number="33",value="0"},{number="34",value="65535"},{number="35",value="0"},{number="36",value="0"},{number="37",value="0"},{number="38",value="0"},{number="39",value="0"},{number="40",value="{v4_float = {0, 0, -9223372036854775808, 0}, v2_double = {0, 0}, v16_int8 = {0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0}, v8_int16 = {-256, 0, 0, 0, 0, -256, 0, 0}, v4_int32 = {65280, 0, -16777216, 0}, v2_int64 = {65280, 4278190080}, uint128 = 00078918677504442992524819234560}"},{number="41",value="{v4_float = {0, 0, 0, 0}, v2_double = {0, 0}, v16_int8 = {0 <repeats 16 times>}, v8_int16 = {0, 0, 0, 0, 0, 0, 0, 0}, v4_int32 = {0, 0, 0, 0}, v2_int64 = {0, 0}, uint128 = 00000000000000000000000000000000}"},{number="42",value="{v4_float = {-9223372036854775808, 0, 0, 0}, v2_double = {0, 0}, v16_int8 = {0, 0, 0, -1, 0 <repeats 12 times>}, v8_int16 = {0, -256, 0, 0, 0, 0, 0, 0}, v4_int32 = {-16777216, 0, 0, 0}, v2_int64 = {4278190080, 0}, uint128 = 00000000000000000000004278190080}"},{number="43",value="{v4_float = {0, 0, -9223372036854775808, -9223372036854775808}, v2_double = {0, -9223372036854775808}, v16_int8 = {0, -1, 0, 0, 0, 0, 0, 0, 0, 0, -1, -1, -1, -1, -1, -1}, v8_int16 = {-256, 0, 0, 0, 0, -1, -1, -1}, v4_int32 = {65280, 0, -65536, -1}, v2_int64 = {65280, -65536}, uint128 = 340282366920937254537554992802593570560}"},{number="44",value="{v4_float = {-9223372036854775808, -9223372036854775808, -9223372036854775808, -9223372036854775808}, v2_double = {-9223372036854775808, -9223372036854775808}, v16_int8 = {-1 <repeats 16 times>}, v8_int16 = {-1, -1, -1, -1, -1, -1, -1, -1}, v4_int32 = {-1, -1, -1, -1}, v2_int64 = {-1, -1}, uint128 = 340282366920938463463374607431768211455}"},{number="45",value="{v4_float = {0, 0, 0, 0}, v2_double = {0, 0}, v16_int8 = {0 <repeats 16 times>}, v8_int16 = {0, 0, 0, 0, 0, 0, 0, 0}, v4_int32 = {0, 0, 0, 0}, v2_int64 = {0, 0}, uint128 = 00000000000000000000000000000000}"},{number="46",value="{v4_float = {0, 0, 0, 0}, v2_double = {0, 0}, v16_int8 = {0 <repeats 16 times>}, v8_int16 = {0, 0, 0, 0, 0, 0, 0, 0}, v4_int32 = {0, 0, 0, 0}, v2_int64 = {0, 0}, uint128 = 00000000000000000000000000000000}"},{number="47",value="{v4_float = {0, 0, 0, 0}, v2_double = {0, 0}, v16_int8 = {0 <repeats 16 times>}, v8_int16 = {0, 0, 0, 0, 0, 0, 0, 0}, v4_int32 = {0, 0, 0, 0}, v2_int64 = {0, 0}, uint128 = 00000000000000000000000000000000}"},{number="48",value="{v4_float = {0, 0, 0, 0}, v2_double = {0, 0}, v16_int8 = {0, 0, 0, 0, 0, 0, 0, 0, -56, 117, 117, 0, 0, 0, 0, 0}, v8_int16 = {0, 0, 0, 0, 30152, 117, 0, 0}, v4_int32 = {0, 0, 7697864, 0}, v2_int64 = {0, 7697864}, uint128 = 00000142000527122222103840948224}"},{number="49",value="{v4_float = {0, 0, 0, 0}, v2_double = {0, 0}, v16_int8 = {0 <repeats 16 times>}, v8_int16 = {0, 0, 0, 0, 0, 0, 0, 0}, v4_int32 = {0, 0, 0, 0}, v2_int64 = {0, 0}, uint128 = 00000000000000000000000000000000}"},{number="50",value="{v4_float = {0, 0, 0, 0}, v2_double = {0, 0}, v16_int8 = {0 <repeats 16 times>}, v8_int16 = {0, 0, 0, 0, 0, 0, 0, 0}, v4_int32 = {0, 0, 0, 0}, v2_int64 = {0, 0}, uint128 = 00000000000000000000000000000000}"},{number="51",value="{v4_float = {-9223372036854775808, 0, 0, 0}, v2_double = {0, 0}, v16_int8 = {0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0}, v8_int16 = {0, -256, 0, 0, 0, 0, 0, 255}, v4_int32 = {-16777216, 0, 0, 16711680}, v2_int64 = {4278190080, 71776119061217280}, uint128 = 1324035698926381045275276568229314560}"},{number="52",value="{v4_float = {0, 0, 0, 0}, v2_double = {0, 0}, v16_int8 = {0 <repeats 16 times>}, v8_int16 = {0, 0, 0, 0, 0, 0, 0, 0}, v4_int32 = {0, 0, 0, 0}, v2_int64 = {0, 0}, uint128 = 00000000000000000000000000000000}"},{number="53",value="{v4_float = {0, 0, 0, 0}, v2_double = {0, 0}, v16_int8 = {0 <repeats 16 times>}, v8_int16 = {0, 0, 0, 0, 0, 0, 0, 0}, v4_int32 = {0, 0, 0, 0}, v2_int64 = {0, 0}, uint128 = 00000000000000000000000000000000}"},{number="54",value="{v4_float = {0, 0, 0, 0}, v2_double = {0, 0}, v16_int8 = {0 <repeats 16 times>}, v8_int16 = {0, 0, 0, 0, 0, 0, 0, 0}, v4_int32 = {0, 0, 0, 0}, v2_int64 = {0, 0}, uint128 = 00000000000000000000000000000000}"},{number="55",value="{v4_float = {0, 0, 0, 0}, v2_double = {0, 0}, v16_int8 = {0 <repeats 16 times>}, v8_int16 = {0, 0, 0, 0, 0, 0, 0, 0}, v4_int32 = {0, 0, 0, 0}, v2_int64 = {0, 0}, uint128 = 00000000000000000000000000000000}"},{number="56",value="8064"},{number="57",value="00000000000000000000000000000000"},{number="58",value="00000000000000000000000000000000"},{number="59",value="00000000000000000000000000000000"},{number="60",value="00000000000000000000000000000000"},{number="61",value="00000000000000000000000000000000"},{number="62",value="00000000000000000000000000000000"},{number="63",value="00000000000000000000000000000000"},{number="64",value="00000000000000000000000000000000"},{number="65",value="00000000000000000000000000000000"},{number="66",value="00000000000000000000000000000000"},{number="67",value="00000000000000000000000000000000"},{number="68",value="00000000000000000000000000000000"},{number="69",value="00000000000000000000000000000000"},{number="70",value="00000000000000000000000000000000"},{number="71",value="00000000000000000000000000000000"},{number="72",value="00000000000000000000000000000000"},{number="151",value="-1"},{number="152",value="0"},{number="153",value="96"},{number="154",value="125"},{number="155",value="80"},{number="156",value="80"},{number="157",value="1"},{number="158",value="-128"},{number="159",value="-112"},{number="160",value="0"},{number="161",value="64"},{number="162",value="8"},{number="163",value="6"},{number="164",value="-96"},{number="165",value="-112"},{number="166",value="1"},{number="167",value="80"},{number="168",value="0"},{number="169",value="-32"},{number="170",value="-23"},{number="171",value="-33"},{number="172",value="0"},{number="173",value="-8096"},{number="174",value="-5763"},{number="175",value="-8368"},{number="176",value="-8368"},{number="177",value="1"},{number="178",value="-8576"},{number="180",value="0"},{number="181",value="-9152"},{number="182",value="8"},{number="183",value="518"},{number="184",value="-8288"},{number="185",value="-8304"},{number="186",value="1"},{number="187",value="-8368"},{number="188",value="0"},{number="189",value="-8096"},{number="190",value="-138614403"},{number="191",value="-8368"},{number="192",value="-8368"},{number="193",value="1"},{number="194",value="-8576"},{number="195",value="-11376"},{number="196",value="0"},{number="197",value="-9152"},{number="198",value="8"},{number="199",value="518"},{number="200",value="-8288"},{number="201",value="-8304"},{number="202",value="1"},{number="203",value="-8368"},{number="204",value="{v8_float = {0, 0, -9223372036854775808, 0, 0, 0, 0, 0}, v4_double = {0, 0, 0, 0}, v32_int8 = {0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0 <repeats 20 times>}, v16_int16 = {-256, 0, 0, 0, 0, -256, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, v8_int32 = {65280, 0, -16777216, 0, 0, 0, 0, 0}, v4_int64 = {65280, 4278190080, 0, 0}, v2_int128 = {00078918677504442992524819234560, 00000000000000000000000000000000}}"},{number="205",value="{v8_float = {0, 0, 0, 0, 0, 0, 0, 0}, v4_double = {0, 0, 0, 0}, v32_int8 = {0 <repeats 32 times>}, v16_int16 = {0 <repeats 16 times>}, v8_int32 = {0, 0, 0, 0, 0, 0, 0, 0}, v4_int64 = {0, 0, 0, 0}, v2_int128 = {00000000000000000000000000000000, 00000000000000000000000000000000}}"},{number="206",value="{v8_float = {-9223372036854775808, 0, 0, 0, 0, 0, 0, 0}, v4_double = {0, 0, 0, 0}, v32_int8 = {0, 0, 0, -1, 0 <repeats 28 times>}, v16_int16 = {0, -256, 0 <repeats 14 times>}, v8_int32 = {-16777216, 0, 0, 0, 0, 0, 0, 0}, v4_int64 = {4278190080, 0, 0, 0}, v2_int128 = {00000000000000000000004278190080, 00000000000000000000000000000000}}"},{number="207",value="{v8_float = {0, 0, -9223372036854775808, -9223372036854775808, 0, 0, 0, 0}, v4_double = {0, -9223372036854775808, 0, 0}, v32_int8 = {0, -1, 0, 0, 0, 0, 0, 0, 0, 0, -1, -1, -1, -1, -1, -1, 0 <repeats 16 times>}, v16_int16 = {-256, 0, 0, 0, 0, -1, -1, -1, 0, 0, 0, 0, 0, 0, 0, 0}, v8_int32 = {65280, 0, -65536, -1, 0, 0, 0, 0}, v4_int64 = {65280, -65536, 0, 0}, v2_int128 = {340282366920937254537554992802593570560, 00000000000000000000000000000000}}"},{number="208",value="{v8_float = {-9223372036854775808, -9223372036854775808, -9223372036854775808, -9223372036854775808, 0, 0, 0, 0}, v4_double = {-9223372036854775808, -9223372036854775808, 0, 0}, v32_int8 = {-1 <repeats 16 times>, 0 <repeats 16 times>}, v16_int16 = {-1, -1, -1, -1, -1, -1, -1, -1, 0, 0, 0, 0, 0, 0, 0, 0}, v8_int32 = {-1, -1, -1, -1, 0, 0, 0, 0}, v4_int64 = {-1, -1, 0, 0}, v2_int128 = {340282366920938463463374607431768211455, 00000000000000000000000000000000}}"},{number="209",value="{v8_float = {0, 0, 0, 0, 0, 0, 0, 0}, v4_double = {0, 0, 0, 0}, v32_int8 = {0 <repeats 32 times>}, v16_int16 = {0 <repeats 16 times>}, v8_int32 = {0, 0, 0, 0, 0, 0, 0, 0}, v4_int64 = {0, 0, 0, 0}, v2_int128 = {00000000000000000000000000000000, 00000000000000000000000000000000}}"},{number="210",value="{v8_float = {0, 0, 0, 0, 0, 0, 0, 0}, v4_double = {0, 0, 0, 0}, v32_int8 = {0 <repeats 32 times>}, v16_int16 = {0 <repeats 16 times>}, v8_int32 = {0, 0, 0, 0, 0, 0, 0, 0}, v4_int64 = {0, 0, 0, 0}, v2_int128 = {00000000000000000000000000000000, 00000000000000000000000000000000}}"},{number="211",value="{v8_float = {0, 0, 0, 0, 0, 0, 0, 0}, v4_double = {0, 0, 0, 0}, v32_int8 = {0 <repeats 32 times>}, v16_int16 = {0 <repeats 16 times>}, v8_int32 = {0, 0, 0, 0, 0, 0, 0, 0}, v4_int64 = {0, 0, 0, 0}, v2_int128 = {00000000000000000000000000000000, 00000000000000000000000000000000}}"},{number="212",value="{v8_float = {0, 0, 0, 0, 0, 0, 0, 0}, v4_double = {0, 0, 0, 0}, v32_int8 = {0, 0, 0, 0, 0, 0, 0, 0, -56, 117, 117, 0 <repeats 21 times>}, v16_int16 = {0, 0, 0, 0, 30152, 117, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, v8_int32 = {0, 0, 7697864, 0, 0, 0, 0, 0}, v4_int64 = {0, 7697864, 0, 0}, v2_int128 = {00000142000527122222103840948224, 00000000000000000000000000000000}}"},{number="213",value="{v8_float = {0, 0, 0, 0, 0, 0, 0, 0}, v4_double = {0, 0, 0, 0}, v32_int8 = {0 <repeats 32 times>}, v16_int16 = {0 <repeats 16 times>}, v8_int32 = {0, 0, 0, 0, 0, 0, 0, 0}, v4_int64 = {0, 0, 0, 0}, v2_int128 = {00000000000000000000000000000000, 00000000000000000000000000000000}}"},{number="214",value="{v8_float = {0, 0, 0, 0, 0, 0, 0, 0}, v4_double = {0, 0, 0, 0}, v32_int8 = {0 <repeats 32 times>}, v16_int16 = {0 <repeats 16 times>}, v8_int32 = {0, 0, 0, 0, 0, 0, 0, 0}, v4_int64 = {0, 0, 0, 0}, v2_int128 = {00000000000000000000000000000000, 00000000000000000000000000000000}}"},{number="215",value="{v8_float = {-9223372036854775808, 0, 0, 0, 0, 0, 0, 0}, v4_double = {0, 0, 0, 0}, v32_int8 = {0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0 <repeats 17 times>}, v16_int16 = {0, -256, 0, 0, 0, 0, 0, 255, 0, 0, 0, 0, 0, 0, 0, 0}, v8_int32 = {-16777216, 0, 0, 16711680, 0, 0, 0, 0}, v4_int64 = {4278190080, 71776119061217280, 0, 0}, v2_int128 = {1324035698926381045275276568229314560, 00000000000000000000000000000000}}"},{number="216",value="{v8_float = {0, 0, 0, 0, 0, 0, 0, 0}, v4_double = {0, 0, 0, 0}, v32_int8 = {0 <repeats 32 times>}, v16_int16 = {0 <repeats 16 times>}, v8_int32 = {0, 0, 0, 0, 0, 0, 0, 0}, v4_int64 = {0, 0, 0, 0}, v2_int128 = {00000000000000000000000000000000, 00000000000000000000000000000000}}"},{number="217",value="{v8_float = {0, 0, 0, 0, 0, 0, 0, 0}, v4_double = {0, 0, 0, 0}, v32_int8 = {0 <repeats 32 times>}, v16_int16 = {0 <repeats 16 times>}, v8_int32 = {0, 0, 0, 0, 0, 0, 0, 0}, v4_int64 = {0, 0, 0, 0}, v2_int128 = {00000000000000000000000000000000, 00000000000000000000000000000000}}"},{number="218",value="{v8_float = {0, 0, 0, 0, 0, 0, 0, 0}, v4_double = {0, 0, 0, 0}, v32_int8 = {0 <repeats 32 times>}, v16_int16 = {0 <repeats 16 times>}, v8_int32 = {0, 0, 0, 0, 0, 0, 0, 0}, v4_int64 = {0, 0, 0, 0}, v2_int128 = {00000000000000000000000000000000, 00000000000000000000000000000000}}"},{number="219",value="{v8_float = {0, 0, 0, 0, 0, 0, 0, 0}, v4_double = {0, 0, 0, 0}, v32_int8 = {0 <repeats 32 times>}, v16_int16 = {0 <repeats 16 times>}, v8_int32 = {0, 0, 0, 0, 0, 0, 0, 0}, v4_int64 = {0, 0, 0, 0}, v2_int128 = {00000000000000000000000000000000, 00000000000000000000000000000000}}"}]

// ^done,stack=[frame={level="0",addr="0x00000000004a4e04",func="_D3std6getopt38__T6getoptTE3std6getopt6configTAyaTPbZ6getoptFKAAyaE3std6getopt6configAyaPbZS3std6getopt12GetoptResult",file="/usr/include/dmd/phobos/std/getopt.d",fullname="/usr/include/dmd/phobos/std/getopt.d",line="438"},frame={level="1",addr="0x000000000049fb82",func="D main",file="/home/basile/Dev/dproj/Resource.d/src/resource.d",fullname="/home/basile/Dev/dproj/Resource.d/src/resource.d",line="39"}]

// *stopped,reason="signal-received",signal-name="SIGSEGV",signal-meaning="Segmentation fault",frame={addr="0x000000000049fb89",func="D main",args=[{name="args",value="..."}],file="/home/basile/Dev/dproj/Resource.d/src/resource.d",fullname="/home/basile/Dev/dproj/Resource.d/src/resource.d",line="25"},thread-id="1",stopped-threads="all",core="3"

procedure TCEGdbWidget.interpretJson;
var
  i: integer;
  val: TJSONData;
  obj: TJSONObject;
  arr: TJSONArray;
  // common data
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
begin

  val := fJson.Find('reason');
  if val.isNotNil then
  begin
    reason := val.AsString;

    if (reason = 'breakpoint-hit') or (reason = 'end-stepping-range') then
    begin
      case reason of
        'breakpoint-hit': brkreason := dbBreakPoint;
        'end-stepping-range': brkreason := dbStep;
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
        if fDocHandler.findDocument(fullname).isNil then
          fDocHandler.openDocument(fullname);
        if fOptions.autoGetCallStack then
          infoStack;
        if fOptions.autoGetRegisters then
          infoRegs;
        subjDebugBreak(fSubj, fullname, line, brkreason);
      end;

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
        if  fDocHandler.findDocument(fullname).isNil then
          fDocHandler.openDocument(fullname);
        if fOptions.autoGetCallStack then
          infoStack;
        if fOptions.autoGetRegisters then
          infoRegs;
        subjDebugBreak(fSubj, fullname, line, dbSignal);
      end
      else
      begin
        if dlgYesNo(format('The signal %s (%s) was received on line %d of file %s .'
        + LineEnding + 'Do you wish to pause execution ?', [signame, sigmean, line, fullname]),
        'Unexpected signal received') = mrNo then
          gdbCommand('continue', @gdboutJsonize)
        else
        begin
          if not fDocHandler.findDocument(fullname).isNil then
            fDocHandler.openDocument(fullname);
          if fOptions.autoGetCallStack then
            infoStack;
          if fOptions.autoGetRegisters then
            infoRegs;
          subjDebugBreak(fSubj, fullname, line, dbSignal);
        end;
      end;
    end

    else if (reason = 'exited-normally') or (reason = 'exited-signalled') then
      subjDebugStop(fSubj);

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
        if val.isNotNil then
        begin
          if (0 <= number) and (TCpuRegister(number) <= high(TCpuRegister)) then
            fInspState.CPU.setInspectableRegister(TCpuRegister(number),
            {$IFDEF CPU64}val.AsInt64{$ELSE}val.AsInteger{$ENDIF});
        end;


        //else
          // TODO-cGDB: FPU and SSE regs are in a sub object
        //  break;
        // TODO-cGDB: set inspectable state
      end;
    end;
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

  if fOptions.showGdbOutput then
  begin
    arr := TJSONArray(fJson.Find('CLI'));
    if arr.isNotNil then
      for i := 0 to arr.Count-1 do
        fMsg.message(arr.Strings[i], nil, amcMisc, amkBub);
  end;

  if fOptions.showOutput then
  begin
    arr := TJSONArray(fJson.Find('OUT'));
    if arr.isNotNil then
      for i := 0 to arr.Count-1 do
        fMsg.message(arr.Strings[i], nil, amcMisc, amkBub);
  end;

end;

procedure TCEGdbWidget.gdboutJsonize(sender: TObject);
var
  str: string;
  lst: TStringList;
begin
  if fMsg = nil then
    exit;

  fLog.Clear;
  fGdb.getFullLines(fLog);
  //for str in fLog do
  //  fMsg.message(str, nil, amcMisc, amkAuto);

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

procedure TCEGdbWidget.gdboutQuiet(sender: TObject);
begin
  fGdb.OutputStack.Clear;
  fGdb.OnReadData:=@gdboutJsonize;
end;
{$ENDREGION}

{$REGIOn GDB commands & actions ------------------------------------------------}
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
  gdbCommand('-data-list-register-values d', @gdboutJsonize);
end;

procedure TCEGdbWidget.infoStack;
begin
  gdbCommand('-stack-list-frames', @gdboutJsonize);
end;

procedure TCEGdbWidget.btnStartClick(Sender: TObject);
begin
  startDebugging;
end;

procedure TCEGdbWidget.btnContClick(Sender: TObject);
begin
  gdbCommand('continue', @gdboutJsonize);
end;

procedure TCEGdbWidget.btnNextClick(Sender: TObject);
begin
  gdbCommand('step', @gdboutJsonize);
end;

procedure TCEGdbWidget.btnOverClick(Sender: TObject);
begin
  gdbCommand('next', @gdboutJsonize);
end;

procedure TCEGdbWidget.btnPauseClick(Sender: TObject);
begin
  pauseDebugee;
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
  pauseDebugee;
  gdbCommand('kill', @gdboutJsonize);
  subjDebugStop(fSubj);
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

procedure TCEGdbWidget.sendCustomCommand;
var
  cmd: string;
begin
  cmd := edit1.Text;
  if cmd.isBlank or cmd.isEmpty then
    exit;
  gdbCommand(cmd, @gdboutJsonize);
  if edit1.Items.IndexOf(cmd) = -1 then
    edit1.Items.Add(cmd);
  edit1.Text := '';
end;

procedure TCEGdbWidget.setGpr(reg: TCpuRegister; val: TCpuRegValue);
const
  spec = 'set $%s = 0x%X';
var
  cmd : string;
begin
  cmd := format(spec, [GetEnumName(typeinfo(TCpuRegister),integer(reg)), val]);
  gdbCommand(cmd);
end;

{$ENDREGION}

initialization
  RegisterPropertyEditor(TypeInfo(TCpuRegValue), nil, '', TCpuRegValueEditor);
end.

