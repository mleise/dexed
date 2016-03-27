unit ce_gdb;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, ListFilterEdit, Forms, Controls, Graphics, RegExpr,
  ComCtrls, PropEdits, GraphPropEdits, RTTIGrids, Dialogs, ExtCtrls, Menus, strutils,
  Buttons, StdCtrls, process ,ce_common, ce_interfaces, ce_widget, ce_processes,
  ce_observer, ce_synmemo, ce_sharedres, ce_stringrange;

type

  //TODO-cDebugging: write a parser for the DBG/MI output messages

  {$IFDEF CPU64}
  TCpuRegister = (rax, rbx, rcx, rdx, rsi, rdi, rbp, rsp, r8, r9, r10, r11, r12, r13,
    r14, r15, rip);
  {$ENDIF}

  {$IFDEF CPU32}
  TCpuRegister = (eax, ebx, ecx, edx, esi, edi, ebp, esp, eip);
  {$ENDIF}

  TFLAG = (CS, PF, AF, ZF, SF, TF, IF_, DF, OF_, NT, RF, VM,
    AC, VIF, VIP, ID);
  TEFLAG = set of TFLAG;

  TSegmentRegister = (S_CS, S_SS, S_DS, S_ES, S_FS, S_GS);

  // aliased to get hex display in object inspector.
  TCpuRegValue = type PtrInt;

  // Stores the stack and the registers content, to be displayable in
  // an object inspector.
  TInspectableState = class(TPersistent)
  private
    fWordSpliter: TRegExpr;
    fFlags: TEFLAG;
    fSegment: array[TSegmentRegister] of byte;
    fLastCalls: array[0..9] of string;
    fCallStack: TStringList;
    fRegisters: array[TCpuRegister] of TCpuRegValue;
  published
    property EFLAGS: TEFLAG read fFlags;
  {$IFDEF CPU64}
    property RAX: TCpuRegValue read fRegisters[TCpuRegister.rax];
    property RBX: TCpuRegValue read fRegisters[TCpuRegister.rbx];
    property RCX: TCpuRegValue read fRegisters[TCpuRegister.rcx];
    property RDX: TCpuRegValue read fRegisters[TCpuRegister.rdx];
    property RSI: TCpuRegValue read fRegisters[TCpuRegister.rsi];
    property RDI: TCpuRegValue read fRegisters[TCpuRegister.rdi];
    property RBP: TCpuRegValue read fRegisters[TCpuRegister.rbp];
    property RSP: TCpuRegValue read fRegisters[TCpuRegister.rsp];
    property R8:  TCpuRegValue read fRegisters[TCpuRegister.r8];
    property R9:  TCpuRegValue read fRegisters[TCpuRegister.r9];
    property R10: TCpuRegValue read fRegisters[TCpuRegister.r10];
    property R11: TCpuRegValue read fRegisters[TCpuRegister.r11];
    property R12: TCpuRegValue read fRegisters[TCpuRegister.r12];
    property R13: TCpuRegValue read fRegisters[TCpuRegister.r13];
    property R14: TCpuRegValue read fRegisters[TCpuRegister.r14];
    property R15: TCpuRegValue read fRegisters[TCpuRegister.r15];
    property RIP: TCpuRegValue read fRegisters[TCpuRegister.rip];
  {$ELSE}
    property EAX: TCpuRegValue read fRegisters[TCpuRegs.eax];
    property EBX: TCpuRegValue read fRegisters[TCpuRegs.ebx];
    property ECX: TCpuRegValue read fRegisters[TCpuRegs.ecx];
    property EDX: TCpuRegValue read fRegisters[TCpuRegs.edx];
    property ESI: TCpuRegValue read fRegisters[TCpuRegs.esi];
    property EDI: TCpuRegValue read fRegisters[TCpuRegs.edi];
    property EBP: TCpuRegValue read fRegisters[TCpuRegs.ebp];
    property ESP: TCpuRegValue read fRegisters[TCpuRegs.esp];
    property EIP: TCpuRegValue read fRegisters[TCpuRegs.eip];
  {$ENDIF}
    property CallStack_M0: string read fLastCalls[0];
    property CallStack_M1: string read fLastCalls[1];
    property CallStack_M2: string read fLastCalls[2];
    property CallStack_M3: string read fLastCalls[3];
    property CallStack_M4: string read fLastCalls[4];
    property CallStack_M5: string read fLastCalls[5];
    property CallStack_M6: string read fLastCalls[6];
    property CallStack_M7: string read fLastCalls[7];
    property CallStack_M8: string read fLastCalls[8];
    property CallStack_M9: string read fLastCalls[9];
    property CallStack: TStringList read fCallStack;
    //
    property CS: byte read fSegment[TSegmentRegister.S_CS];
    property DS: byte read fSegment[TSegmentRegister.S_DS];
    property ES: byte read fSegment[TSegmentRegister.S_ES];
    property FS: byte read fSegment[TSegmentRegister.S_FS];
    property GS: byte read fSegment[TSegmentRegister.S_GS];
    property SS: byte read fSegment[TSegmentRegister.S_SS];
  public
    constructor create;
    destructor destroy; override;
    // called on the result of "info stack"
    procedure parseCallStack(stream: TStream);
    // called on the result of "info register"
    procedure parseRegisters(stream: TStream);
  end;

  TCpuRegValueEditor = class(TIntegerProperty)
  public
    function GetValue: ansistring; override;
  end;


  TGDBMI_Frame = record
    level: integer;
    func: string;
    adrress: ptruint;
    fname: string;  // named "file"
    line: integer;
    from: string;
  end;

  TGDBMI_Breakpoint = record
    number: integer;
    tpe: string;        // named "type"
    catchtype: string;  // named "catch-type"
    disp: string;       // "del" | "keep"
    enabled: boolean;   // "y" | "n"
    addr: ptrUint;      // hex | <PENDING> | <MULTIPLE>
    func: string;
    filename: string;
    fullname: string;
    line: integer;
    at: string;
    pending: string;    // value is the command passed to set the BP
    evaluateby: string; // named "evaluate-by" , host | target
    thread: ptrUint;
    task: string;
    cond: string;
    ignore: integer;
    enable: integer;
    traceframeusage: string;// named "traceframe-usage"
    statictraceid: string;  // named "static-tracepoint-marker-string-id"
    mask: string;
    pass: integer;
    originloc: string; // named "original-location"
    times: integer;
    installed: boolean; // "y" | "n" , only for trace points
    what: string;
  end;

  TGDBMI_Thread = record
    id: ptrUint;
    targetid: string; // named "target-id"
    details: string;
    state: string;    // running | stopped
    core: integer;
  end;


  { TCEGdbWidget }
  TCEGdbWidget = class(TCEWidget, ICEProjectObserver, ICEMultiDocObserver)
    btnReg: TBitBtn;
    btnStack: TBitBtn;
    btnSendCom: TBitBtn;
    btnStop: TBitBtn;
    btnStart: TBitBtn;
    btnCont: TBitBtn;
    Edit1: TEdit;
    lstfilter: TListFilterEdit;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    stateViewer: TTIPropertyGrid;
    procedure btnContClick(Sender: TObject);
    procedure btnRegClick(Sender: TObject);
    procedure btnSendComClick(Sender: TObject);
    procedure btnStackClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure Edit1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    fProj: ICECommonProject;
    fLog: TStringList;
    fFileLineBrks: TStringList;
    fDocHandler: ICEMultiDocHandler;
    fMsg: ICEMessagesDisplay;
    fGdb: TCEAutoBufferedProcess;
    fInspState: TInspectableState;
    //
    procedure startDebugging;
    procedure killGdb;
    procedure updateFileLineBrks;
    procedure editorModBrk(sender: TCESynMemo; line: integer; modification: TBreakPointModification);
    // GDB output processors
    procedure processInfoRegs(sender: TObject);
    procedure processInfoStack(sender: TObject);
    procedure processSilently(sender: TObject);
    procedure gdbOutput(sender: TObject);
    // GDB commands & actions
    procedure gdbCommand(aCommand: string; outputCatcher: TNotifyEvent = nil);
    procedure infoRegs;
    procedure infoStack;
    //
    procedure projNew(aProject: ICECommonProject);
    procedure projChanged(aProject: ICECommonProject);
    procedure projClosing(aProject: ICECommonProject);
    procedure projFocused(aProject: ICECommonProject);
    procedure projCompiling(aProject: ICECommonProject);
    procedure projCompiled(aProject: ICECommonProject; success: boolean);
    //
    procedure docNew(aDoc: TCESynMemo);
    procedure docFocused(aDoc: TCESynMemo);
    procedure docChanged(aDoc: TCESynMemo);
    procedure docClosing(aDoc: TCESynMemo);
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
  end;


implementation
{$R *.lfm}

{$REGION TInspectableState -----------------------------------------------------}
function TCpuRegValueEditor.GetValue: ansistring;
begin
  {$IFDEF CPU64}
  result := '0x' + IntToHex(GetInt64Value, 16);
  {$ELSE}
  result := '0x' + IntToHex(GetInt64Value, 8);
  {$ENDIF}
end;

constructor TInspectableState.create;
begin
  fCallStack := TStringList.Create;
  fWordSpliter := TRegExpr.Create('[A-Za-z0-9_#]+');
  fWordSpliter.Compile;
end;

destructor TInspectableState.destroy;
begin
  fCallStack.free;
  fWordSpliter.Free;
  inherited;
end;

procedure TInspectableState.parseCallStack(stream: TStream);
var
  rdr: TStringList;
  str: string;
  i,j: integer;
begin
  fCallStack.Clear;
  for i := low(fLastCalls) to high(fLastCalls) do
    fLastCalls[i] := '';
  rdr := TStringList.Create;
  try
    rdr.LoadFromStream(stream);
    if (rdr.Count = 0) or (pos('(gdb)', rdr[0]) = -1) then
      exit;
    // fix first line
    str := rdr[0];
    rdr[0] := str[7 .. str.length];
    for str in rdr do
    begin
      if fWordSpliter.Exec(str) and fWordSpliter.ExecNext then
        fCallStack.Insert(0, str[fWordSpliter.MatchLen[0]+1 .. str.length]);
    end;
    if fCallStack.Count > 9 then j := 9 else j := fCallStack.Count-1;
    for i := 0 to j do
      fLastCalls[i] := fCallStack[i];
  finally
    rdr.free;
  end;
end;

procedure TInspectableState.parseRegisters(stream: TStream);
var
  rdr: TStringList;
  str: string;
  reg: string;
  val: string;
  rng: TStringRange = (ptr: nil; pos: 0; len: 0);
begin

  setLength(str, stream.Size);
  stream.Read(str[1], str.length);
  rng.init(str);

  if rng.empty then
    exit;
  if not rng.startsWith('&"info registers\n"') then
    exit;
  rng.popUntil(#10)^.popFront;

  while not rng.empty do
  begin
    if rng.front <> '~' then
      exit;
    rng.popFront;
    if rng.front <> '"' then
      exit;
    rng.popFront;

    reg := rng.takeUntil([' ', #9]).yield;

    if (reg = 'rip') or (reg = 'eip') then
    begin
      rng.popUntil(#10)^.popFront;
      rng.popUntil(#10)^.popFront;
      continue;
    end;

    rng.popUntil(#10)^.popFront;
    if rng.front <> '~' then
      exit;
    rng.popFront;
    if rng.front <> '"' then
      exit;
    rng.popFront;
    if rng.front <> '\' then
      exit;
    rng.popFront;

    if rng.front <> 't' then
      exit;
    rng.popFront;


    if reg = 'eflags' then
    begin
      fFlags := [];
      if rng.front <> '[' then
        exit;
      rng.popFront;
      while rng.front <> ']' do
      begin
        val := rng.popWhile([' ', #9])^.takeUntil([' ', #9, ']']).yield;
        case val of
          'CS': include(fFlags, TFLAG.CS);
          'PF': include(fFlags, TFLAG.PF);
          'AF': include(fFlags, TFLAG.AF);
          'ZF': include(fFlags, TFLAG.ZF);
          'SF': include(fFlags, TFLAG.SF);
          'TF': include(fFlags, TFLAG.TF);
          'IF': include(fFlags, TFLAG.IF_);
          'DF': include(fFlags, TFLAG.DF);
          'OF': include(fFlags, TFLAG.OF_);
          //'NT': include(fFlags, TFLAG.NT);
          //'RF': include(fFlags, TFLAG.RF);
          //'VM': include(fFlags, TFLAG.VM);
          //'AC': include(fFlags, TFLAG.AC);
          //'VIF':include(fFlags, TFLAG.VIF);
          //'VIP':include(fFlags, TFLAG.VIP);
          //'ID': include(fFlags, TFLAG.ID);
        end;
      end;
      rng.popUntil(#10)^.popFront;
      rng.popUntil(#10)^.popFront;
      continue;
    end;

    val := rng.takeWhile(['0','1','2','3','4','5','6','7','8','9']).yield;
    rng.popUntil(#10)^.popFront;
    rng.popUntil(#10)^.popFront;
    case reg of
      'cs':  fSegment[TSegmentRegister.S_CS] := StrToInt(val);
      'ds':  fSegment[TSegmentRegister.S_DS] := StrToInt(val);
      'es':  fSegment[TSegmentRegister.S_ES] := StrToInt(val);
      'fs':  fSegment[TSegmentRegister.S_FS] := StrToInt(val);
      'gs':  fSegment[TSegmentRegister.S_GS] := StrToInt(val);
      'ss':  fSegment[TSegmentRegister.S_SS] := StrToInt(val);
      {$IFDEF CPU64}
      'rax': fRegisters[TCpuRegister.rax] := StrToInt64(val);
      'rbx': fRegisters[TCpuRegister.rbx] := StrToInt64(val);
      'rcx': fRegisters[TCpuRegister.rcx] := StrToInt64(val);
      'rdx': fRegisters[TCpuRegister.rdx] := StrToInt64(val);
      'rdi': fRegisters[TCpuRegister.rdi] := StrToInt64(val);
      'rsi': fRegisters[TCpuRegister.rsi] := StrToInt64(val);
      'rbp': fRegisters[TCpuRegister.rbp] := StrToInt64(val);
      'rsp': fRegisters[TCpuRegister.rsp] := StrToInt64(val);
      'r8':  fRegisters[TCpuRegister.r8]  := StrToInt64(val);
      'r9':  fRegisters[TCpuRegister.r9]  := StrToInt64(val);
      'r10': fRegisters[TCpuRegister.r10] := StrToInt64(val);
      'r11': fRegisters[TCpuRegister.r11] := StrToInt64(val);
      'r12': fRegisters[TCpuRegister.r12] := StrToInt64(val);
      'r13': fRegisters[TCpuRegister.r13] := StrToInt64(val);
      'r14': fRegisters[TCpuRegister.r14] := StrToInt64(val);
      'r15': fRegisters[TCpuRegister.r15] := StrToInt64(val);
      'rip': fRegisters[TCpuRegister.rip] := StrToInt64(val);
      {$ELSE}
      'eax': fRegisters[TCpuRegister.eax] := StrToInt(val);
      'ebx': fRegisters[TCpuRegister.ebx] := StrToInt(val);
      'ecx': fRegisters[TCpuRegister.ecx] := StrToInt(val);
      'edx': fRegisters[TCpuRegister.edx] := StrToInt(val);
      'edi': fRegisters[TCpuRegister.edi] := StrToInt(val);
      'esi': fRegisters[TCpuRegister.esi] := StrToInt(val);
      'ebp': fRegisters[TCpuRegister.ebp] := StrToInt(val);
      'esp': fRegisters[TCpuRegister.esp] := StrToInt(val);
      'eip': fRegisters[TCpuRegister.eip] := StrToInt(val);
      {$ENDIF}
    end;

  end;
end;
{$ENDREGION}

{$REGION Common/standard comp --------------------------------------------------}
constructor TCEGdbWidget.create(aOwner: TComponent);
begin
  inherited;
  EntitiesConnector.addObserver(self);
  fDocHandler:= getMultiDocHandler;
  fMsg:= getMessageDisplay;
  fFileLineBrks:= TStringList.Create;
  fLog := TStringList.Create;
  fInspState := TInspectableState.Create;
  stateViewer.TIObject := fInspState;
  //
  AssignPng(btnSendCom, 'accept');
end;

destructor TCEGdbWidget.destroy;
begin
  fFileLineBrks.Free;
  fLog.Free;
  killGdb;
  fInspState.Free;
  EntitiesConnector.removeObserver(self);
  inherited;
end;
{$ENDREGION}

{$REGION ICEProjectObserver ----------------------------------------------------}
procedure TCEGdbWidget.projNew(aProject: ICECommonProject);
begin
  fProj := aProject;
end;

procedure TCEGdbWidget.projChanged(aProject: ICECommonProject);
begin
  if fProj <> aProject then
    exit;
end;

procedure TCEGdbWidget.projClosing(aProject: ICECommonProject);
begin
  if fProj <> aProject then
    exit;
  fProj := nil;
end;

procedure TCEGdbWidget.projFocused(aProject: ICECommonProject);
begin
  fProj := aProject;
end;

procedure TCEGdbWidget.projCompiling(aProject: ICECommonProject);
begin
end;

procedure TCEGdbWidget.projCompiled(aProject: ICECommonProject; success: boolean);
begin
end;
{$ENDREGION}

{$REGION ICEMultiDocObserver ---------------------------------------------------}
procedure TCEGdbWidget.docNew(aDoc: TCESynMemo);
begin
  if aDoc.isDSource then
    aDoc.onBreakpointModify := @editorModBrk;
end;

procedure TCEGdbWidget.docFocused(aDoc: TCESynMemo);
begin
  if aDoc.isDSource then
    aDoc.onBreakpointModify := @editorModBrk;
end;

procedure TCEGdbWidget.docChanged(aDoc: TCESynMemo);
begin
end;

procedure TCEGdbWidget.docClosing(aDoc: TCESynMemo);
begin
end;
{$ENDREGION}

{$REGION Unsorted Debugging things ---------------------------------------------}
procedure TCEGdbWidget.killGdb;
begin
  if not assigned(fGdb) then
    exit;
  if fGdb.Running then
    fGdb.Terminate(0);
  FreeAndNil(fGdb);
end;

procedure TCEGdbWidget.updateFileLineBrks;
var
  i,j: integer;
  doc: TCESynMemo;
  nme: string;
begin
  fFileLineBrks.Clear;
  if fDocHandler = nil then exit;
  //
  for i:= 0 to fDocHandler.documentCount-1 do
  begin
    doc := fDocHandler.document[i];
    if not doc.isDSource then
      continue;
    nme := doc.fileName;
    if not nme.fileExists then
      continue;
    {$PUSH}{$WARNINGS OFF}{$HINTS OFF}
    for j := 0 to doc.breakPointsCount-1 do
      fFileLineBrks.AddObject(nme, TObject(pointer(doc.BreakPointLine(j))));
    {$POP}
  end;
end;

procedure TCEGdbWidget.editorModBrk(sender: TCESynMemo; line: integer; modification: TBreakPointModification);
var
  str: string;
  nme: string;
const
  cmd: array[TBreakPointModification] of string = ('break ', 'clear ');
begin
  // set only breakpoint in live, while debugging
  // note: only works if execution is paused (breakpoint)
  // and not inside a loop (for ex. with sleep).
  if fGdb = nil then exit;
  if not fGdb.Running then exit;
  nme := sender.fileName;
  if not nme.fileExists then exit;
  //
  str := cmd[modification] + nme + ':' + intToStr(line);
  fGdb.Suspend;
  gdbCommand(str);
  fGdb.Resume;
end;

procedure TCEGdbWidget.startDebugging;
var
  str: string;
  i: integer;
begin
  // protect
  if fProj = nil then exit;
  if fProj.binaryKind <> executable then exit;
  str := fProj.outputFilename;
  if not str.fileExists then exit;
  // gdb process
  killGdb;
  fGdb := TCEAutoBufferedProcess.create(nil);
  fGdb.Executable:= 'gdb' + exeExt;
  fgdb.Options:= [poUsePipes, poStderrToOutPut];
  fgdb.Parameters.Add(str);
  fgdb.Parameters.Add('--interpreter=mi');
  fGdb.OnReadData:= @gdbOutput;
  fGdb.OnTerminate:= @gdbOutput;
  fgdb.execute;
  // file:line breakpoints
  updateFileLineBrks;
  for i:= 0 to fFileLineBrks.Count-1 do
  begin
    str := 'break ' + fFileLineBrks.Strings[i] + ':' + intToStr(PtrUInt(fFileLineBrks.Objects[i])) + #10;
    fGdb.Input.Write(str[1], str.length);
  end;
  // break on druntime exceptions heper + throw'
  fGdb.OnReadData := @processSilently;
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
  fGdb.OnReadData := @gdbOutput;
  // launch
  gdbCommand('run');
end;
{$ENDREGION}

{$REGIOn GDB output processors -------------------------------------------------}
procedure TCEGdbWidget.gdbOutput(sender: TObject);
var
  str: string;
begin
  if fMsg = nil then
    exit;
  fLog.Clear;
  fGdb.getFullLines(fLog);
  for str in fLog do
    fMsg.message(str, nil, amcMisc, amkAuto);
end;

procedure TCEGdbWidget.processSilently(sender: TObject);
begin
  fGdb.OutputStack.Clear;
  fGdb.OnReadData:=@gdbOutput;
end;

procedure TCEGdbWidget.processInfoRegs(sender: TObject);
begin
  try
    fInspState.parseRegisters(fgdb.OutputStack);
    fgdb.OutputStack.Clear;
  finally
    fGdb.OnReadData:=@gdbOutput;
  end;
end;

procedure TCEGdbWidget.processInfoStack(sender: TObject);
begin
  try
    fInspState.parseCallStack(fgdb.OutputStack);
    fgdb.OutputStack.Clear;
  finally
    fGdb.OnReadData:=@gdbOutput;
  end;
end;
{$ENDREGION}

{$REGIOn GDB commands & actions ------------------------------------------------}
procedure TCEGdbWidget.gdbCommand(aCommand: string; outputCatcher: TNotifyEvent = nil);
begin
  if fGdb = nil then exit;
  if not fGdb.Running then exit;
  //
  aCommand += #10;
  if assigned(outputCatcher) then
    fGdb.OnReadData := outputCatcher;
  fGdb.Input.Write(aCommand[1], aCommand.length);
end;

procedure TCEGdbWidget.infoRegs;
begin
  gdbCommand('info registers', @processInfoRegs);
end;

procedure TCEGdbWidget.infoStack;
begin
  gdbCommand('info stack', @processInfoStack);
end;

procedure TCEGdbWidget.btnStartClick(Sender: TObject);
begin
  startDebugging;
end;

procedure TCEGdbWidget.btnContClick(Sender: TObject);
begin
  gdbCommand('continue');
end;

procedure TCEGdbWidget.btnRegClick(Sender: TObject);
begin
  infoRegs;
end;

procedure TCEGdbWidget.btnStopClick(Sender: TObject);
begin
  gdbCommand('kill');
  killGdb;
end;

procedure TCEGdbWidget.btnSendComClick(Sender: TObject);
begin
  gdbCommand(edit1.Text);
  edit1.Text := '';
end;

procedure TCEGdbWidget.btnStackClick(Sender: TObject);
begin
  infoStack;
end;

procedure TCEGdbWidget.Edit1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key <> byte(#13) then exit;
  gdbCommand(edit1.Text);
  edit1.Text := '';
end;
{$ENDREGION}
initialization
  RegisterPropertyEditor(TypeInfo(TCpuRegValue), nil, '', TCpuRegValueEditor);
end.

