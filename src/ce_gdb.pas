unit ce_gdb;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, ListFilterEdit, Forms, Controls, Graphics, RegExpr,
  ComCtrls, PropEdits, GraphPropEdits, RTTIGrids, Dialogs, ExtCtrls, Menus, strutils,
  Buttons, StdCtrls, process ,ce_common, ce_interfaces, ce_widget, ce_processes,
  ce_observer, ce_synmemo, ce_sharedres, ce_stringrange, fpjson;

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

  // Makes a category for the GP registers in a project inspector
  TInspectableGPR = class(TPersistent)
  end;

  // Makes a category for the FP registers in a project inspector
  TInspectableFPR = class(TPersistent)
  end;

  // Makes a category for the SSE registers in a project inspector
  TInspectableSSER = class(TPersistent)
  end;

  // Makes a category for the call stack in a project inspector
  TInspectableStack = class(Tpersistent)
  end;


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
    property EAX: TCpuRegValue read fRegisters[TCpuRegister.eax];
    property EBX: TCpuRegValue read fRegisters[TCpuRegister.ebx];
    property ECX: TCpuRegValue read fRegisters[TCpuRegister.ecx];
    property EDX: TCpuRegValue read fRegisters[TCpuRegister.edx];
    property ESI: TCpuRegValue read fRegisters[TCpuRegister.esi];
    property EDI: TCpuRegValue read fRegisters[TCpuRegister.edi];
    property EBP: TCpuRegValue read fRegisters[TCpuRegister.ebp];
    property ESP: TCpuRegValue read fRegisters[TCpuRegister.esp];
    property EIP: TCpuRegValue read fRegisters[TCpuRegister.eip];
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

  {
    breakpoint:

    (gdb)
    =breakpoint-modified,bkpt={number="2",type="breakpoint",disp="keep",enabled="y",addr="0x000000000049dc7a",func="D main",file="/home/basile/Dev/dproj/Resource.d/src/resource.d",fullname="/home/basile/Dev/dproj/Resource.d/src/resource.d",line="39",thread-groups=["i1"],times="1",original-location="/home/basile/Dev/dproj/Resource.d/src/resource.d:39"}
    ~"\nBreakpoint "
    ~"2, D main (args=...) at /home/basile/Dev/dproj/Resource.d/src/resource.d:39\n"
    ~"39\t    getopt(args, config.passThrough, \"h|help\", &wantHelp);\n"
    *stopped,reason="breakpoint-hit",disp="keep",bkptno="2",frame={addr="0x000000000049dc7a", func="D main",args=[{name="args",value="..."}],file="/home/basile/Dev/dproj/Resource.d/src/resource.d",fullname="/home/basile/Dev/dproj/Resource.d/src/resource.d",line="39"},thread-id="1",stopped-threads="all",core="3"
    (gdb)

    . line starting with = is to parse as TGDBMI_Breakpoint, thorically its [opt token]=, no token for breakpoint reached since it's not a result
    . lines starting with "~" can be ignored, they represent the output stream displayed in the CLI

  }
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
  TCEGdbWidget = class(TCEWidget, ICEProjectObserver, ICEDocumentObserver)
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
    fDoc: TCESynMemo;
    fProj: ICECommonProject;
    fJson: TJsonObject;
    fLog: TStringList;
    fFileLineBrks: TStringList;
    fDocHandler: ICEMultiDocHandler;
    fMsg: ICEMessagesDisplay;
    fGdb: TCEProcess;
    fInspState: TInspectableState;
    fShowCLI: boolean;
    //
    procedure startDebugging;
    procedure killGdb;
    procedure updateFileLineBrks;
    procedure editorModBrk(sender: TCESynMemo; line: integer; modification: TBreakPointModification);
    // GDB output processors
    procedure gdboutQuiet(sender: TObject);
    procedure gdboutJsonize(sender: TObject);
    procedure interpretJson;
    // GDB commands & actions
    procedure gdbCommand(aCommand: string; gdboutProcessor: TNotifyEvent = nil);
    procedure infoRegs;
    procedure infoStack;
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
  result := '0x' + IntToHex(GetOrdValue, 8);
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
  fJson := TJsonObject.Create;
  fShowCLI := true;
  //
  AssignPng(btnSendCom, 'ACCEPT');
end;

destructor TCEGdbWidget.destroy;
begin
  fFileLineBrks.Free;
  fLog.Free;
  killGdb;
  fInspState.Free;
  fJson.Free;
  EntitiesConnector.removeObserver(self);
  inherited;
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
  if document.isDSource then
    document.onBreakpointModify := @editorModBrk;
end;

procedure TCEGdbWidget.docFocused(document: TCESynMemo);
begin
  if document.isDSource then
    document.onBreakpointModify := @editorModBrk;
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
  fGdb := TCEProcess.create(nil);
  fGdb.Executable:= 'gdb' + exeExt;
  fgdb.Options:= [poUsePipes, poStderrToOutPut];
  fgdb.Parameters.Add(str);
  fgdb.Parameters.Add('--interpreter=mi');
  fGdb.OnReadData:= @gdboutQuiet;
  fGdb.OnTerminate:= @gdboutQuiet;
  fgdb.execute;
  // file:line breakpoints
  updateFileLineBrks;
  for i:= 0 to fFileLineBrks.Count-1 do
  begin
    str := 'break ' + fFileLineBrks.Strings[i] + ':' + intToStr(PtrUInt(fFileLineBrks.Objects[i])) + #10;
    fGdb.Input.Write(str[1], str.length);
  end;
  // break on druntime exceptions heper + throw'
  fGdb.OnReadData := @gdboutQuiet;
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
  fGdb.OnReadData := @gdboutJsonize;
  // launch
  gdbCommand('run');
end;
{$ENDREGION}

{$REGIOn GDB output processors -------------------------------------------------}
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
          break;
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
  json.Arrays['CLI'] := TJSONArray.Create;
  rng.init(str);
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
      // what would be output in a console by the debugee
      // ...
    end;
    // line is not interesting
    rng.popUntil(#10);
    if not rng.empty then
      rng.popFront;
  end;
end;

procedure TCEGdbWidget.interpretJson;
var
  val: TJSONData;
  obj: TJSONObject;
  arr: TJSONArray;
  idx: integer;
  // brkp data
  fne: string = '';
  lne: integer = -1;
begin

  val := fJson.Find('reason');
  if val.isNotNil and (val.AsString = 'breakpoint-hit') then
  begin
    obj := TJSONObject(fJson.Find('frame'));
    if obj.isNotNil and (obj.JSONType = jtObject) then
    begin
      val := obj.Find('fullname');
      if val.isNotNil then
        fne := val.AsString;
      val := obj.Find('line');
      if val.isNotNil then
        lne := strToInt(val.AsString);
      if (lne <> -1) and fne.fileExists then
      begin
        getMultiDocHandler.openDocument(fne);
        fDoc.setFocus;
        fDoc.CaretY:= lne;
      end;
    end;
  end;

  if fShowCLI then
  begin
    arr := TJSONArray(fJson.Find('CLI'));
    if arr.isNotNil then
      for idx := 0 to arr.Count-1 do
        fMsg.message(arr.Strings[idx], nil, amcMisc, amkBub);
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
  //  str := fGdbMessage.json.FormatJSON(DefaultFormat,2);
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
procedure TCEGdbWidget.gdbCommand(aCommand: string; gdboutProcessor: TNotifyEvent = nil);
begin
  if fGdb = nil then exit;
  if not fGdb.Running then exit;
  //
  aCommand += #10;
  if assigned(gdboutProcessor) then
    fGdb.OnReadData := gdboutProcessor;
  fGdb.Input.Write(aCommand[1], aCommand.length);
end;

procedure TCEGdbWidget.infoRegs;
begin
  // GDBMI output format, "info registers" is for CLI output
  gdbCommand('-data-list-register-values d', @gdboutJsonize);
end;

procedure TCEGdbWidget.infoStack;
begin
  // GDBMI output format, "info frame" is for CLI output
  gdbCommand('-stack-info-frame', @gdboutJsonize);
end;

procedure TCEGdbWidget.btnStartClick(Sender: TObject);
begin
  startDebugging;
end;

procedure TCEGdbWidget.btnContClick(Sender: TObject);
begin
  gdbCommand('continue', @gdboutJsonize);
end;

procedure TCEGdbWidget.btnRegClick(Sender: TObject);
begin
  infoRegs;
end;

procedure TCEGdbWidget.btnStopClick(Sender: TObject);
begin
  gdbCommand('kill', @gdboutQuiet);
  killGdb;
end;

procedure TCEGdbWidget.btnSendComClick(Sender: TObject);
begin
  gdbCommand(edit1.Text, @gdboutJsonize);
  edit1.Text := '';
end;

procedure TCEGdbWidget.btnStackClick(Sender: TObject);
begin
  infoStack;
end;

procedure TCEGdbWidget.Edit1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key <> byte(#13) then exit;
  gdbCommand(edit1.Text, @gdboutJsonize);
  edit1.Text := '';
end;
{$ENDREGION}
initialization
  RegisterPropertyEditor(TypeInfo(TCpuRegValue), nil, '', TCpuRegValueEditor);
end.

