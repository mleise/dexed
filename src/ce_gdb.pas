unit ce_gdb;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, ListFilterEdit, Forms, Controls, Graphics,
  RegExpr, ComCtrls, PropEdits, GraphPropEdits, RTTIGrids, Dialogs, ExtCtrls,
  Menus, strutils, Buttons, StdCtrls, process, ce_common, ce_interfaces,
  ce_widget, ce_processes, ce_observer, ce_synmemo, ce_sharedres,
  ce_stringrange, ce_dsgncontrols, fpjson;

type

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

  TCEDebugWidgetOptions = class
    fDemangle: boolean;
    fShowCLI: boolean;
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
    btnContinue: TCEToolButton;
    btnPause: TCEToolButton;
    btnReg: TCEToolButton;
    btnStack: TCEToolButton;
    btnStart: TCEToolButton;
    btnStop: TCEToolButton;
    button0: TCEToolButton;
    button1: TCEToolButton;
    button4: TCEToolButton;
    Edit1: TEdit;
    lstCallStack: TListView;
    Panel2: TPanel;
    Panel3: TPanel;
    btnSendCom: TSpeedButton;
    stateViewer: TTIPropertyGrid;
    procedure btnContClick(Sender: TObject);
    procedure btnRegClick(Sender: TObject);
    procedure btnSendComClick(Sender: TObject);
    procedure btnStackClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure Edit1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected
    procedure setToolBarFlat(value: boolean); override;
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
    fStackItems: TStackItems;
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
  fStackItems := TStackItems.create;
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
  fStackItems.Free;
  EntitiesConnector.removeObserver(self);
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
        'a'..'z':
        begin
          r^.takeUntil('=').yield;
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

// ^done,register-values=[{number="0",value="0"},{number="1",value="140737488347232"},{number="2",value="140737349740925"},{number="3",value="140737488346960"},{number="4",value="140737488346960"},{number="5",value="1"},{number="6",value="140737488346752"},{number="7",value="140737488343952"},{number="8",value="0"},{number="9",value="140737488346176"},{number="10",value="8"},{number="11",value="518"},{number="12",value="140737488347040"},{number="13",value="140737488347024"},{number="14",value="1"},{number="15",value="140737488346960"},{number="16",value="4848450"},{number="17",value="582"},{number="18",value="51"},{number="19",value="43"},{number="20",value="0"},{number="21",value="0"},{number="22",value="0"},{number="23",value="0"},{number="24",value="0"},{number="25",value="0"},{number="26",value="0"},{number="27",value="0"},{number="28",value="0"},{number="29",value="0"},{number="30",value="0"},{number="31",value="0"},{number="32",value="895"},{number="33",value="0"},{number="34",value="65535"},{number="35",value="0"},{number="36",value="0"},{number="37",value="0"},{number="38",value="0"},{number="39",value="0"},{number="40",value="{v4_float = {0, 0, -9223372036854775808, 0}, v2_double = {0, 0}, v16_int8 = {0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0}, v8_int16 = {-256, 0, 0, 0, 0, -256, 0, 0}, v4_int32 = {65280, 0, -16777216, 0}, v2_int64 = {65280, 4278190080}, uint128 = 00078918677504442992524819234560}"},{number="41",value="{v4_float = {0, 0, 0, 0}, v2_double = {0, 0}, v16_int8 = {0 <repeats 16 times>}, v8_int16 = {0, 0, 0, 0, 0, 0, 0, 0}, v4_int32 = {0, 0, 0, 0}, v2_int64 = {0, 0}, uint128 = 00000000000000000000000000000000}"},{number="42",value="{v4_float = {-9223372036854775808, 0, 0, 0}, v2_double = {0, 0}, v16_int8 = {0, 0, 0, -1, 0 <repeats 12 times>}, v8_int16 = {0, -256, 0, 0, 0, 0, 0, 0}, v4_int32 = {-16777216, 0, 0, 0}, v2_int64 = {4278190080, 0}, uint128 = 00000000000000000000004278190080}"},{number="43",value="{v4_float = {0, 0, -9223372036854775808, -9223372036854775808}, v2_double = {0, -9223372036854775808}, v16_int8 = {0, -1, 0, 0, 0, 0, 0, 0, 0, 0, -1, -1, -1, -1, -1, -1}, v8_int16 = {-256, 0, 0, 0, 0, -1, -1, -1}, v4_int32 = {65280, 0, -65536, -1}, v2_int64 = {65280, -65536}, uint128 = 340282366920937254537554992802593570560}"},{number="44",value="{v4_float = {-9223372036854775808, -9223372036854775808, -9223372036854775808, -9223372036854775808}, v2_double = {-9223372036854775808, -9223372036854775808}, v16_int8 = {-1 <repeats 16 times>}, v8_int16 = {-1, -1, -1, -1, -1, -1, -1, -1}, v4_int32 = {-1, -1, -1, -1}, v2_int64 = {-1, -1}, uint128 = 340282366920938463463374607431768211455}"},{number="45",value="{v4_float = {0, 0, 0, 0}, v2_double = {0, 0}, v16_int8 = {0 <repeats 16 times>}, v8_int16 = {0, 0, 0, 0, 0, 0, 0, 0}, v4_int32 = {0, 0, 0, 0}, v2_int64 = {0, 0}, uint128 = 00000000000000000000000000000000}"},{number="46",value="{v4_float = {0, 0, 0, 0}, v2_double = {0, 0}, v16_int8 = {0 <repeats 16 times>}, v8_int16 = {0, 0, 0, 0, 0, 0, 0, 0}, v4_int32 = {0, 0, 0, 0}, v2_int64 = {0, 0}, uint128 = 00000000000000000000000000000000}"},{number="47",value="{v4_float = {0, 0, 0, 0}, v2_double = {0, 0}, v16_int8 = {0 <repeats 16 times>}, v8_int16 = {0, 0, 0, 0, 0, 0, 0, 0}, v4_int32 = {0, 0, 0, 0}, v2_int64 = {0, 0}, uint128 = 00000000000000000000000000000000}"},{number="48",value="{v4_float = {0, 0, 0, 0}, v2_double = {0, 0}, v16_int8 = {0, 0, 0, 0, 0, 0, 0, 0, -56, 117, 117, 0, 0, 0, 0, 0}, v8_int16 = {0, 0, 0, 0, 30152, 117, 0, 0}, v4_int32 = {0, 0, 7697864, 0}, v2_int64 = {0, 7697864}, uint128 = 00000142000527122222103840948224}"},{number="49",value="{v4_float = {0, 0, 0, 0}, v2_double = {0, 0}, v16_int8 = {0 <repeats 16 times>}, v8_int16 = {0, 0, 0, 0, 0, 0, 0, 0}, v4_int32 = {0, 0, 0, 0}, v2_int64 = {0, 0}, uint128 = 00000000000000000000000000000000}"},{number="50",value="{v4_float = {0, 0, 0, 0}, v2_double = {0, 0}, v16_int8 = {0 <repeats 16 times>}, v8_int16 = {0, 0, 0, 0, 0, 0, 0, 0}, v4_int32 = {0, 0, 0, 0}, v2_int64 = {0, 0}, uint128 = 00000000000000000000000000000000}"},{number="51",value="{v4_float = {-9223372036854775808, 0, 0, 0}, v2_double = {0, 0}, v16_int8 = {0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0}, v8_int16 = {0, -256, 0, 0, 0, 0, 0, 255}, v4_int32 = {-16777216, 0, 0, 16711680}, v2_int64 = {4278190080, 71776119061217280}, uint128 = 1324035698926381045275276568229314560}"},{number="52",value="{v4_float = {0, 0, 0, 0}, v2_double = {0, 0}, v16_int8 = {0 <repeats 16 times>}, v8_int16 = {0, 0, 0, 0, 0, 0, 0, 0}, v4_int32 = {0, 0, 0, 0}, v2_int64 = {0, 0}, uint128 = 00000000000000000000000000000000}"},{number="53",value="{v4_float = {0, 0, 0, 0}, v2_double = {0, 0}, v16_int8 = {0 <repeats 16 times>}, v8_int16 = {0, 0, 0, 0, 0, 0, 0, 0}, v4_int32 = {0, 0, 0, 0}, v2_int64 = {0, 0}, uint128 = 00000000000000000000000000000000}"},{number="54",value="{v4_float = {0, 0, 0, 0}, v2_double = {0, 0}, v16_int8 = {0 <repeats 16 times>}, v8_int16 = {0, 0, 0, 0, 0, 0, 0, 0}, v4_int32 = {0, 0, 0, 0}, v2_int64 = {0, 0}, uint128 = 00000000000000000000000000000000}"},{number="55",value="{v4_float = {0, 0, 0, 0}, v2_double = {0, 0}, v16_int8 = {0 <repeats 16 times>}, v8_int16 = {0, 0, 0, 0, 0, 0, 0, 0}, v4_int32 = {0, 0, 0, 0}, v2_int64 = {0, 0}, uint128 = 00000000000000000000000000000000}"},{number="56",value="8064"},{number="57",value="00000000000000000000000000000000"},{number="58",value="00000000000000000000000000000000"},{number="59",value="00000000000000000000000000000000"},{number="60",value="00000000000000000000000000000000"},{number="61",value="00000000000000000000000000000000"},{number="62",value="00000000000000000000000000000000"},{number="63",value="00000000000000000000000000000000"},{number="64",value="00000000000000000000000000000000"},{number="65",value="00000000000000000000000000000000"},{number="66",value="00000000000000000000000000000000"},{number="67",value="00000000000000000000000000000000"},{number="68",value="00000000000000000000000000000000"},{number="69",value="00000000000000000000000000000000"},{number="70",value="00000000000000000000000000000000"},{number="71",value="00000000000000000000000000000000"},{number="72",value="00000000000000000000000000000000"},{number="151",value="-1"},{number="152",value="0"},{number="153",value="96"},{number="154",value="125"},{number="155",value="80"},{number="156",value="80"},{number="157",value="1"},{number="158",value="-128"},{number="159",value="-112"},{number="160",value="0"},{number="161",value="64"},{number="162",value="8"},{number="163",value="6"},{number="164",value="-96"},{number="165",value="-112"},{number="166",value="1"},{number="167",value="80"},{number="168",value="0"},{number="169",value="-32"},{number="170",value="-23"},{number="171",value="-33"},{number="172",value="0"},{number="173",value="-8096"},{number="174",value="-5763"},{number="175",value="-8368"},{number="176",value="-8368"},{number="177",value="1"},{number="178",value="-8576"},{number="180",value="0"},{number="181",value="-9152"},{number="182",value="8"},{number="183",value="518"},{number="184",value="-8288"},{number="185",value="-8304"},{number="186",value="1"},{number="187",value="-8368"},{number="188",value="0"},{number="189",value="-8096"},{number="190",value="-138614403"},{number="191",value="-8368"},{number="192",value="-8368"},{number="193",value="1"},{number="194",value="-8576"},{number="195",value="-11376"},{number="196",value="0"},{number="197",value="-9152"},{number="198",value="8"},{number="199",value="518"},{number="200",value="-8288"},{number="201",value="-8304"},{number="202",value="1"},{number="203",value="-8368"},{number="204",value="{v8_float = {0, 0, -9223372036854775808, 0, 0, 0, 0, 0}, v4_double = {0, 0, 0, 0}, v32_int8 = {0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0 <repeats 20 times>}, v16_int16 = {-256, 0, 0, 0, 0, -256, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, v8_int32 = {65280, 0, -16777216, 0, 0, 0, 0, 0}, v4_int64 = {65280, 4278190080, 0, 0}, v2_int128 = {00078918677504442992524819234560, 00000000000000000000000000000000}}"},{number="205",value="{v8_float = {0, 0, 0, 0, 0, 0, 0, 0}, v4_double = {0, 0, 0, 0}, v32_int8 = {0 <repeats 32 times>}, v16_int16 = {0 <repeats 16 times>}, v8_int32 = {0, 0, 0, 0, 0, 0, 0, 0}, v4_int64 = {0, 0, 0, 0}, v2_int128 = {00000000000000000000000000000000, 00000000000000000000000000000000}}"},{number="206",value="{v8_float = {-9223372036854775808, 0, 0, 0, 0, 0, 0, 0}, v4_double = {0, 0, 0, 0}, v32_int8 = {0, 0, 0, -1, 0 <repeats 28 times>}, v16_int16 = {0, -256, 0 <repeats 14 times>}, v8_int32 = {-16777216, 0, 0, 0, 0, 0, 0, 0}, v4_int64 = {4278190080, 0, 0, 0}, v2_int128 = {00000000000000000000004278190080, 00000000000000000000000000000000}}"},{number="207",value="{v8_float = {0, 0, -9223372036854775808, -9223372036854775808, 0, 0, 0, 0}, v4_double = {0, -9223372036854775808, 0, 0}, v32_int8 = {0, -1, 0, 0, 0, 0, 0, 0, 0, 0, -1, -1, -1, -1, -1, -1, 0 <repeats 16 times>}, v16_int16 = {-256, 0, 0, 0, 0, -1, -1, -1, 0, 0, 0, 0, 0, 0, 0, 0}, v8_int32 = {65280, 0, -65536, -1, 0, 0, 0, 0}, v4_int64 = {65280, -65536, 0, 0}, v2_int128 = {340282366920937254537554992802593570560, 00000000000000000000000000000000}}"},{number="208",value="{v8_float = {-9223372036854775808, -9223372036854775808, -9223372036854775808, -9223372036854775808, 0, 0, 0, 0}, v4_double = {-9223372036854775808, -9223372036854775808, 0, 0}, v32_int8 = {-1 <repeats 16 times>, 0 <repeats 16 times>}, v16_int16 = {-1, -1, -1, -1, -1, -1, -1, -1, 0, 0, 0, 0, 0, 0, 0, 0}, v8_int32 = {-1, -1, -1, -1, 0, 0, 0, 0}, v4_int64 = {-1, -1, 0, 0}, v2_int128 = {340282366920938463463374607431768211455, 00000000000000000000000000000000}}"},{number="209",value="{v8_float = {0, 0, 0, 0, 0, 0, 0, 0}, v4_double = {0, 0, 0, 0}, v32_int8 = {0 <repeats 32 times>}, v16_int16 = {0 <repeats 16 times>}, v8_int32 = {0, 0, 0, 0, 0, 0, 0, 0}, v4_int64 = {0, 0, 0, 0}, v2_int128 = {00000000000000000000000000000000, 00000000000000000000000000000000}}"},{number="210",value="{v8_float = {0, 0, 0, 0, 0, 0, 0, 0}, v4_double = {0, 0, 0, 0}, v32_int8 = {0 <repeats 32 times>}, v16_int16 = {0 <repeats 16 times>}, v8_int32 = {0, 0, 0, 0, 0, 0, 0, 0}, v4_int64 = {0, 0, 0, 0}, v2_int128 = {00000000000000000000000000000000, 00000000000000000000000000000000}}"},{number="211",value="{v8_float = {0, 0, 0, 0, 0, 0, 0, 0}, v4_double = {0, 0, 0, 0}, v32_int8 = {0 <repeats 32 times>}, v16_int16 = {0 <repeats 16 times>}, v8_int32 = {0, 0, 0, 0, 0, 0, 0, 0}, v4_int64 = {0, 0, 0, 0}, v2_int128 = {00000000000000000000000000000000, 00000000000000000000000000000000}}"},{number="212",value="{v8_float = {0, 0, 0, 0, 0, 0, 0, 0}, v4_double = {0, 0, 0, 0}, v32_int8 = {0, 0, 0, 0, 0, 0, 0, 0, -56, 117, 117, 0 <repeats 21 times>}, v16_int16 = {0, 0, 0, 0, 30152, 117, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, v8_int32 = {0, 0, 7697864, 0, 0, 0, 0, 0}, v4_int64 = {0, 7697864, 0, 0}, v2_int128 = {00000142000527122222103840948224, 00000000000000000000000000000000}}"},{number="213",value="{v8_float = {0, 0, 0, 0, 0, 0, 0, 0}, v4_double = {0, 0, 0, 0}, v32_int8 = {0 <repeats 32 times>}, v16_int16 = {0 <repeats 16 times>}, v8_int32 = {0, 0, 0, 0, 0, 0, 0, 0}, v4_int64 = {0, 0, 0, 0}, v2_int128 = {00000000000000000000000000000000, 00000000000000000000000000000000}}"},{number="214",value="{v8_float = {0, 0, 0, 0, 0, 0, 0, 0}, v4_double = {0, 0, 0, 0}, v32_int8 = {0 <repeats 32 times>}, v16_int16 = {0 <repeats 16 times>}, v8_int32 = {0, 0, 0, 0, 0, 0, 0, 0}, v4_int64 = {0, 0, 0, 0}, v2_int128 = {00000000000000000000000000000000, 00000000000000000000000000000000}}"},{number="215",value="{v8_float = {-9223372036854775808, 0, 0, 0, 0, 0, 0, 0}, v4_double = {0, 0, 0, 0}, v32_int8 = {0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0 <repeats 17 times>}, v16_int16 = {0, -256, 0, 0, 0, 0, 0, 255, 0, 0, 0, 0, 0, 0, 0, 0}, v8_int32 = {-16777216, 0, 0, 16711680, 0, 0, 0, 0}, v4_int64 = {4278190080, 71776119061217280, 0, 0}, v2_int128 = {1324035698926381045275276568229314560, 00000000000000000000000000000000}}"},{number="216",value="{v8_float = {0, 0, 0, 0, 0, 0, 0, 0}, v4_double = {0, 0, 0, 0}, v32_int8 = {0 <repeats 32 times>}, v16_int16 = {0 <repeats 16 times>}, v8_int32 = {0, 0, 0, 0, 0, 0, 0, 0}, v4_int64 = {0, 0, 0, 0}, v2_int128 = {00000000000000000000000000000000, 00000000000000000000000000000000}}"},{number="217",value="{v8_float = {0, 0, 0, 0, 0, 0, 0, 0}, v4_double = {0, 0, 0, 0}, v32_int8 = {0 <repeats 32 times>}, v16_int16 = {0 <repeats 16 times>}, v8_int32 = {0, 0, 0, 0, 0, 0, 0, 0}, v4_int64 = {0, 0, 0, 0}, v2_int128 = {00000000000000000000000000000000, 00000000000000000000000000000000}}"},{number="218",value="{v8_float = {0, 0, 0, 0, 0, 0, 0, 0}, v4_double = {0, 0, 0, 0}, v32_int8 = {0 <repeats 32 times>}, v16_int16 = {0 <repeats 16 times>}, v8_int32 = {0, 0, 0, 0, 0, 0, 0, 0}, v4_int64 = {0, 0, 0, 0}, v2_int128 = {00000000000000000000000000000000, 00000000000000000000000000000000}}"},{number="219",value="{v8_float = {0, 0, 0, 0, 0, 0, 0, 0}, v4_double = {0, 0, 0, 0}, v32_int8 = {0 <repeats 32 times>}, v16_int16 = {0 <repeats 16 times>}, v8_int32 = {0, 0, 0, 0, 0, 0, 0, 0}, v4_int64 = {0, 0, 0, 0}, v2_int128 = {00000000000000000000000000000000, 00000000000000000000000000000000}}"}]

// ^done,stack=[frame={level="0",addr="0x00000000004a4e04",func="_D3std6getopt38__T6getoptTE3std6getopt6configTAyaTPbZ6getoptFKAAyaE3std6getopt6configAyaPbZS3std6getopt12GetoptResult",file="/usr/include/dmd/phobos/std/getopt.d",fullname="/usr/include/dmd/phobos/std/getopt.d",line="438"},frame={level="1",addr="0x000000000049fb82",func="D main",file="/home/basile/Dev/dproj/Resource.d/src/resource.d",fullname="/home/basile/Dev/dproj/Resource.d/src/resource.d",line="39"}]



procedure TCEGdbWidget.interpretJson;
var
  val: TJSONData;
  obj: TJSONObject;
  arr: TJSONArray;
  idx: integer;
  // brkp data
  fne: string = '';
  lne: integer = -1;
  // registers data
  rnm: integer = 0;
  rvl: PtrUInt = 0;
  // call stack data
  nme: string;
  adr: PtrUInt;
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

  val := fJson.Find('register-values');
  if val.isNotNil and (val.JSONType = jtArray) then
  begin
    arr := TJSONArray(val);
    for idx := 0 to arr.Count-1 do
    begin
      obj := TJSONObject(arr.Objects[idx]);
      if obj.isNil then
        break
      else
      begin
        val := obj.Find('number');
        if val.isNotNil then
          rnm := strToInt(val.AsString);
        val := obj.Find('value');
        //if val.isNotNil and (val.JSONType = jtString) then
        //  rvl := StrToInt64(val.AsString)
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
    for idx := 0 to arr.Count-1 do
    begin
      obj := arr.Objects[idx];
      if obj.isNil then
        break;
      val := obj.Find('fullname');
      if val.isNotNil then
        fne:= val.AsString;
      // TODO-cGDB: demangle function name.
      val := obj.Find('func');
      if val.isNotNil then
        nme:= val.AsString;
      val := obj.Find('addr');
      if val.isNotNil then
        adr := val.AsInt64;
      val := obj.Find('line');
      if val.isNotNil then
        lne := val.AsInteger;
      fStackItems.addItem(adr, fne, nme, lne);
    end;
    fStackItems.assignToList(lstCallStack);
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
  for str in fLog do
    fMsg.message(str, nil, amcMisc, amkAuto);

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

