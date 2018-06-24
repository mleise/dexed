unit ce_processes;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, ExtCtrls, process, asyncprocess;

type

  //TODO: follow up https://bugs.freepascal.org/view.php?id=33897
  //which is the cause of the timer workaround

  {
    The stanndard process wrapper used in Coedit.

    This class solves several issues encountered when using TProcess and TAsyncProcess:

    -   OnTerminate event is never called under Linux.
        Here a timer perdiodically check the process and call the event accordingly.
        (STILL DOES NOT WORK)
    -   TAsyncProcess.OnReadData event is not usable to read full output lines.
        Here the output is accumulated in a TMemoryStream which allows to keep data
        at the left of an unterminated line when a buffer is available.

    The member Output is not usable anymore. Instead:

    -   getFullLines() can be used  in OnReadData or after the execution to fill
        a string list.
    -   OutputStack can be used to read the raw output. It allows to seek, which
        overcomes another limitation of the basic process classes.
  }
  TCEProcess = class(TASyncProcess)
  private
    fRealOnTerminate: TNotifyEvent;
    fRealOnReadData: TNotifyEvent;
    fOutputStack: TMemoryStream;
    fTerminateChecker: TTimer;
    fDoneTerminated: boolean;
    fHasRead: boolean;
    fRedirectStdErr: boolean;
    procedure checkTerminated(sender: TObject);
    procedure setOnTerminate(value: TNotifyEvent);
    procedure setOnReadData(value: TNotifyEvent);
  protected
    procedure internalDoOnReadData(sender: TObject); virtual;
    procedure internalDoOnTerminate(sender: TObject); virtual;
  published
    property OnTerminate write setOnTerminate;
    property OnReadData write setOnReadData;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    procedure execute; override;
    // Check if process is terminated (bug 33897).
    // Not to be called in the OnTerminated handler.
    procedure checkTerminated;
    // Add stderr to stdout, to be called when terminated
    procedure appendStdErr;
    // reads TProcess.OUtput in OutputStack
    procedure fillOutputStack;
    // fills list with the full lines contained in OutputStack
    procedure getFullLines(list: TStrings; consume: boolean = true);
    // access to a flexible copy of TProcess.Output
    property OutputStack: TMemoryStream read fOutputStack;
    // indicates if an output buffer is read
    property hasRead: boolean read fHasRead;
  end;

  {
    OnReadData is only called if no additional buffers are passed
    during a timeout.
  }
  TCEAutoBufferedProcess = class(TCEProcess)
  private
    fNewBufferChecker: TTimer;
    fNewBufferTimeOut: Integer;
    fPreviousSize: Integer;
    procedure newBufferCheckerChecks(sender: TObject);
    procedure setTimeout(value: integer);
  protected
    procedure internalDoOnReadData(sender: TObject); override;
    procedure internalDoOnTerminate(sender: TObject); override;
  public
    constructor create(aOwner: TComponent); override;
    procedure execute; override;
    property timeOut: integer read fNewBufferTimeOut write setTimeout;
  end;

  procedure killProcess(var proc: TCEProcess);

  function prettyReturnStatus(proc: TProcess): string;

implementation

procedure killProcess(var proc: TCEProcess);
begin
  if proc = nil then
    exit;
  if proc.Running then
    proc.Terminate(0);
  proc.Free;
  proc := nil;
end;

function prettyReturnStatus(proc: TProcess): string;
var
  s: integer;
  {$IFDEF UNIX}
  u: integer;
  {$ENDIF}
begin
  result := '';
  s := proc.ExitStatus;
  {$IFDEF UNIX}
  if s > 255 then
  begin
    u := s div 256;
    result := intToStr(u) + ' (Program-defined exit status)';
  end
  else if s > 127 then
  begin
    u := s - 128;
    if s > 128 then
    case u of
      0: result := '128 (Invalid argument to exit)';
      1: result := '1 (SIGHUP)';
      2: result := '2 (SIGINT)';
      3: result := '3 (SIGQUIT)';
      4: result := '4 (SIGILL)';
      5: result := '4 (SIGTRAP)';
      6: result := '6 (SIGABRT)';
      7: result := '7 (SIGEMT)';
      8: result := '8 (SIGFPE)';
      9: result := '9 (SIGKILL)';
      10: result := '10 (SIGBUS)';
      11: result := '11 (SIGSEGV)';
      12: result := '12 (SIGSYS)';
      13: result := '13 (SIGPIPE)';
      14: result := '14 (SIGALRM)';
      15: result := '15 (SIGTERM)';
      16: result := '16 (SIGUSR1)';
      17: result := '17 (SIGUSR2)';
      18: result := '18 (SIGCHLD)';
      19: result := '19 (SIGPWR)';
      20: result := '20 (SIGWINCH)';
      21: result := '21 (SIGURG)';
      22: result := '22 (SIGPOLL)';
      23: result := '23 (SIGSTOP)';
      24: result := '24 (SIGTSTP)';
      25: result := '25 (SIGCONT)';
      26: result := '26 (SIGTTIN)';
      27: result := '27 (SIGTTOU)';
      28: result := '28 (SIGVTALRM)';
      29: result := '29 (SIGPROF)';
      30: result := '30 (SIGXCPU)';
      31: result := '31 (SIGXFSZ)';
      32: result := '32 (SIGWAITING)';
      33: result := '33 (SIGLWP)';
      34: result := '34 (SIGAIO)';
    end;
  end;
  {$ENDIF}
  if result = '' then
    result := intToStr(s) + ' (undeterminated meaning)';
end;

constructor TCEProcess.create(aOwner: TComponent);
begin
  inherited;
  FOutputStack := TMemoryStream.Create;
  FTerminateChecker := TTimer.Create(nil);
  FTerminateChecker.Interval := 100;
  fTerminateChecker.OnTimer := @checkTerminated;
  fTerminateChecker.Enabled := false;
  TAsyncProcess(self).OnTerminate := @internalDoOnTerminate;
  TAsyncProcess(self).OnReadData := @internalDoOnReadData;
end;

destructor TCEProcess.destroy;
begin
  FTerminateChecker.Free;
  FOutputStack.Free;
  inherited;
end;

procedure TCEProcess.Execute;
begin
  fRedirectStdErr := poStderrToOutPut in Options;
  Options := Options - [poStderrToOutPut];
  fHasRead := false;
  fOutputStack.Clear;
  fDoneTerminated := false;
  TAsyncProcess(self).OnReadData := @internalDoOnReadData;
  TAsyncProcess(self).OnTerminate := @internalDoOnTerminate;
  fTerminateChecker.Enabled := true;
  inherited;
end;

procedure TCEProcess.fillOutputStack;
var
  sum, cnt: Integer;
begin
  if not (poUsePipes in Options) then
    exit;
  sum := fOutputStack.Size;
  while (Output <> nil) and (NumBytesAvailable > 0) do
  begin
    fOutputStack.SetSize(sum + NumBytesAvailable);
    cnt := Output.Read((fOutputStack.Memory + sum)^, NumBytesAvailable);
    sum += cnt;
  end;
  fOutputStack.SetSize(sum);
end;

procedure TCEProcess.appendStdErr;
var
  sum, cnt: Integer;
begin
  if not (poUsePipes in Options) then
    exit;
  sum := fOutputStack.Size;
  while (Stderr <> nil) and (Stderr.NumBytesAvailable > 0) do
  begin
    fOutputStack.SetSize(sum + Stderr.NumBytesAvailable);
    cnt := Stderr.Read((fOutputStack.Memory + sum)^, Stderr.NumBytesAvailable);
    sum += cnt;
  end;
  fOutputStack.SetSize(sum);
end;

procedure TCEProcess.getFullLines(list: TStrings; consume: boolean = true);
var
  stored: Integer;
  lastEOL: Integer;
  toread: Integer;
  buff: Byte = 0;
  str: TMemoryStream;
begin
  if not Running then
  begin
    list.LoadFromStream(fOutputStack);
    if consume then
      fOutputStack.Clear;
  end else
  begin
    lastEOL := fOutputStack.Position;
    stored := fOutputStack.Position;
    while fOutputStack.Read(buff, 1) = 1 do
      if buff = 10 then
        lastEOL := fOutputStack.Position;
    fOutputStack.Position := stored;
    if lastEOL <> stored then
    begin
      str := TMemoryStream.Create;
      try
        toread := lastEOL - stored;
        str.SetSize(toRead);
        fOutputStack.Read(str.Memory^, toread);
        list.LoadFromStream(str);
      finally
        str.Free;
      end;
    end;
  end;
end;

procedure TCEProcess.setOnTerminate(value: TNotifyEvent);
begin
  fRealOnTerminate := value;
  TAsyncProcess(self).OnTerminate := @internalDoOnTerminate;
end;

procedure TCEProcess.setOnReadData(value: TNotifyEvent);
begin
  fRealOnReadData := value;
  TAsyncProcess(self).OnReadData := @internalDoOnReadData;
end;

procedure TCEProcess.internalDoOnReadData(sender: TObject);
begin
  fHasRead := true;
  fillOutputStack;
  if fRealOnReadData <> nil then
    fRealOnReadData(self);
end;

procedure TCEProcess.internalDoOnTerminate(sender: TObject);
begin
  fHasRead := false;
  fTerminateChecker.Enabled := false;
  if fDoneTerminated then exit;
  fDoneTerminated := true;

  // note: made to fix a leak in the process used by the linter
  // onTerminate is sometimes determined by an internal timer
  // and not the base method of TAsyncProcess (which usually unhooks)
  UnhookPipeHandle;
  UnhookProcessHandle;

  fillOutputStack;
  // note: redirection is to output stream is done by hand at the end
  // because of sync issue (https://github.com/BBasile/Coedit/issues/336).
  if fRedirectStdErr then
    appendStdErr;

  if fRealOnTerminate <> nil then
    fRealOnTerminate(self);
end;

procedure TCEProcess.checkTerminated(sender: TObject);
begin
  if Running then
    exit;
  fTerminateChecker.Enabled := false;
  internalDoOnTerminate(self);
end;

procedure TCEProcess.checkTerminated;
begin
  sleep(20);
  checkTerminated(self);
end;

constructor TCEAutoBufferedProcess.create(aOwner: TComponent);
begin
  inherited;
  fNewBufferTimeOut := 1000;
  fNewBufferChecker := TTimer.Create(self);
  fNewBufferChecker.Enabled:= false;
  fNewBufferChecker.Interval:= fNewBufferTimeOut;
  fNewBufferChecker.OnTimer:= @newBufferCheckerChecks;
end;

procedure TCEAutoBufferedProcess.setTimeout(value: integer);
begin
  if fNewBufferTimeOut = value then
    exit;
  fNewBufferTimeOut := value;
  fNewBufferChecker.Interval:= fNewBufferTimeOut;
end;

procedure TCEAutoBufferedProcess.execute;
begin
  fPreviousSize := fOutputStack.Size;
  fNewBufferChecker.Enabled:=true;
  inherited;
end;

procedure TCEAutoBufferedProcess.newBufferCheckerChecks(sender: TObject);
begin
  if fOutputStack.Size = fPreviousSize then
  begin
    if assigned(fRealOnReadData) then
      fRealOnReadData(self);
  end;
  fPreviousSize := fOutputStack.Size;
end;

procedure TCEAutoBufferedProcess.internalDoOnReadData(sender: TObject);
begin
  fillOutputStack;
end;

procedure TCEAutoBufferedProcess.internalDoOnTerminate(sender: TObject);
begin
  fNewBufferChecker.Enabled:=false;
  inherited;
end;

end.

