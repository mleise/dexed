unit ce_dbgitf;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, ce_observer;

type

  TBreakPointKind = (
    bpkNone,  // nothing
    bpkBreak, // break point
    bpkWatch  // watch point
  );

  (**
   * ICEEDebugObserver can call any of the method during debugging
   *)
  ICEDebugger = interface(ICESingleService)
    function running: boolean;
    procedure addBreakPoint(const fname: string; line: integer; kind: TBreakPointKind = bpkBreak);
    procedure removeBreakPoint(const fname: string; line: integer; kind: TBreakPointKind = bpkBreak);
    procedure removeBreakPoints(const fname: string);
  end;

  // Enumerates th e reason why debuging breaks.
  TCEDebugBreakReason = (
    dbUnknown,      // ?
    dbBreakPoint,   // a break point is reached.
    dbSignal,       // an unexpected signal is emitted.
    dbStep,         // step to this line
    dbWatch         // watchpoint reached
  );
  (**
   * An implementer is informed about a debuging session.
   *)
  ICEDebugObserver = interface(IObserverType)
  ['ICEDebugObserver']
    // a debugging session starts. The ICEDebugger can be stored for the session.
    procedure debugStart(debugger: ICEDebugger);
    // a debugging session terminates. Any pointer to a ICEDebugger becomes invalid.
    procedure debugStop;
    // a break happens when code in fname at line is executed.
    procedure debugBreak(const fname: string; line: integer; reason: TCEDebugBreakReason);
    // debugging continue
    procedure debugContinue;
  end;

  (**
   * An implementer notifies is observer about a debuginf session.
   *)
  TCEDebugObserverSubject = specialize TCECustomSubject<ICEDebugObserver>;

  // TCEDebugObserverSubject primitives
  procedure subjDebugStart(subj: TCEDebugObserverSubject; dbg: ICEDebugger);
  procedure subjDebugStop(subj: TCEDebugObserverSubject);
  procedure subjDebugContinue(subj: TCEDebugObserverSubject);
  procedure subjDebugBreak(subj: TCEDebugObserverSubject; const fname: string;
    line: integer; reason: TCEDebugBreakReason);


implementation

procedure subjDebugStart(subj: TCEDebugObserverSubject; dbg: ICEDebugger);
var
  i: integer;
begin
  for i:= 0 to subj.observersCount-1 do
    (subj.observers[i] as ICEDebugObserver).debugStart(dbg);
end;

procedure subjDebugStop(subj: TCEDebugObserverSubject);
var
  i: integer;
begin
  for i:= 0 to subj.observersCount-1 do
    (subj.observers[i] as ICEDebugObserver).debugStop;
end;

procedure subjDebugBreak(subj: TCEDebugObserverSubject; const fname: string;
    line: integer; reason: TCEDebugBreakReason);
var
  i: integer;
begin
  for i:= 0 to subj.observersCount-1 do
    (subj.observers[i] as ICEDebugObserver).debugBreak(fname, line, reason);
end;

procedure subjDebugContinue(subj: TCEDebugObserverSubject);
var
  i: integer;
begin
  for i:= 0 to subj.observersCount-1 do
    (subj.observers[i] as ICEDebugObserver).debugContinue;
end;

end.

