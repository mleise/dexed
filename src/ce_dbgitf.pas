unit ce_dbgitf;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, ce_observer;

type

  TBreakPointKind = (
    bpkBreak, // break point
    bpkWatch  // watch point
  );

  (**
   * ICEEDebugObserver can call any of the method during debugging
   *)
  ICEDebugger = interface(ICESingleService)
    function running: boolean;
    procedure addBreakPoint(const fname: string; line: integer; kind: TBreakPointKind);
    procedure removeBreakPoint(const fname: string; line: integer);
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
    // the debuger wants to know how many times debugQueryBreakPoints must be called.
    function debugQueryBpCount: integer;
    // the debuger wants breakpoints.
    procedure debugQueryBreakPoint(const index: integer; out fname: string;
      out line: integer; out kind: TBreakPointKind);
    // a break happens when code in fname at line is executed.
    procedure debugBreak(const fname: string; line: integer; reason: TCEDebugBreakReason);
  end;

  (**
   * An implementer notifies is observer about a debuginf session.
   *)
  TCEDebugObserverSubject = specialize TCECustomSubject<ICEDebugObserver>;

  // TCEDebugObserverSubject primitives
  procedure subjDebugStart(subj: TCEDebugObserverSubject; dbg: ICEDebugger);
  procedure subjDebugStop(subj: TCEDebugObserverSubject);
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

end.

