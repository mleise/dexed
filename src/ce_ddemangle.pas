unit ce_ddemangle;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, process, forms,
  ce_processes, ce_common;

type

  TCEDDemangler = class
  private
    fActive: boolean;
    fDone: boolean;
    fProc: TCEProcess;
    fList, fOut: TStringList;
    procedure procOutput(sender: TObject);
    procedure init;
  public
    constructor create;
    destructor destroy; override;
    procedure demangle(const value: string);
    property output: TStringList read fList;
    property active: boolean read fActive;
  end;

// TODO-cWindows: test the demangler under Windows

// demangle a D name
function demangle(const value: string): string;
// demangle a list of D names
procedure demangle(values, output: TStrings);

implementation

var
  demangler: TCEDDemangler;

constructor TCEDDemangler.create;
begin
  init;
  fList := TStringList.Create;
  fOut  := TStringList.Create;
end;

destructor TCEDDemangler.destroy;
begin
  fProc.Terminate(0);
  fProc.Free;
  fOut.Free;
  fList.Free;
  inherited;
end;

procedure TCEDDemangler.init;
begin
  if assigned(fProc) and fProc.Running then
    exit;
  fProc.free;
  fProc := TCEProcess.create(nil);
  fProc.Executable:= exeFullName('ddemangle' + exeExt);
  fProc.Options:= [poUsePipes];
  fProc.OnReadData:=@procOutput;
  fProc.ShowWindow:= swoHIDE;
  fProc.execute;
  fActive := true;
end;

procedure TCEDDemangler.demangle(const value: string);
var
  i: integer = 0;
begin
  init;
  fDone := false;
  fProc.Input.Write(value[1], value.length);
  fProc.Input.WriteByte(10);
  while not fDone do
  begin
    Application.ProcessMessages;
    i += 1;
    if i = high(integer) then
      i := 0;
  end;
end;

procedure TCEDDemangler.procOutput(sender: TObject);
begin
  fProc.getFullLines(fOut);
  if fOut.Count <> 0 then
    fList.Add(fOut[0]);
  fDone := true;
end;

function demangle(const value: string): string;
begin
  if demangler.active then
  begin
    demangler.output.Clear;
    demangler.demangle(value);
    if demangler.output.Count <> 0 then
      result := demangler.output[0]
    else
      result := value;
  end
  else result := value;
end;

procedure demangle(values, output: TStrings);
var
  value: string;
begin
  if demangler.active then
  begin
    for value in values do
      demangler.demangle(value);
    output.AddStrings(demangler.output);
    demangler.output.Clear;
  end
  else output.AddStrings(values);
end;

initialization
  demangler := TCEDDemangler.create;
finalization
  demangler.Free;
end.
