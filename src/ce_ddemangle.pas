unit ce_ddemangle;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, process, forms,
  ce_processes, ce_common;

type

  TCEDDemangler = class
  strict private
    fActive: boolean;
    fProc: TCEProcess;
    fList, fOut: TStringList;
    procedure procTerminate(sender: TObject);
  public
    constructor create;
    destructor destroy; override;
    procedure demangle(const value: string);
    property output: TStringList read fList;
    property active: boolean read fActive;
  end;

// demangle a D name
function demangle(const value: string): string;
// demangle a list of D names
procedure demangle(values, output: TStrings);

implementation

var
  demangler: TCEDDemangler;

constructor TCEDDemangler.create;
begin
  fList := TStringList.Create;
  fOut  := TStringList.Create;
  fProc := TCEProcess.create(nil);
  fProc.Executable:= exeFullName('ddemangle' + exeExt);
  fProc.Options:= [poUsePipes];
  fProc.OnTerminate:=@procTerminate;
  fProc.ShowWindow:= swoHIDE;
  fProc.execute;
  fActive := true;
end;

destructor TCEDDemangler.destroy;
begin
  fProc.Terminate(0);
  fProc.Free;
  fOut.Free;
  fList.Free;
  inherited;
end;

procedure TCEDDemangler.procTerminate(sender: TObject);
begin
  fActive := false;
end;

procedure TCEDDemangler.demangle(const value: string);
var
  nb: integer;
begin
  if value.isNotEmpty then
    fProc.Input.Write(value[1], value.length);
  fProc.Input.WriteByte(10);
  while true do
  begin
    nb := fProc.NumBytesAvailable;
    if nb <> 0 then
      break;
  end;
  fProc.fillOutputStack;
  fProc.getFullLines(fOut);
  if fOut.Count <> 0 then
    fList.Add(fOut[0]);
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
