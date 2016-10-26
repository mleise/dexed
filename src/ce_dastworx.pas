unit ce_dastworx;
{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, process, ce_common;

(**
 * Gets the module name and the imports of the source code located in
 * "source". The first line of "import" contains the module name, double quoted.
 * Each following line contain an import.
 *)
procedure getModuleImports(source, imports: TStrings);

(**
 * Gets the module names and the imports of the sources in "files".
 * source. Each line in "import" that contains double quoted text indicates
 * that a new group of import starts.
 *)
procedure getModulesImports(const files: string; results: TStrings);

implementation

var
  toolname: string;

function getToolName: string;
begin
  if toolname = '' then
    toolname := exeFullName('dastworx' + exeExt);
  exit(toolname);
end;

procedure getModuleImports(source, imports: TStrings);
var
  str: string;
  prc: TProcess;
begin
  str := getToolName;
  if str.isEmpty then
    exit;
  prc := TProcess.Create(nil);
  try
    prc.Executable := str;
    prc.Parameters.Add('-i');
    prc.Options := [poUsePipes{$IFDEF WINDOWS}, poNewConsole{$ENDIF}];
    prc.ShowWindow := swoHIDE;
    prc.Execute;
    str := source.Text;
    prc.Input.Write(str[1], str.length);
    prc.CloseInput;
    processOutputToStrings(prc, imports);
    // TODO-cmaintenance: remove this from version 3 gold
    tryRaiseFromStdErr(prc);
    while prc.Running do ;
  finally
    prc.free;
  end;
end;

procedure getModulesImports(const files: string; results: TStrings);
var
  str: string;
  prc: TProcess;
begin
  str := getToolName;
  if str.isEmpty then
    exit;
  prc := TProcess.Create(nil);
  try
    prc.Executable := str;
    prc.Parameters.Add(files);
    prc.Parameters.Add('-i');
    prc.Options := [poUsePipes {$IFDEF WINDOWS}, poNewConsole{$ENDIF}];
    prc.ShowWindow := swoHIDE;
    prc.Execute;
    prc.CloseInput;
    processOutputToStrings(prc, results);
    // TODO-cmaintenance: remove this from version 3 gold
    tryRaiseFromStdErr(prc);
    while prc.Running do ;
  finally
    prc.free;
  end;
end;

end.

