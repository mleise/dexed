unit ce_projutils;
{$I ce_defines.inc}

interface

uses
  Classes, SysUtils,
  ce_nativeproject, ce_dubproject, ce_interfaces, ce_common, ce_observer, ce_synmemo;

type
  TCEProjectFileFormat = (pffNone, pffCe, pffDub);

(**
 * Loads either a DUB or a CE project. If the filename is invalid or if it
 * doesn't points to a valid project, nil is returned, otherwise a project.
 * When 'discret' is set to true, the ICEPorjectObserver are not notified
 * that a new project is created and focused
 *)
function loadProject(const filename: string; discret: boolean): ICECommonProject;

(**
 * Indicates wether a file is a project, either CE or DUB.
 *)
function isProject(const filename: string): boolean;

(**
 * Indicates wether a file is a project, either CE or DUB.
 *)
function projectFormat(const filename: string): TCEProjectFileFormat;

(**
 * Saves all the project files that are being edited and if they're modified.
 *)
procedure saveModifiedProjectFiles(project: ICECommonProject);

implementation

function isProject(const filename: string): boolean;
begin
  if filename.extractFileExt = '.json' then
    result := isValidDubProject(filename)
  else
    result := isValidNativeProject(filename);
end;

function projectFormat(const filename: string): TCEProjectFileFormat;
begin
  result := pffNone;
  if filename.extractFileExt = '.json' then
  begin
    if isValidDubProject(filename) then
      result := pffDub;
  end
  else
    if isValidNativeProject(filename) then result := pffCe;
end;

function loadProject(const filename: string; discret: boolean): ICECommonProject;
var
  isDubProject: boolean = false;
  isCeProject: boolean = false;
  dubProj: TCEDubProject;
  ceProj: TCENativeProject;
begin
  result := nil;
  if not filename.fileExists then
    exit;

  EntitiesConnector.beginUpdate;
  if isValidDubProject(filename) then
    isDubProject := true
  else if isValidNativeProject(filename) then
    isCeProject := true;
  EntitiesConnector.endUpdate;
  if not isDubProject and not isCeProject then
    exit;

  if discret then
    EntitiesConnector.beginUpdate;
  if isDubProject then
  begin
    dubProj := TCEDubProject.create(nil);
    dubproj.loadFromFile(filename);
    result := dubProj as ICECommonProject;
  end
  else begin
    ceProj := TCENativeProject.create(nil);
    ceProj.loadFromFile(filename);
    result := ceProj as ICECommonProject;
  end;
  if discret then
    EntitiesConnector.endUpdate;
end;

procedure saveModifiedProjectFiles(project: ICECommonProject);
var
  mdh: ICEMultiDocHandler;
  doc: TCESynMemo;
  i: integer;
begin
  mdh := getMultiDocHandler;
  for i:= 0 to project.sourcesCount-1 do
  begin
    doc := mdh.findDocument(project.sourceAbsolute(i));
    if doc.isNotNil and doc.modified then
      doc.save;
  end;
end;

end.

