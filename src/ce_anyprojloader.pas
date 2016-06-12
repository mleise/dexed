unit ce_anyprojloader;
{$I ce_defines.inc}

interface

uses
  Classes, SysUtils,
  ce_nativeproject, ce_dubproject, ce_interfaces, ce_common, ce_observer;

(**
 * Loads either a DUB or a CE project. If the filename is invalid or if it
 * doesn't points to a valid project, nil is returned, otherwise a project.
 * When 'discret' is set to true, the ICEPorjectObserver are not notified
 * that a new project is created and focused
 *)
function loadProject(const filename: string; discret: boolean): ICECommonProject;

implementation

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

end.

