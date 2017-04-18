unit ce_projutils;
{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, fpjson, LazFileUtils,
  ce_interfaces, ce_common, ce_observer, ce_synmemo,
  ce_dlang, ce_stringrange;

type
  TCEProjectFileFormat = (pffNone, pffCe, pffDub);

  TLexNameCallback = class
  private
    fModStart: boolean;
  public
    procedure lexFindToken(const token: PLexToken; out stop: boolean);
  end;

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

(**
 * Returns the root of the project sources.
 *)
function projectSourcePath(project: ICECommonProject): string;

implementation

uses
  ce_ceproject, ce_dubproject;

var
  clbck: TLexNameCallback;

function isProject(const filename: string): boolean;
var
  ext: string;
begin
  ext := filename.extractFileExt.upperCase;
  if (ext = '.JSON') or (ext = '.SDL') then
    result := isValidDubProject(filename)
  else
    result := isValidNativeProject(filename);
end;

function projectFormat(const filename: string): TCEProjectFileFormat;
var
  ext: string;
begin
  result := pffNone;
  ext := filename.extractFileExt.upperCase;
  if (ext = '.JSON') or (ext = '.SDL') then
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

procedure TLexNameCallback.lexFindToken(const token: PLexToken; out stop: boolean);
begin
  if (token^.kind = ltkKeyword) and (token^.data = 'module') then
  begin
    fModStart := true;
    exit;
  end;
  if fModStart and (token^.kind = ltkSymbol) and (token^.data = ';') then
  begin
    stop := true;
    fModStart := false;
  end;
end;

function projectSourcePath(project: ICECommonProject): string;
var
  i, j: integer;
  mnme: string;
  path: string;
  base: string;
  fldn: array of string;
  lst: TStringList;
  srcc: TStringList;
  toks: TLexTokenList;
  pdb: TCEDubProject;
  jsn: TJSONArray;
  rng: TStringRange = (ptr: nil; pos: 0; len: 0);
  sym: boolean;
begin

  result := '';

  if clbck.isNil then
    clbck := TLexNameCallback.Create;

  // 1 source, same folder
  if project.sourcesCount = 1 then
  begin
    base := project.basePath;
    path := project.sourceAbsolute(0);
    if path.extractFilePath = base then
      exit(base);
  end;

  // DUB sourcePath
  if project.getFormat = pfDUB then
  begin
    pdb := TCEDubProject(project.getProject);
    if pdb.json.findArray('sourcePath', jsn) then
    begin
      if (jsn.Count = 1) then
        exit(project.filename.extractFilePath + jsn.Strings[0]);
    end
    else
    begin
      result := project.filename.extractFilePath + 'src';
      if result.dirExists then
        exit;
      result := project.filename.extractFilePath + 'source';
      if result.dirExists then
        exit;
    end;
  end;

  lst := TStringList.Create;
  srcc := TStringList.Create;
  toks := TLexTokenList.Create;
  try
    // get module name and store the parent.parent.parent... dir
    for i := 0 to project.sourcesCount-1 do
    begin
      sym := true;
      path := project.sourceAbsolute(i);
      if not hasDlangSyntax(path.extractFileExt) or not path.fileExists then
        continue;
      clbck.fModStart := false;
      srcc.LoadFromFile(path);
      lex(srcc.Text, toks, @clbck.lexFindToken, [lxoNoComments]);
      mnme := getModuleName(toks);
      toks.Clear;
      if mnme.isEmpty then
        continue;
      if path.extractFileName.stripFileExt = 'package' then
        mnme := mnme + '.p';
      setLength(fldn, 0);
      rng.init(mnme);
      while true do
      begin
        setLength(fldn, length(fldn) + 1);
        fldn[high(fldn)] := rng.takeUntil(['.', #0]).yield;
        if rng.empty then
          break
        else
          rng.popFront;
      end;
      for j:= high(fldn)-1 downto 0 do
      begin
        path := path.extractFileDir;
        if path.extractFileName <> fldn[j] then
        begin
          sym := false;
          break;
        end
      end;
      if sym then
      begin
        path := path.extractFileDir;
        lst.Add(path);
      end;
    end;
    deleteDups(lst);
    if project.sourcesCount = 0 then
      result := ''
    else
    begin
      result := lst[0];
      if FilenameIsAbsolute(result) then
        result := expandFilenameEx(GetCurrentDir, result);
    end;
    // Use common directory
    if ((project.sourcesCount > 1) and (lst.Count > 1))
      or (not sym) then
    begin
      lst.Clear;
      for j := 0 to project.sourcesCount-1 do
      begin
        path := project.sourceAbsolute(j);
        if hasDlangSyntax(path.extractFileExt) then
          lst.Add(path);
      end;
      result := commonFolder(lst);
      result := result.extractFileDir;
    end;
  finally
    srcc.Free;
    lst.Free;
    toks.Free;
  end;
end;


finalization
  clbck.Free;
end.

