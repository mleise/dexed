unit ce_libman;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, ce_common, ce_writableComponent, ce_dcd, LazFileUtils,
  ce_dialogs, ce_projutils, ce_interfaces, ce_dlang;

type

  (**
   * Information for a module in a library manager entry
   *)
  TModuleInfo = class(TCollectionItem)
  private
    fName: string;
    fImports: TStringList;
    fDeps: TstringList;
    procedure setImports(value: TStringList);
    procedure setDependencies(value: TStringList);
  published
    // the.module.name
    property name: string read fName write fname;
    // all the imports in this module
    property imports: TStringList read fImports write setImports;
    // the aliases to the libraries used by this module.
    property dependencies: TStringList read fDeps write setDependencies;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  end;


  (**
   * Represents a D static library. In a project libAlias allows to
   * resolve automatically the dependencies of a project.
   *)
  TLibraryItem = class(TCollectionItem)
  private
    fAlias: string;
    fSourcePath: string;
    fLibFile: string;
    fProjFile: string;
    fEnabled: boolean;
    fModules: TOwnedCollection;
    procedure setModules(value: TOwnedCollection);
    function getModuleInfo(ix: integer): TModuleInfo;
  published
    property libAlias: string read fAlias write fAlias;
    property libSourcePath: string read fSourcePath write fSourcePath;
    property libFile: string read fLibFile write fLibFile;
    property projectFile: string read fProjFile write fProjFile;
    property enabled: boolean read fEnabled write fEnabled default true;
    // TODO-creminder: dont forget that this prop is not written
    property modules: TOwnedCollection read fModules write setModules stored false;
  public
    constructor Create(ACollection: TCollection); override;
    procedure updateModulesInfo;
    procedure updateModulesDependencies;
    function addModuleInfo: TModuleInfo;
    function hasModule(const value: string): boolean;
  end;

  (**
   * Represents all the D libraries present on this system.
   *)
  TLibraryManager = class(TWritableLfmTextComponent)
  private
    fCol: TCollection;
    function getLibrary(index: integer): TLibraryItem;
    procedure setCol(value: TCollection);
  published
    property libraries: TCollection read fCol write setCol;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    //
    (**
     * the caller gets all the *.lib/*.a files in aList if someAliases is nil
     * otherwise the static libs selected by the aliases in someAliases.
     *)
    procedure getLibFiles(someAliases, aList: TStrings);
    (**
     * the caller gets all the paths were are located the lib sources in aList if someAliases is nil
     * otherwise the paths where are located the lib sources selected by the aliases in someAliases.
     *)
    procedure getLibSources(someAliases, aList: TStrings);
    (**
     * The caller gets all the static library files and their source root
     * that are required by the specified source code.
     *)
    procedure getLibsForSource(source, libs, paths: TStrings);
    //
    procedure updateDCD;
    property lib[index: integer]: TLibraryItem read getLibrary; default;
  end;

const
  libFname = 'libraryManager.txt';

var
  LibMan: TLibraryManager;

implementation

constructor TModuleInfo.Create(ACollection: TCollection);
begin
  inherited create(ACollection);
  fImports := TStringList.Create;
  fDeps := TstringList.Create;
end;

destructor TModuleInfo.Destroy;
begin
  fImports.free;
  fDeps.Free;
  inherited;
end;

procedure TModuleInfo.setImports(value: TStringList);
begin
  fImports.Assign(value);
end;

procedure TModuleInfo.setDependencies(value: TStringList);
begin
  fDeps.Assign(value);
end;

constructor TLibraryItem.Create(ACollection: TCollection);
begin
  inherited create(ACollection);
  fModules := TOwnedCollection.Create(self, TModuleInfo);
  fEnabled:=true;
end;

function TLibraryItem.getModuleInfo(ix: integer): TModuleInfo;
begin
  exit(TModuleInfo(fModules.Items[ix]));
end;

function TLibraryItem.addModuleInfo: TModuleInfo;
begin
  exit(TModuleInfo(fModules.Add));
end;

function TLibraryItem.hasModule(const value: string): boolean;
var
  i: integer;
begin
  for i:= 0 to fModules.Count-1 do
    if getModuleInfo(i).name = value then
      exit(true);
  exit(false);
end;

procedure TLibraryItem.setModules(value: TOwnedCollection);
begin
  fModules.Assign(value);
end;

procedure TLibraryItem.updateModulesInfo;
var
  prj: ICECommonProject;
  tks: TLexTokenList;
  str: TStringList;
  lst: TStringList;
  mdi: TModuleInfo;
  fle: string;
  i: integer;
begin
  fModules.Clear;
  if fProjFile.fileExists then
  begin
    prj := loadProject(fProjFile, true);
    tks := TLexTokenList.Create;
    str := TStringList.Create;
    try
      for i:= 0 to prj.sourcesCount-1 do
      begin
        fle := prj.sourceAbsolute(i);
        // note: in CE, object files are considered as source since they're taken in the cmdline file list
        if not hasDlangSyntax(fle.extractFileExt) then
          continue;
        str.LoadFromFile(fle);
        lex(str.Text, tks, nil, [lxoNoComments]);
        mdi := addModuleInfo;
        mdi.name := getModuleName(tks);
        getImports(tks, mdi.imports);
        tks.Clear;
      end;
    finally
      tks.Free;
      str.Free;
      prj.getProject.Free;
    end;
  end else if fSourcePath.dirExists then
  begin
    lst := TStringList.Create;
    str := TStringList.Create;
    tks := TLexTokenList.Create;
    try
      listFiles(lst, fSourcePath, true);
      for i := 0 to lst.Count-1 do
      begin
        fle := lst[i];
        if not hasDlangSyntax(fle.extractFileExt) then
          continue;
        str.LoadFromFile(fle);
        lex(str.Text, tks, nil, [lxoNoComments]);
        mdi := addModuleInfo;
        mdi.name := getModuleName(tks);
        getImports(tks, mdi.imports);
        tks.Clear;
      end;
    finally
      lst.Free;
      str.Free;
      tks.Free;
    end;
  end;
end;

procedure TLibraryItem.updateModulesDependencies;
begin

end;

constructor TLibraryManager.create(aOwner: TComponent);
var
  fName: string;
  {$IFDEF WINDOWS}
  fDmdPath: string;
  {$ENDIF}
  i: integer;
begin
  inherited;
  fCol := TCollection.Create(TLibraryItem);
  fname := getCoeditDocPath + libFname;
  if fname.fileExists then
    loadFromFile(fname);
  if fCol.Count = 0 then
  begin
    {$IFDEF WINDOWS}
    fDmdPath := ExeSearch('dmd.exe');
    if fDmdPath.fileExists then
    begin
      // add phobos
      fname := fDmdPath.ExtractFileDir;
      fname := fname.ExtractFileDir;
      with TLibraryItem(fCol.Add) do begin
        libAlias := 'phobos';
        libFile  := fname + '\lib\phobos.lib';
        libSourcePath := fname.ExtractFileDir + '\src\phobos';
      end;
      // add druntime (no lib - only for DCD)
      fname := fDmdPath.ExtractFileDir;
      fname := fname.ExtractFileDir;
      with TLibraryItem(fCol.Add) do begin
        libAlias := 'druntime';
        libFile  := '';
        libSourcePath := fname.ExtractFileDir + '\src\druntime\import';
      end;
    end;
    {$ENDIF}
    {$IFDEF LINUX}
    // add phobos
    if '/usr/include/dmd/phobos'.dirExists then
    begin
      with TLibraryItem(fCol.Add) do begin
        libAlias := 'phobos';
        libFile := '';
        libSourcePath := '/usr/include/dmd/phobos';
      end;
    end;
    // add druntime (no lib - only for DCD)
    if '/usr/include/dmd/druntime/import'.dirExists then
    begin
      with TLibraryItem(fCol.Add) do begin
        libAlias := 'druntime';
        libFile  := '';
        libSourcePath := '/usr/include/dmd/druntime/import';
      end;
    end;
    {$ENDIF}
    {$IFDEF DARWIN}
    if '/Library/D/dmd/src/phobos'.dirExists then
    begin
      with TLibraryItem(fCol.Add) do begin
        libAlias := 'phobos';
        libFile := '';
        libSourcePath := '/Library/D/dmd/src/phobos';
      end;
    end;
    // add druntime (no lib - only for DCD)
    if '/Library/D/dmd/src/druntime/import'.dirExists then
    begin
      with TLibraryItem(fCol.Add) do begin
        libAlias := 'druntime';
        libFile  := '';
        libSourcePath := '/Library/D/dmd/src/druntime/import';
      end;
    end;
    {$ENDIF}
  end;
  if (fCol.Count = 0) and not (getCoeditDocPath + libFname).fileExists then
  begin
    dlgOkInfo(
      'Coedit failed to add "druntime" and "phobos" to the library manager.'
    + 'If they are not already specified in the DCD configuration then the '
    + 'completion will not work properly');
  end;
  updateDCD;
  //
  for i := 2 to fCol.Count-1 do
  begin
    TLibraryItem(fCol.Items[i]).updateModulesInfo;
  end;
end;

destructor TLibraryManager.destroy;
begin
  ForceDirectoriesUTF8(getCoeditDocPath);
  LibMan.saveToFile(getCoeditDocPath + libFname);
  fCol.Free;
  inherited;
end;

procedure TLibraryManager.setCol(value: TCollection);
begin
  fCol.assign(value);
end;

function TLibraryManager.getLibrary(index: integer): TLibraryItem;
begin
  exit(TLibraryItem(fCol.Items[index]));
end;

procedure TLibraryManager.updateDCD;
var
  itm: TLibraryItem;
  str: TStringList;
  i: Integer;
begin
  if not DcdWrapper.available then exit;
  // note: new items are directly handled but removed ones still in cache until server restarts.
  str := TStringList.Create;
  try
    for i := 0 to fCol.Count-1 do
    begin
      itm := TLibraryItem(fCol.Items[i]);
      if itm.enabled then
        str.Add(itm.libSourcePath);
    end;
    DcdWrapper.addImportFolders(str);
  finally
    str.Free;
  end;
end;

procedure TLibraryManager.getLibFiles(someAliases, aList: TStrings);
var
  itm: TLibraryItem;
  lst: TStringList;
  i,j: Integer;
  dir: string;
begin
  for i := 0 to fCol.Count-1 do
  begin
    itm := TLibraryItem(fCol.Items[i]);
    if (not itm.enabled) or
      (someAliases.isNotNil and (someAliases.IndexOf(itm.libAlias) = -1)) then
        continue;
    // single lib files
    if fileExists(itm.libFile) then
    begin
      if aList.IndexOf(itm.libFile) <> -1 then
        continue;
      aList.Add(itm.libFile);
    end
    // folder of lib file
    else if itm.libFile.dirExists then
    begin
      lst := TStringList.Create;
      try
        dir := itm.libFile;
        if itm.libFile[dir.length] = DirectorySeparator then
          dir := dir[1..dir.length-1];
        listFiles(lst, dir);
        for j:= 0 to lst.Count-1 do
        begin
          if lst[j].extractFileExt = libExt then
            if aList.IndexOf(lst[j]) = -1 then
              aList.Add(lst[j]);
        end;
      finally
        lst.Free;
      end;
    end;
  end;
end;


procedure TLibraryManager.getLibSources(someAliases, aList: TStrings);
var
  itm: TLibraryItem;
  i: Integer;
begin
  for i := 0 to fCol.Count-1 do
  begin
    itm := TLibraryItem(fCol.Items[i]);
    if (not itm.enabled) or
      (someAliases.isNotNil and (someAliases.IndexOf(itm.libAlias) = -1)) then
        continue;
    //
    if aList.IndexOf(itm.libSourcePath) <> -1 then
      continue;
    if not itm.libSourcePath.dirExists then
      continue;
    aList.Add('-I' + itm.libSourcePath);
  end;
end;

procedure TLibraryManager.getLibsForSource(source, libs, paths: TStrings);
var
  tks: TLexTokenList;
  imp: TStringList;
  i,j: integer;
  itm: TLibraryItem;
begin
  tks := TLexTokenList.Create;
  imp := TStringList.Create;
  try
    lex(source.Text, tks, nil, [lxoNoComments]);
    getImports(tks, imp);
    for i := 0 to fCol.Count-1 do
    begin
      itm := lib[i];
      for j := imp.Count-1 downto 0 do if itm.hasModule(imp[j]) then
      begin
        imp.Delete(j);
        if (libs.IndexOf(itm.libFile) <> -1) then
          continue;
        libs.Add(itm.libFile);
        paths.Add('-I'+itm.libSourcePath);
      end;
    end;
  finally
    tks.Free;
    imp.Free;
  end;
end;

initialization
  registerClasses([TLibraryManager, TLibraryItem]);
  LibMan := TLibraryManager.create(nil);
finalization
  LibMan.Free;
end.
