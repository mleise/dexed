unit ce_libman;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, ce_common, ce_writableComponent, ce_dcd, LazFileUtils,
  ce_dialogs, ce_projutils, ce_interfaces, ce_dlang, ghashmap, ghashset;

type

  // TODO-clibman: improve import analysis, update (currently always full).

  (**
   * Information for a module in a library manager entry
   *)
  TModuleInfo = class(TCollectionItem)
  strict private
    fName: string;
    fImports: TStringList;
    procedure setImports(value: TStringList);
  published
    // the.module.name
    property name: string read fName write fname;
    // all the imports in this module
    property imports: TStringList read fImports write setImports;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  end;


  (**
   * Represents a D static library.
   *)
  TLibraryItem = class(TCollectionItem)
  type
    TModulesByName = specialize TStringHashMap<TModuleInfo>;
  strict private
    fAlias: string;
    fLibSourcePath: string;
    fLibFile: string;
    fLibProject: string;
    fEnabled: boolean;
    fDependencies: TStringList;
    fModules: TCollection;
    fModulesByName: TModulesByName;
    fHasValidSourcePath: boolean;
    fHasValidLibFile: boolean;
    fHasValidLibProject: boolean;
    procedure setDependencies(value: TStringList);
    procedure setModules(value: TCollection);
    procedure setLibProject(const value: string);
    procedure setLibFile(const value: string);
    procedure setLibSourcePath(const value: string);
    function getModule(value: integer): TModuleInfo;
    function getModule(const value: string): TModuleInfo;
    function moduleCount: integer;
  published
    property libAlias: string read fAlias write fAlias;
    property libSourcePath: string read fLibSourcePath write setLibSourcePath;
    property libFile: string read fLibFile write setLibFile;
    property libProject: string read fLibProject write setLibProject;
    property enabled: boolean read fEnabled write fEnabled default true;
    // TODO-clibman: dont forget that these props are not written
    property dependencies: TStringList read fDependencies write setDependencies stored false;
    property modules: TCollection read fModules write setModules stored false;
    // TODO-cmaintenace: remove this property from 3 update 1
    property projectFile: string read fLibProject write fLibProject stored false;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure updateModulesInfo;
    function addModuleInfo: TModuleInfo;
    function hasModule(const value: string): boolean;
    property hasValidLibFile: boolean read fHasValidLibFile;
    property hasValidLibProject: boolean read fHasValidLibProject;
    property hasValidLibSourcePath: boolean read fHasValidSourcePath;
    property moduleByIndex[value: integer]: TModuleInfo read getModule;
  end;

  TLibraryList = specialize TObjectHashSet<TLibraryItem>;

  (**
   * Represents all the D libraries handled by Coedit.
   *)
  TLibraryManager = class(TWritableLfmTextComponent)
  type
    TItemsByAlias = specialize TStringHashMap<TLibraryItem>;
  strict private
    fCollection: TCollection;
    fItemsByAlias: TItemsByAlias;
    function getLibraryByIndex(index: integer): TLibraryItem;
    function getLibraryByAlias(const value: string): TLibraryItem;
    function getLibraryByImport(const value: string): TLibraryItem;
    procedure setCollection(value: TCollection);
    procedure updateItemsByAlias;
    function getLibrariesCount: integer;
  published
    property libraries: TCollection read fCollection write setCollection;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    (**
     * The caller gets all the static library files in "list" if "aliases" is nil
     * otherwise the static library files selected by "aliases".
     *)
    procedure getLibFiles(aliases, list: TStrings);
    (**
     * The caller gets all the paths were are located the library sources in "list"
     * if "aliases" is nil otherwise the paths where are located the sources of the
     * libraries selected by "aliases".
     *)
    procedure getLibSourcePath(aliases, list: TStrings);
    (**
     * The caller gets static libraries files in "libs" and source paths
     * in "paths", as required by the specified "source" code.
     *)
    procedure getLibsForSource(source, libs, paths: TStrings);
    //
    procedure updateDCD;
    // find the aliases of the libraries used by this library.
    procedure updateCrossDependencies;
    property librariesCount: integer read getLibrariesCount;
    property libraryByIndex[value: integer]: TLibraryItem read getLibraryByIndex;
    property libraryByAlias[const value: string]: TLibraryItem read getLibraryByAlias;
    property libraryByImport[const value: string]: TLibraryItem read getLibraryByImport;
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
end;

destructor TModuleInfo.Destroy;
begin
  fImports.free;
  inherited;
end;

procedure TModuleInfo.setImports(value: TStringList);
begin
  fImports.Assign(value);
end;

constructor TLibraryItem.Create(ACollection: TCollection);
begin
  inherited create(ACollection);
  fModules := TCollection.Create(TModuleInfo);
  fModulesByName := TModulesByName.create;
  fDependencies := TStringList.Create;
  fEnabled:=true;
end;

destructor TLibraryItem.Destroy;
begin
  fModulesByName.Free;
  fDependencies.Free;
  fModules.Free;
  inherited;
end;

function TLibraryItem.moduleCount: integer;
begin
  exit(fModules.Count);
end;

function TLibraryItem.getModule(value: integer): TModuleInfo;
begin
  exit(TModuleInfo(fModules.Items[value]));
end;

function TLibraryItem.getModule(const value: string): TModuleInfo;
begin
  exit(fModulesByName.GetData(value));
end;

function TLibraryItem.addModuleInfo: TModuleInfo;
begin
  exit(TModuleInfo(fModules.Add));
end;

function TLibraryItem.hasModule(const value: string): boolean;
begin
  exit(fModulesByName.contains(value));
end;

procedure TLibraryItem.setModules(value: TCollection);
begin
  fModules.Assign(value);
end;

procedure TLibraryItem.setDependencies(value: TStringList);
begin
  fDependencies.Assign(value);
end;

procedure TLibraryItem.setLibProject(const value: string);
begin
  if fLibProject = value then
    exit;
  fLibProject:=value;
  fHasValidLibProject:=value.fileExists;
end;

procedure TLibraryItem.setLibFile(const value: string);
begin
  if fLibFile = value then
    exit;
  fLibFile:=value;
  fHasValidLibFile:=value.fileExists or value.dirExists;
end;

procedure TLibraryItem.setLibSourcePath(const value: string);
begin
  if fLibSourcePath = value then
    exit;
  fLibSourcePath:=value;
  fHasValidSourcePath:=value.dirExists;
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
  fModulesByName.Free;
  fModulesByName := TModulesByName.create;
  if hasValidLibProject then
  begin
    prj := loadProject(fLibProject, true);
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
        fModulesByName.insert(mdi.name, mdi);
        getImports(tks, mdi.imports);
        tks.Clear;
      end;
    finally
      tks.Free;
      str.Free;
      prj.getProject.Free;
    end;
  end else if hasValidLibSourcePath then
  begin
    lst := TStringList.Create;
    str := TStringList.Create;
    tks := TLexTokenList.Create;
    try
      listFiles(lst, fLibSourcePath, true);
      for i := 0 to lst.Count-1 do
      begin
        fle := lst[i];
        if not hasDlangSyntax(fle.extractFileExt) then
          continue;
        str.LoadFromFile(fle);
        lex(str.Text, tks, nil, [lxoNoComments]);
        mdi := addModuleInfo;
        mdi.name := getModuleName(tks);
        fModulesByName.insert(mdi.name, mdi);
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

constructor TLibraryManager.create(aOwner: TComponent);
var
  fName: string;
  {$IFDEF WINDOWS}
  fDmdPath: string;
  {$ENDIF}
  i: integer;
begin
  inherited;
  fItemsByAlias:= TItemsByAlias.create;
  fCollection := TCollection.Create(TLibraryItem);
  fname := getCoeditDocPath + libFname;
  if fname.fileExists then
    loadFromFile(fname);
  if fCollection.Count = 0 then
  begin
    {$IFDEF WINDOWS}
    fDmdPath := ExeSearch('dmd.exe');
    if fDmdPath.fileExists then
    begin
      // add phobos
      fname := fDmdPath.ExtractFileDir;
      fname := fname.ExtractFileDir;
      with TLibraryItem(fCollection.Add) do begin
        libAlias := 'phobos';
        libFile  := fname + '\lib\phobos.lib';
        libSourcePath := fname.ExtractFileDir + '\src\phobos';
      end;
      // add druntime (no lib - only for DCD)
      fname := fDmdPath.ExtractFileDir;
      fname := fname.ExtractFileDir;
      with TLibraryItem(fCollection.Add) do begin
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
      with TLibraryItem(fCollection.Add) do begin
        libAlias := 'phobos';
        libFile := '';
        libSourcePath := '/usr/include/dmd/phobos';
      end;
    end;
    // add druntime (no libraryByIndex - only for DCD)
    if '/usr/include/dmd/druntime/import'.dirExists then
    begin
      with TLibraryItem(fCollection.Add) do begin
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
  updateItemsByAlias;
  if (fCollection.Count = 0) and not (getCoeditDocPath + libFname).fileExists then
  begin
    dlgOkInfo(
      'Coedit failed to add "druntime" and "phobos" to the library manager.'
    + 'If they are not already specified in the DCD configuration then the '
    + 'completion will not work properly');
  end;
  updateDCD;
  //
  for i := 0 to fCollection.Count-1 do
  begin
    if (libraryByIndex[i].libAlias <> 'phobos') and (libraryByIndex[i].libAlias <> 'druntime') then
      libraryByIndex[i].updateModulesInfo;
  end;
  updateCrossDependencies;
end;

destructor TLibraryManager.destroy;
begin
  ForceDirectoriesUTF8(getCoeditDocPath);
  LibMan.saveToFile(getCoeditDocPath + libFname);
  fItemsByAlias.Free;
  fCollection.Free;
  inherited;
end;

procedure TLibraryManager.updateItemsByAlias;
var
  i: integer;
begin
  fItemsByAlias.Free;
  fItemsByAlias := TItemsByAlias.create;
  for i:= 0 to fCollection.Count-1 do
    fItemsByAlias.insert(libraryByIndex[i].libAlias, libraryByIndex[i]);
end;

procedure TLibraryManager.setCollection(value: TCollection);
begin
  fCollection.assign(value);
end;

function TLibraryManager.getLibraryByIndex(index: integer): TLibraryItem;
begin
  exit(TLibraryItem(fCollection.Items[index]));
end;

function TLibraryManager.getLibraryByAlias(const value: string): TLibraryItem;
begin
  exit(fItemsByAlias.GetData(value));
end;

function TLibraryManager.getLibraryByImport(const value: string): TLibraryItem;
var
  i: integer;
  s: TLibraryItem;
begin
  result := nil;
  for i := 0 to librariesCount-1 do
  begin
    s := libraryByIndex[i];
    if s.hasModule(value) then
      exit(s);
  end;
end;

function TLibraryManager.getLibrariesCount: integer;
begin
  exit(fCollection.Count);
end;

procedure TLibraryManager.updateDCD;
var
  itm: TLibraryItem;
  str: TStringList;
  i: Integer;
begin
  if not DcdWrapper.available then exit;
  // note: new libraryByIndex are directly handled but removed ones still in cache until server restarts.
  str := TStringList.Create;
  try
    for i := 0 to fCollection.Count-1 do
    begin
      itm := TLibraryItem(fCollection.Items[i]);
      if itm.enabled then
        str.Add(itm.libSourcePath);
    end;
    DcdWrapper.addImportFolders(str);
  finally
    str.Free;
  end;
end;

procedure TLibraryManager.getLibFiles(aliases, list: TStrings);
  procedure add(lib: TLibraryItem);
  var
    j: integer;
    dir: string;
    lst: TstringList;
  begin
    // as a trick a folder can be set as source, this allows to pass
    // multiple sources in an automated way.
    if lib.libFile.dirExists then
    begin
      lst := TStringList.Create;
      try
        dir := lib.libFile;
        if lib.libFile[dir.length] = DirectorySeparator then
          dir := dir[1..dir.length-1];
        listFiles(lst, dir);
        for j:= 0 to lst.Count-1 do
        begin
          if lst[j].extractFileExt = libExt then
            if list.IndexOf(lst[j]) = -1 then
              list.Add(lst[j]);
        end;
      finally
        lst.Free;
      end;
    end
    else if lib.hasValidLibFile then
      list.Add(lib.libFile);
  end;
var
  lib: TLibraryItem;
  i: Integer;
begin
  // no selector = all libs
  if aliases.isNil then
  begin
    for i:= 0 to librariesCount-1 do
    begin
      lib := libraryByIndex[i];
      if lib.enabled then
        add(lib);
    end;
  end
  // else get selected libs
  else for i := 0 to aliases.Count-1 do
  begin
    lib := libraryByAlias[aliases[i]];
    if lib.isNotNil and lib.enabled then
      add(lib);
  end;
end;

procedure TLibraryManager.getLibSourcePath(aliases, list: TStrings);
var
  lib: TLibraryItem;
  i: Integer;
begin
  // no selector = all libs
  if aliases.isNil then
  begin
    for i:= 0 to librariesCount-1 do
    begin
      lib := libraryByIndex[i];
      if lib.enabled and lib.hasValidLibSourcePath then
        list.Add('-I' + lib.libSourcePath);
    end;
  end
  // else get selected libs
  else
  begin
    for i := 0 to aliases.Count-1 do
    begin
      lib := libraryByAlias[aliases[i]];
      if lib.isNotNil and lib.enabled and lib.hasValidLibSourcePath then
        list.Add('-I' + lib.libSourcePath);
    end;
  end;
end;

procedure TLibraryManager.getLibsForSource(source, libs, paths: TStrings);
var
  tks: TLexTokenList;
  imp: TStringList;
  i,j: integer;
  itm: TLibraryItem;
  dep: TLibraryItem;
  sel: TLibraryList;
begin
  tks := TLexTokenList.Create;
  imp := TStringList.Create;
  sel := TLibraryList.Create;
  try
    lex(source.Text, tks, nil, [lxoNoComments]);
    getImports(tks, imp);
    for i:= 0 to imp.Count-1 do
    begin
      // get library for import I
      itm := libraryByImport[imp[i]];
      if itm.isNotNil then
      begin
        sel.insert(itm);
        // get libraries for import I dependencies
        for j:= 0 to itm.dependencies.Count-1 do
        begin
          dep := libraryByAlias[itm.dependencies[j]];
          if dep.isNotNil then
            sel.insert(dep);
        end;
      end;
    end;
    // add the library files and the import paths for the selection
    if not sel.IsEmpty then with sel.Iterator do
    begin
      while true do if data.hasValidLibFile then
      begin
        libs.Add(Data.libFile);
        paths.Add('-I' + Data.libSourcePath);
        if not Next then
          break;
      end;
      free;
    end;
  finally
    sel.Free;
    tks.Free;
    imp.Free;
  end;
end;

procedure TLibraryManager.updateCrossDependencies;
var
  i, j, m: integer;
  lib: TLibraryItem;
  dep: TLibraryItem;
  imp: string;
begin
  for i := 0 to fCollection.Count-1 do
  begin
    lib := libraryByIndex[i];
    if (lib.libAlias = 'phobos') or (lib.libAlias = 'druntime') then
      continue;
    lib.dependencies.Clear;
    for j := 0 to lib.modules.Count-1 do
      for m := 0 to lib.moduleByIndex[j].imports.Count-1 do
    begin
      imp := lib.moduleByIndex[j].imports[m];
      // module of the same package so...
      if lib.hasModule(imp) then
        continue;
      dep := libraryByImport[imp];
      // ... this should not happen
      if dep = lib then
        continue;
      // core or std are always handled by sc.ini
      if dep.isNil or (dep.libAlias = 'phobos') or (dep.libAlias = 'druntime') then
        continue;
      // add deps
      libraryByIndex[i].dependencies.Add(dep.libAlias);
    end;
  end;
end;

initialization
  registerClasses([TLibraryManager, TLibraryItem]);
  LibMan := TLibraryManager.create(nil);
finalization
  LibMan.Free;
end.
