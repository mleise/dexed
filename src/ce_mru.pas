unit ce_mru;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils,
  ce_interfaces, ce_observer, ce_synmemo, ce_common;

type

  (**
   * 'Most Recently Used' list for strings.
   *)
  TCEMruList = class(TStringList)
  private
    fMaxCount: Integer;
    fObj: TObject;
  protected
    fChecking: boolean;
    procedure clearOutOfRange;
    procedure setMaxCount(value: Integer);
    function checkItem(const value: string): boolean; virtual;
    procedure Put(index: Integer; const value: string); override;
    procedure InsertItem(index: Integer; const value: string); override;
  published
    property maxCount: Integer read fMaxCount write setMaxCount;
  public
    constructor create; virtual;
    procedure Insert(index: Integer; const value: string); override;
    property objectTag: TObject read fObj write fObj;
  end;

  (**
   * MRU list for filenames.
   *)
  TCEMRUFileList = class(TCEMruList)
  protected
    function checkItem(const value: string): boolean; override;
  public
    constructor create; override;
    procedure assign(source: TPersistent); override;
  end;

  (**
   * MRU list for D/text files.
   * Insertion is automatic (ICEDocumentObserver).
   *)
  TCEMRUDocumentList = class(TCEMRUFileList, ICEDocumentObserver)
  private
    procedure docNew(document: TCESynMemo);
    procedure docFocused(document: TCESynMemo);
    procedure docChanged(document: TCESynMemo);
    procedure docClosing(document: TCESynMemo);
  public
    constructor create; override;
    destructor destroy; override;
  end;

  (**
   * MRU list for the ceodit projects.
   * Insertion is automatic (ICEProjectObserver).
   *)
  TCEMRUProjectList = class(TCEMRUFileList, ICEProjectObserver)
  private
    procedure projNew(project: ICECommonProject);
    procedure projChanged(project: ICECommonProject);
    procedure projClosing(project: ICECommonProject);
    procedure projFocused(project: ICECommonProject);
    procedure projCompiling(project: ICECommonProject);
    procedure projCompiled(project: ICECommonProject; success: boolean);
  public
    constructor create; override;
    destructor destroy; override;
  end;

  (**
   * MRU list for the ceodit projects group.
   * Managed manually since only 1 group exists.
   *)
  TCEMRUProjectsGroupList = class(TCEMRUFileList)
  end;

implementation

constructor TCEMruList.Create;
begin
  fMaxCount := 10;
end;

procedure TCEMruList.clearOutOfRange;
begin
  while Count > fMaxCount do
    delete(Count-1);
end;

procedure TCEMruList.setMaxCount(value: Integer);
begin
  if value < 0 then
    value := 0;
  if fMaxCount = value then
    exit;
  fMaxCount := value;
  clearOutOfRange;
end;

function TCEMruList.checkItem(const value: string): boolean;
var
  i: integer;
begin
  i := indexOf(value);
  if i = -1 then
    exit(true);
  if i = 0 then
    exit(false);
  if Count < 2 then
    exit(false);
  exchange(i, i-1);
  exit( false);
end;

procedure TCEMruList.Put(index: Integer; const value: string);
begin
  if not (checkItem(value)) then
    exit;
  inherited;
  clearOutOfRange;
end;

procedure TCEMruList.InsertItem(index: Integer; const value: string);
begin
  if not (checkItem(value)) then
    exit;
  inherited;
  clearOutOfRange;
end;

procedure TCEMruList.Insert(index: Integer; const value: string);
begin
  if not (checkItem(value)) then
    exit;
  inherited;
  clearOutOfRange;
end;

constructor TCEMRUFileList.create;
begin
  inherited;
  {$IFDEF WINDOWS}
  CaseSensitive := true;
  {$ENDIF}
end;

procedure TCEMRUFileList.assign(source: TPersistent);
var
  i: Integer;
begin
  inherited;
  for i := Count-1 downto 0 do
    if not Strings[i].fileExists then
      Delete(i);
end;

function TCEMRUFileList.checkItem(const value: string): boolean;
begin
  exit( inherited checkItem(value) and value.fileExists);
end;

constructor TCEMRUDocumentList.create;
begin
  inherited;
  EntitiesConnector.addObserver(self);
end;

destructor TCEMRUDocumentList.destroy;
begin
  EntitiesConnector.removeObserver(self);
  inherited;
end;

procedure TCEMRUDocumentList.docNew(document: TCESynMemo);
begin
end;

procedure TCEMRUDocumentList.docFocused(document: TCESynMemo);
begin
end;

procedure TCEMRUDocumentList.docChanged(document: TCESynMemo);
begin
end;

procedure TCEMRUDocumentList.docClosing(document: TCESynMemo);
begin
  if document.fileName.fileExists and not document.isTemporary then
    Insert(0, document.fileName);
end;

constructor TCEMRUProjectList.create;
begin
  inherited;
  EntitiesConnector.addObserver(self);
end;

destructor TCEMRUProjectList.destroy;
begin
  EntitiesConnector.removeObserver(self);
  inherited;
end;

procedure TCEMRUProjectList.projNew(project: ICECommonProject);
begin
end;

procedure TCEMRUProjectList.projFocused(project: ICECommonProject);
begin
end;

procedure TCEMRUProjectList.projChanged(project: ICECommonProject);
begin
end;

procedure TCEMRUProjectList.projCompiling(project: ICECommonProject);
begin
end;

procedure TCEMRUProjectList.projCompiled(project: ICECommonProject; success: boolean);
begin
end;

procedure TCEMRUProjectList.projClosing(project: ICECommonProject);
var
  fname: string;
begin
  if project = nil then
    exit;

  fname := project.filename;
  if fname.fileExists then
    Insert(0, fname);
end;

initialization
  RegisterClasses([TCEMRUList, TCEMRUFileList, TCEMRUProjectList, TCEMRUDocumentList]);
end.
