unit ce_observer;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, Contnrs, ce_common;

type

  (**
   * interface for a single Coedit service (many to one relation).
   * A service is valid during the whole application life-time and
   * is mostly designed to avoid messy uses clauses or to limit
   * the visibility of the implementer methods.
   *)
  ICESingleService = interface
    function singleServiceName: string;
  end;

  TServiceList = class(specialize TStringHashMap<TObject>);

  (**
   * Manages the connections between the observers and their subjects in the
   * whole program.
   *)
  TCEEntitiesConnector = class
  private
    fObservers: TObjectList;
    fSubjects: TObjectList;
    fServices: TServiceList;
    fUpdatesCount: Integer;
    procedure tryUpdate;
    procedure updateEntities;
    function getIsUpdating: boolean;
  public
    constructor Create;
    destructor Destroy; override;
    // forces the update, fixes begin/add pair error or if immediate update is needed.
    procedure forceUpdate;
    // entities will be added in bulk, must be followed by an enUpdate().
    procedure beginUpdate;
    // entities has ben added in bulk
    procedure endUpdate;
    // add/remove entities, update is automatic
    procedure addObserver(observer: TObject);
    procedure addSubject(subject: TObject);
    procedure removeObserver(observer: TObject);
    procedure removeSubject(subject: TObject);
    // allow to register a single service provider.
    procedure addSingleService(provider: TObject);
    // allow to retrieve a single service provider based on its interface name
    function getSingleService(const serviceName: string): TObject;
    // should be tested before forceUpdate()
    property isUpdating: boolean read getIsUpdating;
  end;

  (**
   * Interface for a Coedit subject. Basically designed to hold a list of observer
   *)
  ICESubject = interface
    // an observer is proposed. anObserver is not necessarly compatible.
    procedure addObserver(observer: TObject);
    // anObserver must be removed.
    procedure removeObserver(observer: TObject);
    // optionally implemented to trigger all the methods of the observer interface.
  end;

  (**
   * Base type used as constraint for an interface that contains
   * the methods called by a ICESubject.
   *)
  IObserverType = interface
  end;

  (**
   * Standard implementation of an ICESubject.
   * Any descendant automatically adds itself to the EntitiesConnector.
   *)
  generic TCECustomSubject<T:IObserverType> = class(ICESubject)
  protected
    fObservers: TObjectList;
    // test for a specific interface when adding an observer.
    function acceptObserver(observer: TObject): boolean;
    function getObserversCount: Integer;
    function getObserver(index: Integer): TObject;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    //
    procedure addObserver(observer: TObject);
    procedure removeObserver(observer: TObject);
    //
    property observersCount: Integer read getObserversCount;
    property observers[index: Integer]: TObject read getObserver; default;
  end;

var
  EntitiesConnector: TCEEntitiesConnector = nil;

implementation

uses
  LCLProc;

{$REGION TCEEntitiesConnector --------------------------------------------------}
constructor TCEEntitiesConnector.Create;
begin
  fObservers := TObjectList.Create(False);
  fSubjects := TObjectList.Create(False);
  fServices := TServiceList.Create();
end;

destructor TCEEntitiesConnector.Destroy;
begin
  fObservers.Free;
  fSubjects.Free;
  fServices.Free;
  inherited;
end;

function TCEEntitiesConnector.getIsUpdating: boolean;
begin
  exit(fUpdatesCount > 0);
end;

procedure TCEEntitiesConnector.tryUpdate;
begin
  if fUpdatesCount <= 0 then
    updateEntities;
end;

procedure TCEEntitiesConnector.forceUpdate;
begin
  updateEntities;
end;

procedure TCEEntitiesConnector.updateEntities;
var
  i, j: Integer;
begin
  fUpdatesCount := 0;
  for i := 0 to fSubjects.Count - 1 do
  begin
    if not (fSubjects[i] is ICESubject) then
      continue;
    for j := 0 to fObservers.Count - 1 do
    begin
      if fSubjects[i] <> fObservers[j] then
        (fSubjects[i] as ICESubject).addObserver(fObservers[j]);
    end;
  end;
end;

procedure TCEEntitiesConnector.beginUpdate;
begin
  fUpdatesCount += 1;
end;

procedure TCEEntitiesConnector.endUpdate;
begin
  fUpdatesCount -= 1;
  tryUpdate;
end;

procedure TCEEntitiesConnector.addObserver(observer: TObject);
begin
  if fObservers.IndexOf(observer) <> -1 then
    exit;
  fObservers.Add(observer);
  tryUpdate;
end;

procedure TCEEntitiesConnector.addSubject(subject: TObject);
begin
  if (subject as ICESubject) = nil then
    exit;
  if fSubjects.IndexOf(subject) <> -1 then
    exit;
  fSubjects.Add(subject);
  tryUpdate;
end;

procedure TCEEntitiesConnector.removeObserver(observer: TObject);
var
  i: Integer;
begin
  fObservers.Remove(observer);
  for i := 0 to fSubjects.Count - 1 do
    if fSubjects[i] <> nil then
      (fSubjects[i] as ICESubject).removeObserver(observer);
  tryUpdate;
end;

procedure TCEEntitiesConnector.removeSubject(subject: TObject);

begin
  fSubjects.Remove(subject);
  tryUpdate;
end;

procedure TCEEntitiesConnector.addSingleService(provider: TObject);
begin
  if not (provider is ICESingleService) then
    exit;
  fServices.insert((provider as ICESingleService).singleServiceName, provider);
end;

function TCEEntitiesConnector.getSingleService(const serviceName: string): TObject;
begin
  Result := nil;
  if not fServices.GetValue(serviceName, result) then
    result := nil;
end;
{$ENDREGION}

{$REGION TCECustomSubject ------------------------------------------------------}
constructor TCECustomSubject.Create;
begin
  fObservers := TObjectList.Create(False);
  EntitiesConnector.addSubject(Self);
end;

destructor TCECustomSubject.Destroy;
begin
  EntitiesConnector.removeSubject(Self);
  fObservers.Free;
  Inherited;
end;

function TCECustomSubject.acceptObserver(observer: TObject): boolean;
begin
  exit(observer is T);
end;

function TCECustomSubject.getObserversCount: Integer;
begin
  exit(fObservers.Count);
end;

function TCECustomSubject.getObserver(index: Integer): TObject;
begin
  exit(fObservers.Items[index]);
end;

procedure TCECustomSubject.addObserver(observer: TObject);
begin
  if not acceptObserver(observer) then
    exit;
  if fObservers.IndexOf(observer) <> -1 then
    exit;
  fObservers.Add(observer);
end;

procedure TCECustomSubject.removeObserver(observer: TObject);
begin
  fObservers.Remove(observer);
end;
{$ENDREGION}

initialization
  EntitiesConnector := TCEEntitiesConnector.Create;
  EntitiesConnector.beginUpdate;

finalization
  EntitiesConnector.Free;
  EntitiesConnector := nil;
end.
