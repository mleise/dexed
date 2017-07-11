unit ce_shortcutseditor;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, TreeFilterEdit, Forms, Controls, Menus, Graphics,
  ExtCtrls, LCLProc, ComCtrls, Buttons, LCLType, PropEdits, RTTIGrids,
  strutils, ce_sharedres, ce_observer, ce_interfaces, ce_common,
  ce_writableComponent, ce_dialogs, EditBtn;

type

  TShortcutItem = class(TCollectionItem)
  private
    fIdentifier: string;
    fData: TShortcut;
    fDeclarator: ICEEditableShortCut;
    property declarator: ICEEditableShortCut read fDeclarator write fDeclarator;
  published
    property identifier: string read fIdentifier write fIdentifier;
    property data: TShortcut read fData write fData;
  public
    function combination: string;
    procedure assign(source: TPersistent); override;
  end;

  TShortCutCollection = class(TWritableLfmTextComponent)
  private
    fItems: TCollection;
    procedure setItems(value: TCollection);
    function getCount: Integer;
    function getItem(index: Integer): TShortcutItem;
  published
    property items: TCollection read fItems write setItems;
  public
    constructor create(AOwner: TComponent); override;
    destructor destroy; override;
    procedure assign(source: TPersistent); override;
    //
    function findIdentifier(const identifier: string): boolean;
    function findShortcut(aShortcut: Word): TShortcutItem;
    //
    property count: Integer read getCount;
    property item[index: Integer]: TShortcutItem read getItem; default;
  end;

  TEditableShortcut = class(TPersistent)
  public
    value: TShortCut;
  published
    property shortcut: TShortCut read value write value;
  end;

  { TCEShortcutEditor }

  TCEShortcutEditor = class(TFrame, ICEEditableOptions)
    btnClear: TSpeedButton;
    btnEdit: TSpeedButton;
    Panel1: TPanel;
    fltItems: TTreeFilterEdit;
    Panel2: TPanel;
    propedit: TTIPropertyGrid;
    tree: TTreeView;
    procedure btnClearClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    function fltItemsFilterItem(Item: TObject; out Done: Boolean): Boolean;
    procedure shortcutCatcherExit(Sender: TObject);
    procedure shortcutCatcherMouseLeave(Sender: TObject);
    procedure propeditModified(Sender: TObject);
    procedure treeSelectionChanged(Sender: TObject);
  private
    fObservers: TCEEditableShortCutSubject;
    fShortcuts: TShortCutCollection;
    fBackup: TShortCutCollection;
    fHasChanged: boolean;
    propvalue: TEditableShortcut;
    //
    function optionedWantCategory(): string;
    function optionedWantEditorKind: TOptionEditorKind;
    function optionedWantContainer: TPersistent;
    procedure optionedEvent(event: TOptionEditorEvent);
    function optionedOptionsModified: boolean;
    //
    function findCategory(const aName: string; aData: Pointer): TTreeNode;
    function findCategory(const aShortcutItem: TShortcutItem): string;
    function sortCategories(Cat1, Cat2: TTreeNode): integer;
    procedure receiveShortcuts;
    procedure updateEditCtrls;
    procedure sendShortcuts;
    function anItemIsSelected: boolean;
  public
    constructor create(TheOwner: TComponent); override;
    destructor destroy; override;
  end;

implementation
{$R *.lfm}

var
  CEShortcutEditor: TCEShortcutEditor;

{$REGION TShortCutCollection ---------------------------------------------------}
function TShortcutItem.combination: string;
begin
  result := ShortCutToText(fData);
end;

procedure TShortcutItem.assign(source: TPersistent);
var
  src: TShortcutItem;
begin
  if source is TShortcutItem then
  begin
    src := TShortcutItem(source);
    fData:= src.fData;
    fIdentifier:= src.fIdentifier;
    fDeclarator := src.fDeclarator;
  end
  else inherited;
end;

constructor TShortCutCollection.create(AOwner: TComponent);
begin
  inherited;
  fItems := TCollection.Create(TShortcutItem);
end;

destructor TShortCutCollection.destroy;
begin
  fItems.Free;
  inherited;
end;

procedure TShortCutCollection.assign(source: TPersistent);
begin
  if source is TShortCutCollection then
    fItems.Assign(TShortCutCollection(source).fItems)
  else
    inherited;
end;

procedure TShortCutCollection.setItems(value: TCollection);
begin
  fItems.Assign(value);
end;

function TShortCutCollection.getCount: Integer;
begin
  exit(fItems.Count);
end;

function TShortCutCollection.getItem(index: Integer): TShortcutItem;
begin
  exit(TShortcutItem(fItems.Items[index]));
end;

function TShortCutCollection.findIdentifier(const identifier: string): boolean;
var
  i: Integer;
begin
  result := false;
  for i := 0 to count-1 do
    if item[i].identifier = identifier then
      exit(true);
end;

function TShortCutCollection.findShortcut(aShortcut: Word): TShortcutItem;
var
  i: Integer;
begin
  result := nil;
  for i := 0 to count-1 do
    if item[i].data = aShortcut then
      exit(item[i]);
end;
{$ENDREGION}

{$REGION Standard Comp/Object things -------------------------------------------}
constructor TCEShortcutEditor.create(TheOwner: TComponent);
begin
  inherited;
  propvalue := TEditableShortcut.Create;
  fObservers := TCEEditableShortCutSubject.create;
  fShortcuts := TShortCutCollection.create(self);
  fBackup := TShortCutCollection.create(self);
  AssignPng(btnClear, 'CLEAN');
  AssignPng(btnEdit, 'KEYBOARD_PENCIL');
  EntitiesConnector.addObserver(self);
  propedit.TIObject := propvalue;
  propedit.PropertyEditorHook.AddHandlerModified(@propeditModified);
end;

destructor TCEShortcutEditor.destroy;
begin
  propvalue.Free;
  fObservers.Free;
  inherited;
end;

function TCEShortcutEditor.anItemIsSelected: boolean;
begin
  result := true;
  if tree.Selected.isNil or (tree.Selected.Level = 0) or tree.Selected.Data.isNil then
    result := false;
end;
{$ENDREGION}

{$REGION ICEEditableOptions ----------------------------------------------------}
function TCEShortcutEditor.optionedWantCategory(): string;
begin
  exit('Shortcuts');
end;

function TCEShortcutEditor.optionedWantEditorKind: TOptionEditorKind;
begin
  exit(oekControl);
end;

function TCEShortcutEditor.optionedWantContainer: TPersistent;
begin
  receiveShortcuts;
  exit(self);
end;

procedure TCEShortcutEditor.optionedEvent(event: TOptionEditorEvent);
begin
  case event of
    oeeSelectCat: receiveShortcuts;
    oeeCancel:
    begin
      fShortcuts.assign(fBackup);
      sendShortcuts;
      fHasChanged := false;
    end;
    oeeAccept:
    begin
      fBackup.assign(fShortcuts);
      sendShortcuts;
      fHasChanged := false;
    end;
  end;
end;

function TCEShortcutEditor.optionedOptionsModified: boolean;
begin
  exit(fHasChanged);
end;
{$ENDREGION}

{$REGION shortcut editor things ------------------------------------------------}
procedure TCEShortcutEditor.treeSelectionChanged(Sender: TObject);
begin
  updateEditCtrls;
end;

procedure TCEShortcutEditor.shortcutCatcherExit(Sender: TObject);
begin
  updateEditCtrls;
end;

procedure TCEShortcutEditor.shortcutCatcherMouseLeave(Sender: TObject);
begin
  updateEditCtrls;
end;

procedure TCEShortcutEditor.propeditModified(Sender: TObject);
var
  i: integer;
  sh: TShortCut;
  sht: string;
  dup: TShortcutItem = nil;
const
  msg = '"%s" is already assigned in the same category by "%s". The new shortcut will be ignored';
begin
  if not anItemIsSelected then
    exit;
  sh := propvalue.value;
  sht := shortCutToText(sh);
  if sht.isEmpty then
    exit;
  for i:= 0 to tree.Selected.Parent.Count-1 do
    if i <> tree.Selected.Index then
      if TShortcutItem(tree.Selected.Parent.Items[i].Data).data = sh then
        dup := TShortcutItem(tree.Selected.Parent.Items[i].Data);
  if dup.isNotNil then
    dlgOkInfo(format(msg,[ShortCutToText(sh), dup.identifier]))
  else if TShortcutItem(tree.Selected.Data).data <> sh then
  begin
    TShortcutItem(tree.Selected.Data).data := sh;
    fHasChanged := true;
  end;
  updateEditCtrls;
end;

procedure TCEShortcutEditor.btnClearClick(Sender: TObject);
begin
  if not anItemIsSelected then
    exit;
  if TShortcutItem(tree.Selected.Data).data <> 0 then
  begin
    TShortcutItem(tree.Selected.Data).data := 0;
    fHasChanged := true;
  end;
  updateEditCtrls;
end;

procedure TCEShortcutEditor.btnEditClick(Sender: TObject);
begin
  if not anItemIsSelected then
    exit;
  propedit.Rows[0].Editor.Edit;
end;

function TCEShortcutEditor.fltItemsFilterItem(Item: TObject; out Done: Boolean): Boolean;
var
  shc: TShortcutItem;
begin
  if fltItems.Filter.isBlank then
  begin
    result := true;
    done := true;
  end
  else
  begin
    result := false;
    done := false;
    // see TTreeFilterEdit: they pass TObject(TTreeNode.Data) and not a TTreeNode
    if Pointer(item).isNil then
      exit;
    shc := TShortcutItem(item);
    result := AnsiContainsText(shc.combination, fltItems.Filter);
    done := true;
  end;
end;

procedure TCEShortcutEditor.updateEditCtrls;
var
  shc: TShortcutItem;
begin
  if not anItemIsSelected then
    exit;
  shc := TShortcutItem(tree.Selected.Data);
  if propvalue.value <> shc.data then
  begin
    propvalue.value := shc.data;
    propedit.BuildPropertyList;
  end;
end;

function TCEShortcutEditor.findCategory(const aName: string; aData: Pointer): TTreeNode;
var
  i: integer;
begin
  result := nil;
  for i:= 0 to tree.Items.Count-1 do
    if tree.Items[i].Text = aName then
      if tree.Items[i].Data = aData then
        exit(tree.Items[i]);
end;

function TCEShortcutEditor.findCategory(const aShortcutItem: TShortcutItem): string;
var
  i, j: integer;
begin
  result := '';
  for i := 0 to tree.Items.Count-1 do
    for j:= 0 to tree.Items.Item[i].Count-1 do
      if tree.Items.Item[i].Items[j].Data = Pointer(aShortcutItem) then
        exit(tree.Items.Item[i].Text);
end;

function TCEShortcutEditor.sortCategories(Cat1, Cat2: TTreeNode): integer;
begin
  result := CompareText(Cat1.Text, Cat2.Text);
end;

procedure TCEShortcutEditor.receiveShortcuts;
var
  i: Integer;
  obs: ICEEditableShortCut;
  cat: string;
  sht: word;
  idt: string;
  itm: TShortcutItem;
procedure addItem();
var
  prt: TTreeNode;
begin
  // root category
  if cat.isEmpty or idt.isEmpty then
    exit;
  prt := findCategory(cat, obs);
  if prt.isNil then
    prt := tree.Items.AddObject(nil, cat, obs);
  // item as child
  itm := TShortcutItem(fShortcuts.items.Add);
  itm.identifier := idt;
  itm.data:= sht;
  itm.declarator := obs;
  tree.Items.AddChildObject(prt, idt, itm);
  cat := '';
  idt := '';
end;
begin
  tree.Items.Clear;
  fShortcuts.items.Clear;
  fBackup.items.Clear;
  cat := '';
  idt := '';
  for i:= 0 to fObservers.observersCount-1 do
  begin
    obs := fObservers.observers[i] as ICEEditableShortCut;
    if obs.scedWantFirst then
    begin
      while obs.scedWantNext(cat, idt, sht) do
        addItem();
      addItem();
    end;
  end;
  tree.Items.SortTopLevelNodes(@sortCategories);
  fBackup.Assign(fShortcuts);
end;

procedure TCEShortcutEditor.sendShortcuts;
var
  i: integer;
  shc: TShortcutItem;
  decl: ICEEditableShortCut = nil;
  cat: string;
begin
  for i := 0 to fShortcuts.count-1 do
  begin
    shc := fShortcuts[i];
    decl:= shc.declarator;
    if decl = nil then
      continue;
    cat := findCategory(shc);
    if cat.isEmpty then
      continue;
    decl.scedSendItem(cat, shc.identifier, shc.data);
    if i = fShortcuts.count-1 then
      decl.scedSendDone
    // fShortcuts is always sorted by declarator, cf. receiveShortcuts()
    else if decl <> fShortcuts[i+1].declarator then
      decl.scedSendDone;
  end;
end;

{$ENDREGION}

initialization
  CEShortcutEditor := TCEShortcutEditor.Create(nil);
finalization
  CEShortcutEditor.Free;
end.

