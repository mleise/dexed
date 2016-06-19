unit ce_messages;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  EditBtn, lcltype, ce_widget, ActnList, Menus, clipbrd, AnchorDocking, math,
  TreeFilterEdit, Buttons, process, GraphType, fgl,
  ce_writableComponent, ce_common, ce_synmemo, ce_interfaces, ce_observer,
  ce_processes, ce_sharedres, ce_stringrange;

type

  TCEEditorMessagePos = class(specialize TFPGMap<string,integer>);

  (**
   * the struct linked to a log message. allow to be filtered.
   *)
  PMessageData = ^TMessageData;
  TMessageData = record
    ctxt: TCEAppMessageCtxt;
    data: Pointer;
    demangled: boolean;
  end;

  TCEMessagesOptions = class(TWritableLfmTextComponent)
  private
    fFastDisplay: boolean;
    fMaxCount: Integer;
    fAutoSelect: boolean;
    fSingleClick: boolean;
    fFont: TFont;
    fMsgColors: array[TCEAppMessageKind] of TColor;
    procedure setFont(aValue: TFont);
  published
    property fastDisplay: boolean read fFastDisplay write fFastDisplay;
    property maxMessageCount: integer read fMaxCount write fMaxCount;
    property autoSelect: boolean read fAutoSelect write fAutoSelect;
    property singleMessageClick: boolean read fSingleClick write fSingleClick;
    property font: TFont read fFont write setFont;
    property colorBuble: TColor read fMsgColors[amkBub] write fMsgColors[amkBub];
    property colorInfo: TColor read fMsgColors[amkInf] write fMsgColors[amkInf];
    property colorHint: TColor read fMsgColors[amkHint] write fMsgColors[amkHint];
    property colorWarning: TColor read fMsgColors[amkWarn] write fMsgColors[amkWarn];
    property colorError: TColor read fMsgColors[amkErr] write fMsgColors[amkErr];
  public
    constructor Create(AOwner: TComponent); override;
    destructor destroy; override;
    procedure assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
  end;

  // grants access to some protected fields that should be actually public (scoll position needed for custom draw !)
  TTreeHack = class(TTreeView)
  end;

  { TCEMessagesWidget }

  TCEMessagesWidget = class(TCEWidget, ICEEditableOptions, ICEMultiDocObserver, ICEProjectObserver, ICEMessagesDisplay)
    btnClearCat: TBitBtn;
    imgList: TImageList;
    List: TTreeView;
    selCtxt: TToolBar;
    btnSelAll: TToolButton;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    btnSelMisc: TToolButton;
    ToolButton11: TToolButton;
    ToolButton2: TToolButton;
    btnSelEdit: TToolButton;
    ToolButton4: TToolButton;
    btnSelProj: TToolButton;
    ToolButton8: TToolButton;
    btnSelApp: TToolButton;
    TreeFilterEdit1: TTreeFilterEdit;
    procedure ListCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure ListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TreeFilterEdit1AfterFilter(Sender: TObject);
    procedure TreeFilterEdit1ButtonClick(Sender: TObject);
  private
    fEditorMessagePos: TCEEditorMessagePos;
    fDemanglerAvailable: boolean;
    fMsgColors: array[TCEAppMessageKind] of TColor;
    fActAutoSel: TAction;
    fActClearAll: TAction;
    fActClearCurCat: TAction;
    fActSaveMsg: TAction;
    fActCopyMsg: TAction;
    fActSelAll: TAction;
    fActDemangle: TAction;
    fMaxMessCnt: Integer;
    fProj: ICECommonProject;
    fDoc: TCESynMemo;
    fCtxt: TCEAppMessageCtxt;
    fAutoSelect: boolean;
    fSingleClick: boolean;
    fastDisplay: boolean;
    fDemangler: TCEProcess;
    fOptions: TCEMessagesOptions;
    fOptionsBackup: TCEMessagesOptions;
    fBtns: array[TCEAppMessageCtxt] of TToolButton;
    fToDemangle: TStringList;
    fToDemangleObjs: TFPList;
    fFiltering: boolean;
    function itemShouldBeVisible(item: TTreeNode; aCtxt: TCEAppMessageCtxt): boolean;
    procedure demanglerOutput(sender: TObject);
    procedure filterMessages(aCtxt: TCEAppMessageCtxt);
    procedure clearOutOfRangeMessg;
    procedure actDemangleExecute(Sender: TObject);
    procedure actAutoSelExecute(Sender: TObject);
    procedure actClearCurCatExecute(Sender: TObject);
    procedure actClearAllExecute(Sender: TObject);
    procedure actSaveMsgExecute(Sender: TObject);
    procedure actCopyMsgExecute(Sender: TObject);
    procedure actSelAllExecute(Sender: TObject);
    procedure setMaxMessageCount(aValue: Integer);
    procedure setAutoSelectCategory(aValue: boolean);
    procedure setSingleMessageClick(aValue: boolean);
    procedure listDeletion(Sender: TObject; Node: TTreeNode);
    procedure selCtxtClick(Sender: TObject);
    function iconIndex(aKind: TCEAppMessageKind): Integer;
    procedure handleMessageClick(Sender: TObject);
    procedure callDemangler;
    procedure freeDemangler;
    //
    procedure setColorError(aValue: TColor);
    procedure setColorInfo(aValue: TColor);
    procedure setColorHint(aValue: TColor);
    procedure setColorBuble(aValue: TColor);
    procedure setColorWarning(aValue: TColor);
    //
    procedure projNew(aProject: ICECommonProject);
    procedure projClosing(aProject: ICECommonProject);
    procedure projFocused(aProject: ICECommonProject);
    procedure projChanged(aProject: ICECommonProject);
    procedure projCompiling(aProject: ICECommonProject);
    procedure projCompiled(aProject: ICECommonProject; success: boolean);
    //
    procedure docNew(aDoc: TCESynMemo);
    procedure docClosing(aDoc: TCESynMemo);
    procedure docFocused(aDoc: TCESynMemo);
    procedure docChanged(aDoc: TCESynMemo);
    //
    function optionedWantCategory(): string;
    function optionedWantEditorKind: TOptionEditorKind;
    function optionedWantContainer: TPersistent;
    procedure optionedEvent(anEvent: TOptionEditorEvent);
    function optionedOptionsModified: boolean;
    //
    function openFileFromDmdMessage(const aMessage: string): boolean;
    function getLineFromMessage(const aMessage: string): TPoint;
    function guessMessageKind(const aMessg: string): TCEAppMessageKind;
    //
    function singleServiceName: string;
    procedure message(const aValue: string; aData: Pointer; aCtxt: TCEAppMessageCtxt; aKind: TCEAppMessageKind);
    procedure clearbyContext(aCtxt: TCEAppMessageCtxt);
    procedure clearbyData(aData: Pointer);
    procedure scrollToBack;
  protected
    procedure updateLoop; override;
    //
    function contextName: string; override;
    function contextActionCount: integer; override;
    function contextAction(index: integer): TAction; override;
    //
    property maxMessageCount: Integer     read fMaxMessCnt  write setMaxMessageCount;
    property autoSelectCategory: boolean  read fAutoSelect  write setAutoSelectCategory;
    property singleMessageClick: boolean  read fSingleClick write setSingleMessageClick;
    //
    property colorBuble: TColor   read fMsgColors[amkBub]   write setColorBuble;
    property colorInfo: TColor    read fMsgColors[amkInf]   write setColorInfo;
    property colorHint: TColor    read fMsgColors[amkHint]  write setColorHint;
    property colorWarning: TColor read fMsgColors[amkWarn]  write setColorWarning;
    property colorError: TColor   read fMsgColors[amkErr]   write setColorError;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
  end;

implementation
{$R *.lfm}

const
  optname = 'messages.txt';
  minColor = $232323;

{$REGION TCEMessagesOptions ----------------------------------------------------}
constructor TCEMessagesOptions.Create(AOwner: TComponent);
begin
  inherited;
  fFont := TFont.Create;
end;

destructor TCEMessagesOptions.destroy;
begin
  fFont.Free;
  inherited;
end;

procedure TCEMessagesOptions.setFont(aValue: TFont);
begin
  fFont.Assign(aValue);
end;

procedure TCEMessagesOptions.assign(Source: TPersistent);
var
  widg : TCEMessagesWidget;
  opts : TCEMessagesOptions;
begin
  if Source is TCEMessagesOptions then
  begin
    opts := TCEMessagesOptions(Source);
    fFont.BeginUpdate;
    fFont.Assign(opts.font);
    fMaxCount := opts.fMaxCount;
    fAutoSelect := opts.fAutoSelect;
    fSingleClick := opts.fSingleClick;
    fFastDisplay := opts.fFastDisplay;
    fMsgColors := opts.fMsgColors;
    fFont.EndUpdate;
  end
  else if Source is TCEMessagesWidget then
  begin
    widg := TCEMessagesWidget(Source);
    fFont.Assign(widg.List.Font);
    fMaxCount := widg.fMaxMessCnt;
    fAutoSelect := widg.fAutoSelect;
    fSingleClick := widg.fSingleClick;
    fFastDisplay := widg.fastDisplay;
    fMsgColors := widg.fMsgColors;
  end
  else inherited;
end;

procedure TCEMessagesOptions.AssignTo(Dest: TPersistent);
var
  widg : TCEMessagesWidget;
begin
  if Dest is TCEMessagesWidget then
  begin
    widg := TCEMessagesWidget(Dest);
    widg.List.Font.Assign(fFont);
    widg.maxMessageCount := fMaxCount;
    widg.autoSelectCategory := fAutoSelect;
    widg.singleMessageClick := fSingleClick;
    widg.fastDisplay:= fFastDisplay;
    widg.fMsgColors := fMsgColors;
  end
  else inherited;
end;
{$ENDREGION}

{$REGION Standard Comp/Obj------------------------------------------------------}
constructor TCEMessagesWidget.create(aOwner: TComponent);
var
  fname: string;
begin
  fMaxMessCnt := 500;
  fCtxt := amcAll;
  //
  fActAutoSel := TAction.Create(self);
  fActAutoSel.Caption := 'Auto select message category';
  fActAutoSel.AutoCheck := true;
  fActAutoSel.OnExecute := @actAutoSelExecute;
  fActClearAll := TAction.Create(self);
  fActClearAll.OnExecute := @actClearAllExecute;
  fActClearAll.caption := 'Clear all messages';
  fActClearCurCat := TAction.Create(self);
  fActClearCurCat.OnExecute := @actClearCurCatExecute;
  fActClearCurCat.caption := 'Clear filtered messages';
  fActCopyMsg := TAction.Create(self);
  fActCopyMsg.OnExecute := @actCopyMsgExecute;
  fActCopyMsg.Caption := 'Copy message(s)';
  fActSelAll := TAction.Create(self);
  fActSelAll.OnExecute := @actSelAllExecute;
  fActSelAll.Caption := 'Select all';
  fActSaveMsg := TAction.Create(self);
  fActSaveMsg.OnExecute := @actSaveMsgExecute;
  fActSaveMsg.caption := 'Save selected message(s) to...';
  fActDemangle := TAction.Create(self);
  fActDemangle.OnExecute := @actDemangleExecute;
  fActDemangle.caption := 'Demangle selection';
  //
  inherited;
  //
  fMsgColors[amkBub]  := clDefault;
  fMsgColors[amkHint] := clDefault;
  fMsgColors[amkInf]  := clDefault;
  fMsgColors[amkErr]  := clDefault;
  fMsgColors[amkWarn] := clDefault;
  //
  updaterByLoopInterval := 12;
  fOptions := TCEMessagesOptions.Create(Self);
  fOptions.assign(self);
  fOptions.Name:= 'messageOptions';
  fOptionsBackup := TCEMessagesOptions.Create(Self);
  //
  List.PopupMenu := contextMenu;
  List.OnDeletion := @ListDeletion;
  List.OnDblClick := @handleMessageClick;
  //
  btnSelProj.OnClick  := @selCtxtClick;
  btnSelMisc.OnClick  := @selCtxtClick;
  btnSelEdit.OnClick  := @selCtxtClick;
  btnSelApp.OnClick   := @selCtxtClick;
  btnSelAll.OnClick   := @selCtxtClick;
  fBtns[amcAll] := btnSelAll;
  fBtns[amcApp] := btnSelApp;
  fBtns[amcEdit]:= btnSelEdit;
  fBtns[amcMisc]:= btnSelMisc;
  fBtns[amcProj]:= btnSelProj;
  //
  btnClearCat.OnClick := @actClearCurCatExecute;
  AssignPng(btnClearCat, 'CLEAN');
  //
  fEditorMessagePos := TCEEditorMessagePos.Create;
  //
  fname := getCoeditDocPath + optname;
  if fname.fileExists then
  begin
    fOptions.loadFromFile(fname);
    fOptions.AssignTo(self);
  end;
  //
  fToDemangle := TStringList.Create;
  fToDemangleObjs:= TFPList.Create;
  //
  EntitiesConnector.addObserver(self);
  EntitiesConnector.addSingleService(self);
end;

destructor TCEMessagesWidget.destroy;
begin
  fEditorMessagePos.Free;
  fToDemangle.Free;
  FreeAndNil(fToDemangleObjs);
  freeDemangler;
  fOptions.saveToFile(getCoeditDocPath + optname);
  EntitiesConnector.removeObserver(self);
  inherited;
end;

procedure TCEMessagesWidget.listDeletion(Sender: TObject; Node: TTreeNode);
var
  i: integer;
begin
  if node.data.isNotNil then
    Dispose(PMessageData(Node.Data));
  if fToDemangleObjs.isNotNil then
  begin
    i := fToDemangleObjs.IndexOf(node);
    if i <> -1 then if i < fToDemangleObjs.Count then
      fToDemangleObjs.Items[i] := nil;
  end;
end;

procedure TCEMessagesWidget.ListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  i: Integer;
begin
  case Key of
    VK_BACK, VK_DELETE:
    begin
      if List.SelectionCount > 0 then
      begin
      for i := List.Items.Count-1 downto 0 do
        if List.Items[i].MultiSelected then
          List.Items.Delete(List.Items[i]);
      end
      else clearbyContext(amcAll);
    end;
    VK_UP, VK_DOWN:
      if fOptions.singleMessageClick then handleMessageClick(nil);
    VK_RETURN:
      handleMessageClick(nil);
  end;
end;

procedure TCEMessagesWidget.TreeFilterEdit1AfterFilter(Sender: TObject);
begin
  fFiltering := TreeFilterEdit1.Filter.isNotEmpty;
  filterMessages(fCtxt);
end;

procedure TCEMessagesWidget.TreeFilterEdit1ButtonClick(Sender: TObject);
begin
  fFiltering := false;
  filterMessages(fCtxt);
end;

procedure TCEMessagesWidget.selCtxtClick(Sender: TObject);
var
  btn: TToolButton;
  i: Integer;
begin
  if sender.isNil then
    exit;
  //
  fCtxt := amcAll;
  btn := TToolButton(Sender);
  for i := 0 to selCtxt.ButtonCount-1 do
    selCtxt.Buttons[i].Down := selCtxt.Buttons[i] = btn;
  if btn = btnSelAll  then
    fCtxt := amcAll
  else if btn = btnSelEdit then
    fCtxt := amcEdit
  else if btn = btnSelProj then
    fCtxt := amcProj
  else if btn = btnSelApp then
    fCtxt := amcApp
  else if btn = btnSelMisc then
    fCtxt := amcMisc;
  filterMessages(fCtxt);
end;

procedure TCEMessagesWidget.setMaxMessageCount(aValue: Integer);
begin
  if aValue < 5 then
    aValue := 5;
  if fMaxMessCnt = aValue then
    exit;
  fMaxMessCnt := aValue;
  clearOutOfRangeMessg;
end;

procedure TCEMessagesWidget.setAutoSelectCategory(aValue: boolean);
begin
  fAutoSelect := aValue;
  fActAutoSel.Checked:= fAutoSelect;
end;

procedure TCEMessagesWidget.setSingleMessageClick(aValue: boolean);
begin
  fSingleClick := aValue;
  if fSingleClick then
  begin
    List.OnClick := @handleMessageClick;
    List.OnDblClick:= nil;
  end else begin
    List.OnClick := nil;
    List.OnDblClick:= @handleMessageClick;
  end;
end;

procedure TCEMessagesWidget.setColorError(aValue: TColor);
begin
  fMsgColors[amkErr] := max(aValue, minColor);
  List.Invalidate;
end;

procedure TCEMessagesWidget.setColorInfo(aValue: TColor);
begin
  fMsgColors[amkInf] := max(aValue, minColor);
  List.Invalidate;
end;

procedure TCEMessagesWidget.setColorHint(aValue: TColor);
begin
  fMsgColors[amkHint] := max(aValue, minColor);
  List.Invalidate;
end;

procedure TCEMessagesWidget.setColorBuble(aValue: TColor);
begin
  fMsgColors[amkBub] := max(aValue, minColor);
  List.Invalidate;
end;

procedure TCEMessagesWidget.setColorWarning(aValue: TColor);
begin
  fMsgColors[amkWarn] := aValue;
  List.Invalidate;
end;

procedure TCEMessagesWidget.ListCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  x: integer;
  rc: TRect;
begin
  rc := node.DisplayRect(false);
  x := rc.Left + 2 - TTreeHack(list).ScrolledLeft;
  // warning: the cast may become wrong if the enum is modified.
  Sender.Canvas.Brush.Color := fMsgColors[TCEAppMessageKind(node.ImageIndex + 1)];
  if node.Selected then
  begin
    Sender.Canvas.DrawFocusRect(rc);
    Sender.Canvas.Brush.Color := Sender.Canvas.Brush.Color - minColor;
  end;
  Sender.Canvas.FillRect(rc);
  list.Images.Draw(sender.Canvas, x, (rc.Top + rc.Bottom - list.Images.Height) div 2,
    node.ImageIndex, Node.NodeEffect);
  x += list.Images.Width + 5;
  Sender.Canvas.TextOut(x, rc.Top, node.Text);
  DefaultDraw := false;
end;
{$ENDREGION}

{$REGION ICEEditableOptions ----------------------------------------------------}
function TCEMessagesWidget.optionedWantCategory(): string;
begin
  exit('Messages');
end;

function TCEMessagesWidget.optionedWantEditorKind: TOptionEditorKind;
begin
  exit(oekGeneric);
end;

function TCEMessagesWidget.optionedWantContainer: TPersistent;
begin
  fOptions.assign(self);
  fOptionsBackup.assign(self);
  exit(fOptions);
end;

procedure TCEMessagesWidget.optionedEvent(anEvent: TOptionEditorEvent);
begin
  case anEvent of
    oeeAccept, oeeSelectCat:
      fOptionsBackup.assign(fOptions);
    oeeCancel:
      fOptions.assign(fOptionsBackup);
  end;
  fOptions.AssignTo(self);
  List.Invalidate;
end;

function TCEMessagesWidget.optionedOptionsModified: boolean;
begin
  exit(false);
end;
{$ENDREGION}

{$REGION ICEContextualActions---------------------------------------------------}
function TCEMessagesWidget.contextName: string;
begin
  result := 'Messages';
end;

function TCEMessagesWidget.contextActionCount: integer;
begin
  result := 7;
end;

function TCEMessagesWidget.contextAction(index: integer): TAction;
begin
  case index of
    0: result := fActAutoSel;
    1: result := fActClearAll;
    2: result := fActClearCurCat;
    3: result := fActCopyMsg;
    4: result := fActSelAll;
    5: result := fActSaveMsg;
    6: result := fActDemangle;
    else result := nil;
  end;
end;

procedure TCEMessagesWidget.actDemangleExecute(Sender: TObject);
begin
  callDemangler;
end;

procedure TCEMessagesWidget.actAutoSelExecute(Sender: TObject);
begin
  fAutoSelect := fActAutoSel.Checked;
end;

procedure TCEMessagesWidget.actClearAllExecute(Sender: TObject);
begin
  clearbyContext(amcAll);
end;

procedure TCEMessagesWidget.actClearCurCatExecute(Sender: TObject);
begin
  case fCtxt of
    amcAll, amcApp, amcMisc :
      clearbyContext(fCtxt);
    amcEdit: if fDoc.isNotNil then
      clearbyData(fDoc);
    amcProj: if fProj <> nil then
      clearbyData(fProj);
  end;
end;

procedure TCEMessagesWidget.actCopyMsgExecute(Sender: TObject);
var
  i: Integer;
  str: string = '';
begin
  for i := 0 to List.Items.Count-1 do
    if List.Items[i].MultiSelected then
      str += List.Items[i].Text + LineEnding;
  Clipboard.AsText := str;
end;

procedure TCEMessagesWidget.actSelAllExecute(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to List.Items.Count-1 do
    if List.Items[i].Visible then
      List.Items[i].MultiSelected := true;
end;

procedure TCEMessagesWidget.actSaveMsgExecute(Sender: TObject);
var
  lst: TStringList;
  itm: TtreeNode;
begin
  with TSaveDialog.Create(nil) do
  try
    if execute then
    begin
      lst := TStringList.Create;
      try
        for itm in List.Items do
          lst.Add(itm.Text);
        lst.SaveToFile(filename);
      finally
        lst.Free;
      end;
    end;
  finally
    free;
  end;
end;
{$ENDREGION}

{$REGION ICEProjectObserver ----------------------------------------------------}
procedure TCEMessagesWidget.projNew(aProject: ICECommonProject);
begin
  fProj := aProject;
  filterMessages(fCtxt);
end;

procedure TCEMessagesWidget.projClosing(aProject: ICECommonProject);
begin
  if fProj <> aProject then
    exit;
  //
  clearbyData(fProj);
  fProj := nil;
  filterMessages(fCtxt);
end;

procedure TCEMessagesWidget.projFocused(aProject: ICECommonProject);
begin
  if fProj = aProject then exit;
  fProj := aProject;
  filterMessages(fCtxt);
end;

procedure TCEMessagesWidget.projChanged(aProject: ICECommonProject);
begin
end;

procedure TCEMessagesWidget.projCompiling(aProject: ICECommonProject);
begin
end;

procedure TCEMessagesWidget.projCompiled(aProject: ICECommonProject; success: boolean);
begin
end;
{$ENDREGION}

{$REGION ICEMultiDocObserver ---------------------------------------------------}
procedure TCEMessagesWidget.docNew(aDoc: TCESynMemo);
begin

  if fDoc.isNotNil and fOptions.fAutoSelect and (fCtxt = amcEdit) then
  begin
    if list.Selected.isNotNil then
      fEditorMessagePos[fDoc.fileName] := list.Selected.Index
    else
      fEditorMessagePos[fDoc.fileName] := -1;
  end;

  fDoc := aDoc;
  filterMessages(fCtxt);
end;

procedure TCEMessagesWidget.docClosing(aDoc: TCESynMemo);
begin
  if aDoc <> fDoc then exit;
  clearbyData(fDoc);
  fEditorMessagePos.Remove(fDoc.fileName);
  fDoc := nil;
  filterMessages(fCtxt);
end;

procedure TCEMessagesWidget.docFocused(aDoc: TCESynMemo);
var
  i: integer;
begin
  if fDoc = aDoc then exit;

  if fDoc.isNotNil and fOptions.fAutoSelect and (fCtxt = amcEdit) then
  begin
    if list.Selected.isNotNil then
      fEditorMessagePos[fDoc.fileName] := list.Selected.Index
    else
      fEditorMessagePos[fDoc.fileName] := -1;
  end;

  fDoc := aDoc;
  filterMessages(fCtxt);

  if fOptions.fAutoSelect and (fCtxt = amcEdit) then
  begin
    i := fEditorMessagePos.IndexOf(fDoc.fileName);
    if i <> -1 then
    begin
      i := fEditorMessagePos.Data[i];
      if (i <> -1) and (i < list.Items.Count) then
      begin
        list.Selected := list.Items[i];
        list.Selected.MakeVisible;
      end;
    end;
  end;
end;

procedure TCEMessagesWidget.docChanged(aDoc: TCESynMemo);
begin
  fDoc := aDoc;
end;
{$ENDREGION}

{$REGION ICEMessagesDisplay ----------------------------------------------------}
function TCEMessagesWidget.singleServiceName: string;
begin
  exit('ICEMessagesDisplay');
end;

procedure TCEMessagesWidget.message(const aValue: string; aData: Pointer;
  aCtxt: TCEAppMessageCtxt; aKind: TCEAppMessageKind);
var
  dt: PMessageData;
  item: TTreeNode;
  msg: string;
begin
  showWidget;
  msg := aValue;
  if aKind = amkAuto then
    aKind := guessMessageKind(msg);
  dt := new(PMessageData);
  dt^.data := aData;
  dt^.ctxt := aCtxt;
  dt^.demangled:=false;
  if fAutoSelect then if fCtxt <> aCtxt then
    fBtns[aCtxt].Click;
  if fastDisplay then
    IncLoopUpdate;
  item := List.Items.Add(nil, msg);
  item.Data := dt;
  item.ImageIndex := iconIndex(aKind);
  item.SelectedIndex := item.ImageIndex;
  if not fastDisplay then
  begin
    clearOutOfRangeMessg;
    scrollToBack;
    TTreeHack(list).scrolledLeft := 0;
    List.Update;
    filterMessages(fCtxt);
  end;
end;

procedure TCEMessagesWidget.clearByContext(aCtxt: TCEAppMessageCtxt);
var
  i: Integer;
  msgdt: PMessageData;
begin
  list.BeginUpdate;
  TreeFilterEdit1.Filter := '';
  if aCtxt = amcAll then
    List.Items.Clear
  else for i := List.Items.Count-1 downto 0 do
  begin
    msgdt := PMessageData(List.Items[i].Data);
    if msgdt^.ctxt = aCtxt then
      List.Items.Delete(List.Items[i]);
  end;
  list.EndUpdate;
end;

procedure TCEMessagesWidget.clearByData(aData: Pointer);
var
  i: Integer;
  msgdt: PMessageData;
begin
  if aData.isNil then
    exit;
  if (TObject(aData) = fDoc) and (fDoc.isNotNil) then
    fEditorMessagePos[fDoc.fileName] := -1;
  list.BeginUpdate;
  for i := List.Items.Count-1 downto 0 do
  begin
    msgdt := PMessageData(List.Items[i].Data);
    if (msgdt^.data = aData) then
      List.Items.Delete(List.Items[i]);
  end;
  list.EndUpdate;
end;
{$ENDREGION}

{$REGION Messages --------------------------------------------------------------}
procedure TCEMessagesWidget.callDemangler;
var
  dat: PMessageData;
  i: integer;
  str: string;
const
  toolname = 'ddemangle' + exeExt;
begin
  fDemanglerAvailable:= exeInSysPath(toolname);
  if not fDemanglerAvailable then
    exit;
  //
  fDemangler := TCEProcess.Create(nil);
  fDemangler.Executable := exeFullName(toolname);
  fDemangler.OnTerminate:= @demanglerOutput;
  fDemangler.Options:= [poUsePipes];
  fDemangler.ShowWindow:= swoHIDE;
  fToDemangle.Clear;
  fToDemangleObjs.Clear;
  for i := 0 to list.Items.Count-1 do
  begin
    if not list.Items.Item[i].Selected then continue;
    dat := PMessageData(list.Items.Item[i].Data);
    if dat^.demangled then continue;
    dat^.demangled := true;
    str := list.Items.Item[i].Text;
    if str.isEmpty then continue;
    fToDemangleObjs.add(list.Items.Item[i]);
    fToDemangle.Add(str);
  end;
  if fToDemangle.Count > 0 then
  begin
    fDemangler.Execute;
    for i := 0 to fToDemangle.Count-1 do
    begin
      str := fToDemangle[i] + LineEnding;
      fDemangler.Input.Write(str[1], str.length);
    end;
    fDemangler.CloseInput;
  end;
end;

procedure TCEMessagesWidget.demanglerOutput(sender: TObject);
var
  itm: TTreeNode;
  i: integer;
begin
  fToDemangle.LoadFromStream(fDemangler.OutputStack);
  for i := 0 to fToDemangleObjs.Count -1 do
  begin
    itm := TTreeNode(fToDemangleObjs.Items[i]);
    if itm.isNil then continue;
    itm.Text := fToDemangle[i];
  end;
  freeDemangler;
end;

procedure TCEMessagesWidget.freeDemangler;
begin
  if fDemangler.isNil then
    exit;
  //
  if fDemangler.Active then
    fDemangler.Terminate(0);
  FreeAndNil(fDemangler);
end;

procedure TCEMessagesWidget.updateLoop;
begin
  if fastDisplay then
  begin
    clearOutOfRangeMessg;
    scrollToBack;
    List.Update;
    filterMessages(fCtxt);
  end;
end;

function TCEMessagesWidget.iconIndex(aKind: TCEAppMessageKind): Integer;
begin
  case aKind of
    amkBub:  exit(0);
    amkInf:  exit(1);
    amkHint: exit(2);
    amkWarn: exit(3);
    amkErr:  exit(4);
    else exit(0);
  end;
end;

procedure TCEMessagesWidget.clearOutOfRangeMessg;
begin
  list.BeginUpdate;
  while List.Items.Count > fMaxMessCnt do
    List.Items.Delete(List.Items.GetFirstNode);
  list.EndUpdate;
end;

procedure TCEMessagesWidget.scrollToBack;
begin
  if not Visible then
    exit;
  if List.BottomItem.isNotNil then
    List.BottomItem.MakeVisible;
end;

procedure TCEMessagesWidget.handleMessageClick(Sender: TObject);
var
  pos: TPoint;
  msg: string;
begin
  if List.Selected.isNil then
    exit;
  msg := List.Selected.Text;
  if not openFileFromDmdMessage(msg) then
    exit;
  // from here, since a doc has the focus, List.Selected is nil
  pos := getLineFromMessage(msg);
  if fDoc.isNil then
    exit;
  fDoc.CaretXY := pos;
  fDoc.SelectLine;
end;

function TCEMessagesWidget.itemShouldBeVisible(item: TTreeNode;
  aCtxt: TCEAppMessageCtxt): boolean;
var
  msgDt: PMessageData;
begin
  result := false;
  msgDt := PMessageData(item.Data);
  if (not assigned(msgDt)) then
    exit;
  if aCtxt = amcAll then
    result := true
  else case msgDt^.ctxt of
    amcEdit: result := (fDoc  = TCESynMemo(msgDt^.data)) and (aCtxt = amcEdit);
    amcProj: result := (fProj = ICECommonProject(msgDt^.data)) and (aCtxt = amcProj);
    amcApp:  result := aCtxt = amcApp;
    amcMisc: result := aCtxt = amcMisc;
  end;
end;

procedure TCEMessagesWidget.filterMessages(aCtxt: TCEAppMessageCtxt);
var
  itm: TTreeNode;
  i: integer;
begin
  if updating then
    exit;
  List.BeginUpdate;
  for i := 0 to List.Items.Count-1 do
  begin
    itm := List.Items.Item[i];
    if not fFiltering then
      itm.Visible := itemShouldBeVisible(itm, aCtxt)
    else
      itm.Visible := itm.Visible and itemShouldBeVisible(itm, aCtxt);
    itm.Selected := false;
  end;
  list.EndUpdate;
end;

function TCEMessagesWidget.guessMessageKind(const aMessg: string): TCEAppMessageKind;
var
  idt: string;
  rng: TStringRange = (ptr:nil; pos:0; len: 0);
function checkIdent: TCEAppMessageKind;
begin
  case idt of
    'ERROR', 'error', 'Error', 'Invalid', 'invalid',
    'exception', 'Exception', 'illegal', 'Illegal',
    'fatal', 'Fatal', 'Critical', 'critical', 'errorlevel':
      exit(amkErr);
    'Warning', 'warning', 'caution', 'Caution', 'warn', 'Warn':
      exit(amkWarn);
    'Hint', 'hint', 'Tip', 'tip', 'advice', 'Advice',
    'suggestion', 'Suggestion':
      exit(amkHint);
    'Information', 'information':
      exit(amkInf);
    else
      exit(amkBub);
  end;
end;
const
  alp = ['a'..'z', 'A'..'Z'];
begin
  result := amkBub;
  rng.init(aMessg);
  while true do
  begin
    if rng.empty then
      break;
    idt := rng.popUntil(alp)^.takeWhile(alp).yield;
    if idt = '' then
      exit;
    result := checkIdent;
    if result <> amkBub then
      exit;
  end;
end;

function TCEMessagesWidget.getLineFromMessage(const aMessage: string): TPoint;
var
  rng: TStringRange = (ptr:nil; pos:0; len: 0);
  lne: string;
  col: string = '';
begin
  Result := Point(-1,-1);
  if aMessage.isEmpty then
    exit;
  rng.init(aMessage);
  rng.popUntil('(')^.popWhile('(');
  lne := rng.takeUntil([',', ':', ')']).yield;
  if rng.front in [',', ':'] then
    col := rng.popWhile([',', ':'])^.takeUntil(')').yield;
  result.y := strToIntDef(lne, -1);
  result.x := strToIntDef(col, -1);
end;

function TCEMessagesWidget.openFileFromDmdMessage(const aMessage: string): boolean;
var
  i: integer = 0;
  ident: string = '';
  absName: string;
begin
  result := false;
  while (true) do
  begin
    inc(i);
    if i > aMessage.length then
      exit;
    // '(': line will be indicated after fname
    // -mixin: dmd, error in mixin(token string) '<fname>-mixinXX<index>('
    if isEditable(ident.extractFileExt) and ((aMessage[i] = '(') or
      ((aMessage[i] = '-') and (i < aMessage.length-5)
        and (aMessage[i..i+5] = '-mixin'))) then
    begin
      // absolute fname
      if ident.fileExists then
      begin
        getMultiDocHandler.openDocument(ident);
        exit(true);
      end;
      // relative fname if project file is the base path to a rel. fname
      absName := ExpandFileName(ident);
      if absName.fileExists then
      begin
        getMultiDocHandler.openDocument(absName);
        exit(true);
      end;
      // if fname relative to project path
      if fProj <> nil then
      begin
        absName := expandFilenameEx(fProj.filename.extractFileDir + DirectorySeparator, ident);
        if absName.fileExists then
        begin
          getMultiDocHandler.openDocument(absName);
          exit(true);
        end;
      end;
    end
    // <assertion failure messg>@<filename>
    else if aMessage[i] = '@' then
      ident := ''
    else
      ident += aMessage[i];
  end;
end;
{$ENDREGION}

end.
