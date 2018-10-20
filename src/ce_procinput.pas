unit ce_procinput;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, StdCtrls, Buttons, ce_widget, process, ce_common, ce_interfaces,
  ce_observer, ce_mru, ce_sharedres, ce_dsgncontrols;

type

  { TCEProcInputWidget }

  TCEProcInputWidget = class(TCEWidget, ICEProcInputHandler)
    Panel1: TPanel;
    btnClose: TSpeedButton;
    btnKill: TSpeedButton;
    btnSend: TSpeedButton;
    txtInp: TEdit;
    txtExeName: TStaticText;
    procedure btnCloseClick(Sender: TObject);
    procedure btnKillClick(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure txtInpKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected
    procedure setToolBarFlat(value: boolean); override;
  private
    fMruPos: Integer;
    fMru: TCEMRUList;
    fProc: TProcess;
    fSymStringExpander: ICESymStringExpander;
    procedure sendInput;
    //
    function singleServiceName: string;
    procedure addProcess(process: TProcess);
    procedure removeProcess(process: TProcess);
    function process(): TProcess;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
  end;

implementation
{$R *.lfm}

uses
  LCLType;

const
  OptsFname = 'procinput.txt';

{$REGION Standard Comp/Obj -----------------------------------------------------}
constructor TCEProcInputWidget.create(aOwner: TComponent);
var
  fname: string;
begin
  inherited;
  fSymStringExpander:= getSymStringExpander;
  fMru := TCEMRUList.Create;
  fMru.maxCount := 25;
  EntitiesConnector.addSingleService(self);
  fname := getDocPath + OptsFname;
  if OptsFname.fileExists then
    fMru.LoadFromFile(fname);
  if fMru.Count = 0 then
    fMru.Insert(0, '(your input here)');

  case GetIconScaledSize of
    iss16:
    begin
      AssignPng(btnClose, 'PENCIL_DELETE');
      AssignPng(btnSend, 'PENCIL_GO');
      AssignPng(btnKill, 'CANCEL');
    end;
    iss24:
    begin
      AssignPng(btnClose, 'PENCIL_DELETE24');
      AssignPng(btnSend, 'PENCIL_GO24');
      AssignPng(btnKill, 'CANCEL24');
    end;
    iss32:
    begin
      AssignPng(btnClose, 'PENCIL_DELETE32');
      AssignPng(btnSend, 'PENCIL_GO32');
      AssignPng(btnKill, 'CANCEL32');
    end;
  end;

  toolbarVisible:=false;
end;

destructor TCEProcInputWidget.destroy;
begin
  // note that mru list max count is not saved.
  fMru.SaveToFile(getDocPath + OptsFname);
  fMru.Free;
  inherited;
end;

procedure TCEProcInputWidget.setToolBarFlat(value: boolean);
begin
  inherited;
  btnClose.flat := fToolbarFlat;
  btnKill.flat := fToolbarFlat;
  btnSend.flat := fToolbarFlat;
end;
{$ENDREGION --------------------------------------------------------------------}

{$REGION ICEProcInputHandler ---------------------------------------------------}
function TCEProcInputWidget.singleServiceName: string;
begin
  exit('ICEProcInputHandler');
end;

procedure TCEProcInputWidget.addProcess(process: TProcess);
begin
  Panel1.Enabled:=false;

  // TODO-cfeature: process list, imply that each TCESynMemo must have its own runnable TProcess
  // currently they share the CEMainForm.fRunProc variable.
  if fProc.isNotNil then
    if fProc.Running then
      fProc.Terminate(0);

  txtExeName.Caption := 'no process';
  fProc := nil;
  if process.isNil then
    exit;
  if not (poUsePipes in process.Options) then
    exit;
  fProc := process;
  if fProc.isNotNil then Panel1.Enabled:=true;
  txtExeName.Caption := shortenPath(fProc.Executable);
end;

procedure TCEProcInputWidget.removeProcess(process: TProcess);
begin
  if fProc = process then
    addProcess(nil);
end;

function TCEProcInputWidget.process(): TProcess;
begin
  exit(fProc);
end;
{$ENDREGION}

{$REGION Process input things --------------------------------------------------}
procedure TCEProcInputWidget.sendInput;
var
  inp: string;
begin
  if fProc.Input.isNil or (fProc.Input.Handle = 0) then
    exit;

  fMru.Insert(0,txtInp.Text);
  fMruPos := 0;
  if txtInp.Text <> '' then
    inp := fSymStringExpander.expand(txtInp.Text) + lineEnding
  else
    inp := txtInp.Text + lineEnding;
  fProc.Input.Write(inp[1], inp.length);
  txtInp.Text := '';
end;

procedure TCEProcInputWidget.btnSendClick(Sender: TObject);
begin
  if fProc.isNotNil then
    sendInput;
end;

procedure TCEProcInputWidget.btnCloseClick(Sender: TObject);
begin
  if fProc.isNotNil and fProc.Input.isNotNil and
    (fProc.Input.Handle <> 0) then
      fProc.CloseInput;
end;

procedure TCEProcInputWidget.btnKillClick(Sender: TObject);
begin
  if fProc.isNotNil then
    fProc.Terminate(0);
end;

procedure TCEProcInputWidget.txtInpKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_RETURN:
      if fProc.isNotNil then sendInput;
    VK_UP: begin
      fMruPos += 1;
      if fMruPos > fMru.Count-1 then fMruPos := 0;
      txtInp.Text := fMru[fMruPos];
    end;
    VK_DOWN: begin
      fMruPos -= 1;
      if fMruPos < 0 then fMruPos := fMru.Count-1;
      txtInp.Text := fMru[fMruPos];
    end;
  end;
end;
{$ENDREGION --------------------------------------------------------------------}

end.
