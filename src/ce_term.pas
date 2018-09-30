unit ce_term;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ce_widget, TerminalCtrls, ce_interfaces, ce_writableComponent, ce_observer,
  ce_common, ce_synmemo;

type

  // Terminal options
  TCETerminalOptionsBase = class(TWritableLfmTextComponent)
  private
    fBackgroundColor: TColor;
    fForegroundColor: TColor;
    fSelectedColor: TColor;
    fFollowEditors: boolean;
    fFollowProjects: boolean;
    fFollowExplorer: boolean;
    fScrollbackLines: longword;
    fFont: TFont;
    procedure setFont(value: TFont);
  public
    constructor create(AOwner: TComponent); override;
    destructor destroy; override;
    procedure assign(value: TPersistent); override;
  published
    property backgroundColor: TColor read fBackgroundColor write fBackgroundColor;
    property foregroundColor: TColor read fForegroundColor write fForegroundColor;
    property selectedColor: TColor read fSelectedColor write fSelectedColor;
    property font: TFont read fFont write setFont;
    property followEditors: boolean read fFollowEditors write fFollowEditors;
    property followProjects: boolean read fFollowProjects write fFollowProjects;
    property followExplorer: boolean read fFollowExplorer write fFollowExplorer;
    property scrollbackLines: longword read fScrollbackLines write fScrollbackLines default 512;
  end;

  // Editable and reversible Terminal options
  TCETerminalOptions = class(TCETerminalOptionsBase, ICEEditableOptions)
  private
    fBackup: TCETerminalOptionsBase;
    function optionedWantCategory(): string;
    function optionedWantEditorKind: TOptionEditorKind;
    function optionedWantContainer: TPersistent;
    procedure optionedEvent(event: TOptionEditorEvent);
    function optionedOptionsModified: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure applyChanges;
  end;

  { TCETermWidget }

  TCETermWidget = class(TCEWidget, ICEDocumentObserver, ICEProjectObserver, ICEMiniExplorerObserver)
    procedure ContentPaint(Sender: TObject);
  private
    fTerm: TTerminal;
    fOpts: TCETerminalOptions;
    fLastCd: string;
    fNeedApplyChanges: boolean;

    procedure docNew(document: TCESynMemo);
    procedure docFocused(document: TCESynMemo);
    procedure docChanged(document: TCESynMemo);
    procedure docClosing(document: TCESynMemo);

    procedure mnexDirectoryChanged(const directory: string);

    procedure projNew(project: ICECommonProject);
    procedure projChanged(project: ICECommonProject);
    procedure projClosing(project: ICECommonProject);
    procedure projFocused(project: ICECommonProject);
    procedure projCompiling(project: ICECommonProject);
    procedure projCompiled(project: ICECommonProject; success: boolean);

  protected

    procedure DoShow; override;
    procedure SetVisible(Value: boolean); override;

  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
  end;

implementation
{$R *.lfm}

const
  optFname = 'terminal.txt';

constructor TCETerminalOptionsBase.create(AOwner: TComponent);
begin
  inherited;
  fFont := TFont.Create;
  fBackgroundColor:= clWhite;
  fForegroundColor:= clBlack;
  fSelectedColor:= clBlack;
  fFont.Name:= 'Monospace';
  fFont.Size:= 12;
  fScrollbackLines:=512;
end;

destructor TCETerminalOptionsBase.destroy;
begin
  fFont.Free;
  inherited;
end;

procedure TCETerminalOptionsBase.setFont(value: TFont);
begin
  fFont.Assign(value);
end;

procedure TCETerminalOptionsBase.assign(value: TPersistent);
var
  s: TCETerminalOptionsBase;
begin
  if value is TCETerminalOptionsBase then
  begin
    s := TCETerminalOptionsBase(value);
    fBackgroundColor:=s.fbackgroundColor;
    fForegroundColor:=s.fForegroundColor;
    fSelectedColor:=s.fSelectedColor;
    followEditors:=s.fFollowEditors;
    fFont.BeginUpdate;
    fFont.Height:=fFont.Height+1;
    fFont.Height:=fFont.Height-1;
    fFont.Assign(s.font);
    fFont.EndUpdate;
    fScrollbackLines := s.fScrollbackLines;
  end
  else inherited;
end;

constructor TCETerminalOptions.Create(AOwner: TComponent);
begin
  inherited;
  fBackup := TCETerminalOptionsBase.Create(self);
end;

procedure TCETerminalOptions.applyChanges;
var
  w: TCETermWidget;
begin
  w := TCETermWidget(owner);
  w.fTerm.backgroundColor:= backgroundColor;
  w.fTerm.foregroundColor:= foregroundColor;
  w.fTerm.selectedColor:= selectedColor;
  w.fTerm.Font.BeginUpdate;
  w.fTerm.Font.Assign(fFont);
  // force the change: assigning does always trigger TTerminal.FontChanged.
  w.fTerm.Font.Size := w.fTerm.Font.Size +1;
  w.fTerm.Font.Size := w.fTerm.Font.Size -1;
  w.fTerm.Font.endUpdate;
  w.fTerm.scrollbackLines:=fScrollbackLines;
end;

function TCETerminalOptions.optionedWantCategory(): string;
begin
  result := 'Terminal';
end;

function TCETerminalOptions.optionedWantEditorKind: TOptionEditorKind;
begin
  result := oekGeneric;
end;

function TCETerminalOptions.optionedWantContainer: TPersistent;
begin
  result := self;
end;

procedure TCETerminalOptions.optionedEvent(event: TOptionEditorEvent);
begin
  case event of
    oeeAccept:
    begin
      fBackup.assign(self);
      applyChanges;
    end;
    oeeCancel:
    begin
      self.assign(fBackup);
      applyChanges;
    end;
    oeeChange:
    begin
      applyChanges;
    end;
  end;
end;

function TCETerminalOptions.optionedOptionsModified: boolean;
begin
  result := false;
end;

constructor TCETermWidget.create(aOwner: TComponent);
var
  f: string;
begin
  inherited;
  toolbarVisible:=false;
  fTerm := TTerminal.Create(self);
  fTerm.Align:= alClient;
  fTerm.BorderSpacing.Around:=4;
  fterm.Parent := self;

  fOpts:= TCETerminalOptions.Create(self);

  f := getCoeditDocPath + optFname;
  if f.fileExists then
    fOpts.loadFromFile(f);

  EntitiesConnector.addObserver(fOpts);
end;

destructor TCETermWidget.destroy;
begin
  fOpts.saveToFile(getCoeditDocPath + optFname);
  EntitiesConnector.removeObserver(fOpts);
  inherited;
end;

procedure TCETermWidget.DoShow;
begin
  inherited;
  fNeedApplyChanges := true;
end;

procedure TCETermWidget.SetVisible(Value: boolean);
begin
  inherited;
  if Value then
    fNeedApplyChanges := true;
end;

procedure TCETermWidget.ContentPaint(Sender: TObject);
begin
  if not fNeedApplyChanges then
    exit;
  fNeedApplyChanges:=false;
  fOpts.applyChanges;
end;

procedure TCETermWidget.mnexDirectoryChanged(const directory: string);
begin
  if fOpts.followExplorer and directory.dirExists and
    not SameText(directory, fLastCd) then
  begin
    fLastCd := directory;
    fTerm.Command('cd ' + directory);
  end;
end;

procedure TCETermWidget.docNew(document: TCESynMemo);
begin
end;

procedure TCETermWidget.docFocused(document: TCESynMemo);
var
  s: string;
begin
  s := document.fileName.extractFileDir;
  if fOpts.followEditors and s.fileExists and not SameText(s, fLastCd) then
  begin
    fLastCd := s;
    fTerm.Command('cd ' + s);
  end;
end;

procedure TCETermWidget.docChanged(document: TCESynMemo);
begin
end;

procedure TCETermWidget.docClosing(document: TCESynMemo);
begin
end;

procedure TCETermWidget.projNew(project: ICECommonProject);
begin
end;

procedure TCETermWidget.projChanged(project: ICECommonProject);
begin
end;

procedure TCETermWidget.projClosing(project: ICECommonProject);
begin
end;

procedure TCETermWidget.projFocused(project: ICECommonProject);
var
  s: string;
begin
  s := project.fileName.extractFileDir;
  if fOpts.followProjects and s.dirExists and not SameText(s, fLastCd) then
  begin
    fLastCd := s;
    fTerm.Command('cd ' + s);
  end;
end;

procedure TCETermWidget.projCompiling(project: ICECommonProject);
begin
end;

procedure TCETermWidget.projCompiled(project: ICECommonProject; success: boolean);
begin
end;

end.

