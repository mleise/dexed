unit ce_d2synpresets;
{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, SynEditMiscClasses, Graphics, Controls, StdCtrls, ExtCtrls,
  SynEditHighlighter, SynEditTypes, SynEdit, RTTIGrids, Buttons,
  ce_interfaces, ce_common, ce_writableComponent, ce_d2syn, ce_observer,
  ce_editoroptions, ce_sharedres;

type

  TAttribHelper = class helper for TSynHighlighterAttributes
    procedure define(
      fore: TColor;
      Stl: TFontStyles = [];
      bck: TColor = clNone;
      frCol: TColor = clNone;
      frStyle: TSynLineStyle = slsSolid;
      frEdges: TSynFrameEdges = sfeNone;
      stlMsk: TFontStyles = []);
  end;

  (**
   * persistent class used to store a highlighter preset.
   *)
  TCED2SynPreset = class(TCollectionItem)
  private
    fBackground: TColor;
    fBracketMatchAttribs: TSynSelectedColor;
    fCurrLineAttribs: TSynSelectedColor;
    fFoldedColor: TSynSelectedColor;
    fIdentifierMarkup: TSynSelectedColor;
    fMouseLinkAttribs: TSynSelectedColor;
    fSelAttribs: TSynSelectedColor;
    fd2syn: TPersistent;
    fName: string;
    procedure setBracketMatchColor(value: TSynSelectedColor);
    procedure setCurrLineAttribs(value: TSynSelectedColor);
    procedure setFoldedColor(value: TSynSelectedColor);
    procedure setIdentifierMarkup(value: TSynSelectedColor);
    procedure setMouseLinkColor(value: TSynSelectedColor);
    procedure setSelCol(value: TSynSelectedColor);
    procedure setD2syn(value: TPersistent);
    function getHl: TSynD2Syn;
  published
    property name: string read fName write fName;
    property highlighter: TPersistent read fd2syn write setD2Syn;
    property background: TColor read fBackground write fBackground default clWhite;
    property bracketMatch: TSynSelectedColor read fBracketMatchAttribs write setBracketMatchColor;
    property currentLine: TSynSelectedColor read fCurrLineAttribs write setCurrLineAttribs;
    property folding: TSynSelectedColor read fFoldedColor write setFoldedColor;
    property identifierMatch: TSynSelectedColor read fIdentifierMarkup write setIdentifierMarkup;
    property mouseLink: TSynSelectedColor read fMouseLinkAttribs write setMouseLinkColor;
    property selection: TSynSelectedColor read fSelAttribs write setSelCol;
  public
    constructor Create(ACollection: TCollection); override;
    destructor destroy; override;
    procedure assignToOptions;
    procedure assignFromOptions;
  end;

  TCED2SynPresets = class(TWritableLfmTextComponent)
  private
    fCollection: TCollection;
    procedure setCollection(value: TCollection);
    function getPreset(index: integer): TCED2SynPreset;
  published
    property presets: TCollection read fCollection write setCollection;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function addPreset: TCED2SynPreset;
    function count: integer;
    property preset[index: integer]: TCED2SynPreset read getPreset ; default;
  end;

  (**
   * UI for loading highlighter presets in the options editor.
   *)
  TCED2SynPresetsLoaderForm = class(TWinControl, ICEEditableOptions)
  private
    fPresets: TCED2SynPresets;
    fList: TComboBox;
    fEditor: TSynEdit;
    fPropEd: TTIPropertyGrid;
    fBackup: TCED2SynPreset;
    function optionedWantCategory(): string;
    function optionedWantEditorKind: TOptionEditorKind;
    function optionedWantContainer: TPersistent;
    procedure optionedEvent(anEvent: TOptionEditorEvent);
    function optionedOptionsModified: boolean;
    procedure lstBoxSelChange(Sender: TObject);
    procedure btnAddClick(sender: TObject);
    procedure btnDelClick(sender: TObject);
    procedure propEdModified(sender: TObject);
    procedure updateList;
    procedure updateEditor;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

const
  optfname = 'highlighterPresets.txt';

var
  presetsLoaderForm: TCED2SynPresetsLoaderForm;

{$REGION TCED2SynPreset -------------------------------------------------------}
constructor TCED2SynPreset.create(ACollection: TCollection);
begin
  inherited Create(ACOllection);
  fBracketMatchAttribs:= TSynSelectedColor.Create;
  fCurrLineAttribs:= TSynSelectedColor.Create;
  fFoldedColor:= TSynSelectedColor.Create;
  fIdentifierMarkup:= TSynSelectedColor.Create;
  fMouseLinkAttribs:= TSynSelectedColor.Create;
  fSelAttribs:= TSynSelectedColor.Create;
  fd2syn := TSynD2Syn.create(nil);
end;

destructor TCED2SynPreset.destroy;
begin
  fBracketMatchAttribs.free;
  fCurrLineAttribs.free;
  fFoldedColor.free;
  fIdentifierMarkup.free;
  fMouseLinkAttribs.free;
  fSelAttribs.free;
  fd2syn.Free;
  inherited;
end;

procedure TCED2SynPreset.setD2syn(value: TPersistent);
begin
  fd2syn.Assign(value);
end;

function TCED2SynPreset.getHl: TSynD2Syn;
begin
  exit(TSynD2Syn(fd2syn));
end;

procedure TCED2SynPreset.setBracketMatchColor(value: TSynSelectedColor);
begin
  fBracketMatchAttribs.Assign(value);
end;

procedure TCED2SynPreset.setCurrLineAttribs(value: TSynSelectedColor);
begin
  fCurrLineAttribs.Assign(value);
end;

procedure TCED2SynPreset.setFoldedColor(value: TSynSelectedColor);
begin
  fFoldedColor.Assign(value);
end;

procedure TCED2SynPreset.setIdentifierMarkup(value: TSynSelectedColor);
begin
  fIdentifierMarkup.Assign(value);
end;

procedure TCED2SynPreset.setMouseLinkColor(value: TSynSelectedColor);
begin
  fMouseLinkAttribs.Assign(value);
end;

procedure TCED2SynPreset.setSelCol(value: TSynSelectedColor);
begin
  fSelAttribs.Assign(value);
end;

procedure TCED2SynPreset.assignToOptions;
begin
  EditorOptions.background:=background;
  EditorOptions.highlighterDlang.Assign(highlighter);
  EditorOptions.bracketMatch.Assign(bracketMatch);
  EditorOptions.currentLine.Assign(currentLine);
  EditorOptions.folding.Assign(folding);
  EditorOptions.identifierMatch.Assign(identifierMatch);
  EditorOptions.mouseLink.Assign(mouseLink);
  EditorOptions.selection.Assign(selection);
  EditorOptions.applyChangesFromSelf;
end;

procedure TCED2SynPreset.assignFromOptions;
begin
  background:=EditorOptions.background;
  highlighter.Assign(EditorOptions.highlighterDlang);
  bracketMatch.Assign(EditorOptions.bracketMatch);
  currentLine.Assign(EditorOptions.currentLine);
  folding.Assign(EditorOptions.folding);
  identifierMatch.Assign(EditorOptions.identifierMatch);
  mouseLink.Assign(EditorOptions.mouseLink);
  selection.Assign(EditorOptions.selection);
end;
{$ENDREGION}

{$REGION TCED2SynPresets ------------------------------------------------------}
constructor TCED2SynPresets.Create(AOwner: TComponent);
begin
  inherited;
  fCollection := TCollection.Create(TCED2SynPreset);
end;

destructor TCED2SynPresets.Destroy;
begin
  fCollection.Free;
  inherited;
end;

procedure TCED2SynPresets.setCollection(value: TCollection);
begin
  fCollection.Assign(value);
end;

function TCED2SynPresets.addPreset: TCED2SynPreset;
begin
  exit(TCED2SynPreset(fCollection.Add));
end;

function TCED2SynPresets.count: integer;
begin
  exit(fCollection.Count);
end;

function TCED2SynPresets.getPreset(index: integer): TCED2SynPreset;
begin
  exit(TCED2SynPreset(fCollection.Items[index]));
end;
{$ENDREGION}

{$REGION TCED2SynPresetsLoaderForm --------------------------------------------}
procedure TAttribHelper.define(fore: TColor;Stl: TFontStyles = [];
  bck: TColor = clNone; frCol: TColor = clNone; frStyle: TSynLineStyle = slsSolid;
  frEdges: TSynFrameEdges = sfeNone; stlMsk: TFontStyles = []);
begin
  Background:=bck;
  Foreground:=fore;
  FrameColor:=frCol;
  FrameStyle:=frStyle;
  FrameEdges:=frEdges;
  Style:=stl;
  StyleMask:=stlMsk;
end;

constructor TCED2SynPresetsLoaderForm.Create(AOwner: TComponent);
var
  fname: string;
  pnl: TPanel;
  btn: TBitBtn;
begin
  inherited;
  fBackup:= TCED2SynPreset.Create(nil);
  fPresets:= TCED2SynPresets.Create(self);
  fname := getCoeditDocPath + optfname;
  //if fname.fileExists then
    //fPresets.loadFromFile(fname)
  //else
  begin
    with fPresets.addPreset do
    begin
      fName :='bright';
      fBackground := clWhite;
      getHl.whites.define(clNone);
      getHl.numbers.define($000079F2);
      getHl.symbols.define(clMaroon);
      getHl.identifiers.define(clBlack);
      getHl.comments.define(clGreen,[fsItalic]);
      getHl.strings.define(clBlue);
      getHl.keywords.define(clNavy,[fsBold]);
      getHl.ddoc.define(clTeal);
      getHl.inlineAsm.define(clGray,[fsBold]);
      getHl.special.define(clNavy,[fsBold]);
      getHl.errors.define(clBlack,[],clNone,clRed,slsWaved,sfeBottom,[]);
      getHl.attributes.define(clNavy,[fsBold]);
    end;
    with fPresets.addPreset do
    begin
      fName :='dark';
      fBackground := $00404040;
      getHl.whites.define(clNone);
      getHl.numbers.define($F27900,[fsBold]);
      getHl.symbols.define(clMaroon);
      getHl.identifiers.define($E1EFF0);
      getHl.comments.define($C7C7C7,[fsItalic]);
      getHl.strings.define($84D2EC);
      getHl.keywords.define($93C797,[fsBold]);
      getHl.ddoc.define(clTeal);
      getHl.inlineAsm.define($EC84CB,[fsBold]);
      getHl.special.define($93C797,[fsBold]);
      getHl.errors.define($E1EFF0,[],clNone,clRed,slsWaved,sfeBottom,[]);
      getHl.attributes.define($93C797,[fsBold]);
    end;
    with fPresets.addPreset do
    begin
      fBackground := clWhite;
      fName :='Mars bright';
    end;
    with fPresets.addPreset do
    begin
      fBackground :=clWhite;
      fName :='Mars dark';
    end;
    with fPresets.addPreset do
    begin
      fName :='Mustard';
      fBackground := $78C8D3;
      getHl.whites.define(clNone);
      getHl.numbers.define($000079F2,[fsBold]);
      getHl.symbols.define(clMaroon);
      getHl.identifiers.define($1E2331);
      getHl.comments.define($4F7184,[fsItalic]);
      getHl.strings.define($6D82BA);
      getHl.keywords.define($313A5A,[fsBold]);
      getHl.ddoc.define($5F8194);
      getHl.inlineAsm.define($98B7B4,[fsBold]);
      getHl.special.define($313A5A,[fsBold]);
      getHl.errors.define($1E2331,[],clNone,clRed,slsWaved,sfeBottom,[]);
      getHl.attributes.define($313A5A,[fsBold]);
    end;
  end;
  //
  fEditor := TSynEdit.Create(self);
  fEditor.Parent:= self;
  fEditor.Height:= 200;
  fEditor.Align:= alTop;
  fEditor.ReadOnly:=true;
  fEditor.Font.Assign(EditorOptions.font);
  fEditor.Font.Size:=12;
  fEditor.Font.Name:=EditorOptions.font.Name;
  fEditor.BorderSpacing.Around:= 4;
  fEditor.ScrollBars:= ssAutoBoth;
  fEditor.Options:= fEditor.Options - [eoScrollPastEof, eoScrollPastEol];
  fEditor.SetHighlightSearch('writeln',[]);
  fEditor.lines.Add('module preview;');
  fEditor.lines.Add('import std.stdio');
  fEditor.lines.Add('/// ddoc comment');
  fEditor.lines.Add('@safe void main(string[] args)');
  fEditor.lines.Add('{');
  fEditor.lines.Add('    // writeln is the current identifier');
  fEditor.lines.Add('    writeln("this is a string");');
  fEditor.lines.Add('    writeln(__DATE__);');
  fEditor.lines.Add('    int number = 0xDEADBEEF;');
  fEditor.lines.Add('    asm{ xor RAX, RAX; }');
  fEditor.lines.Add('    int error = 12G;');
  fEditor.lines.Add('}');
  pnl := TPanel.Create(self);
  pnl.Parent := self;
  pnl.BevelOuter:= bvNone;
  pnl.BevelInner:= bvNone;
  pnl.Align:=alTop;
  pnl.BorderSpacing.Around:= 4;
  pnl.Height:=30;
  //
  fList := TComboBox.Create(self);
  fList.Align:= alClient;
  fList.BorderSpacing.Around:= 4;
  fList.Parent := pnl;
  fList.ReadOnly:=true;
  fList.OnSelect:= @lstBoxSelChange;
  updateList;
  //
  btn := TBitBtn.Create(self);
  btn.Parent := pnl;
  btn.Width:= 26;
  btn.Align:= alRight;
  btn.BorderSpacing.Around:=2;
  btn.OnClick:=@btnAddClick;
  btn.Hint:='add preset';
  AssignPng(btn, 'document_add');
  //
  btn := TBitBtn.Create(self);
  btn.Parent := pnl;
  btn.Width:= 26;
  btn.Align:= alRight;
  btn.BorderSpacing.Around:=2;
  btn.OnClick:=@btnDelClick;
  btn.Hint:='delete preset';
  AssignPng(btn, 'document_delete');
  //
  fPropEd := TTIPropertyGrid.Create(self);
  fPropEd.Parent := self;
  fPropEd.Align:= alClient;
  fPropEd.DefaultValueFont.Color := clGreen;
  fPropEd.OnModified:=@propEdModified;
  fPropEd.CheckboxForBoolean:=true;
  fPropEd.PropertyEditorHook.AddHandlerModified(@propEdModified);
  //
  fList.ItemIndex := 0;
  lstBoxSelChange(nil);
  EntitiesConnector.addObserver(self);
end;

destructor TCED2SynPresetsLoaderForm.Destroy;
begin
  fPresets.saveToFile(getCoeditDocPath + optfname);
  fBackup.Free;
  EntitiesConnector.removeObserver(self);
  inherited;
end;

function TCED2SynPresetsLoaderForm.optionedWantCategory(): string;
begin
  exit('Highlighter presets');
end;

function TCED2SynPresetsLoaderForm.optionedWantEditorKind: TOptionEditorKind;
begin
  exit(oekControl);
end;

function TCED2SynPresetsLoaderForm.optionedWantContainer: TPersistent;
begin
  exit(self);
end;

procedure TCED2SynPresetsLoaderForm.optionedEvent(anEvent: TOptionEditorEvent);
begin
  case anEvent of
    oeeAccept:
    begin
      fPresets[fList.ItemIndex].assignToOptions;
      fBackup.assignFromOptions;
    end;
    oeeCancel: fBackup.assignToOptions;
    oeeSelectCat: fBackup.assignFromOptions;
  end;
end;

function TCED2SynPresetsLoaderForm.optionedOptionsModified: boolean;
begin
  exit(false);
end;

procedure TCED2SynPresetsLoaderForm.lstBoxSelChange(Sender: TObject);
begin
  if fList.ItemIndex <> -1 then
  begin
    fPropEd.TIObject := fPresets[fList.ItemIndex];
    fPropEd.SplitterX:= (fPropEd.Width - 20) div 2;
    fPropEd.PreferredSplitterX:= fPropEd.SplitterX;
    fEditor.Highlighter := fPresets[fList.ItemIndex].getHl;
    updateEditor;
  end else
    fPropEd.TIObject := nil;
end;

procedure TCED2SynPresetsLoaderForm.btnAddClick(sender: TObject);
var
  prs: TCED2SynPreset;
begin
  prs := fPresets.addPreset;
  prs.name := format('preset %d', [fPresets.count]);
  updateList;
  fList.ItemIndex := prs.Index;
  lstBoxSelChange(nil);
end;

procedure TCED2SynPresetsLoaderForm.btnDelClick(sender: TObject);
begin
  fPresets.fCollection.Delete(fList.ItemIndex);
  updateList;
  lstBoxSelChange(nil);
end;

procedure TCED2SynPresetsLoaderForm.propEdModified(sender: TObject);
begin
  updateEditor;
end;

procedure TCED2SynPresetsLoaderForm.updateList;
var
  i, j: integer;
begin
  fList.OnChange:=nil;
  j := fList.ItemIndex;
  fList.Clear;
  for i:= 0 to fPresets.count-1 do
    fList.AddItem(fPresets[i].name, fPresets[i]);
  if (j <> -1) and (j < fPresets.count) then
    fList.ItemIndex := j;
  fList.OnChange:=@lstBoxSelChange;
end;

procedure TCED2SynPresetsLoaderForm.updateEditor;
var
  p: TCED2SynPreset;
begin
  if fList.ItemIndex = -1 then
    exit;
  p := fPresets[fList.ItemIndex];
  fEditor.Color := p.background;
  fEditor.SelectedColor := p.selection;
  fEditor.HighlightAllColor := p.identifierMatch;
  fEditor.LineHighlightColor := p.currentLine;
  fEditor.FoldedCodeColor := p.folding;
  fEditor.MouseLinkColor := p.mouseLink;
  fEditor.BracketMatchColor := p.bracketMatch;
end;
{$ENDREGION}

initialization
  presetsLoaderForm:= TCED2SynPresetsLoaderForm.Create(nil);
finalization
  presetsLoaderForm.Free;
end.

