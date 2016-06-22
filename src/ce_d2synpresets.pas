unit ce_d2synpresets;
{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, SynEditMiscClasses, Graphics, Controls, StdCtrls, ExtCtrls,
  SynEditHighlighter, SynEditTypes, SynEdit, RTTIGrids, Buttons,
  ce_interfaces, ce_common, ce_writableComponent, ce_d2syn, ce_observer,
  ce_editoroptions, ce_sharedres, ce_txtsyn;

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
    property name: string read fName write fName stored true;
    property highlighter: TPersistent read fd2syn write setD2Syn stored true;
    property background: TColor read fBackground write fBackground stored true;
    property bracketMatch: TSynSelectedColor read fBracketMatchAttribs write setBracketMatchColor stored true;
    property currentLine: TSynSelectedColor read fCurrLineAttribs write setCurrLineAttribs stored true;
    property folding: TSynSelectedColor read fFoldedColor write setFoldedColor stored true;
    property identifierMatch: TSynSelectedColor read fIdentifierMarkup write setIdentifierMarkup stored true;
    property mouseLink: TSynSelectedColor read fMouseLinkAttribs write setMouseLinkColor stored true;
    property selection: TSynSelectedColor read fSelAttribs write setSelCol stored true;
  public
    constructor Create(ACollection: TCollection); override;
    destructor destroy; override;
    procedure assignToOptions;
    procedure assignFromOptions;
    procedure Assign(Source: TPersistent); override;
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
    procedure btnCloneClick(sender: TObject);
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
  TSynTxtSyn(EditorOptions.highlighterGeneric).whites.Assign(getHl.whites);
  TSynTxtSyn(EditorOptions.highlighterGeneric).text.Assign(getHl.identifiers);
  TSynTxtSyn(EditorOptions.highlighterGeneric).symbols.Assign(getHl.symbols);
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

procedure TCED2SynPreset.Assign(Source: TPersistent);
var
  src: TCED2SynPreset;
begin
  if Source is TCED2SynPreset then
  begin
    src := TCED2SynPreset(Source);
    background:=src.background;
    highlighter.Assign(src.highlighter);
    bracketMatch.Assign(src.bracketMatch);
    currentLine.Assign(src.currentLine);
    folding.Assign(src.folding);
    identifierMatch.Assign(src.identifierMatch);
    mouseLink.Assign(src.mouseLink);
    selection.Assign(src.selection);
  end else
    inherited;
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
  if fname.fileExists then
    fPresets.loadFromFile(fname)
  else
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
      //
      folding.Background := clNone;
      folding.Foreground := clDkGray;
      folding.FrameColor := clDkGray;
      //
      mouseLink.Style := [fsUnderline, fsBold];
      mouseLink.StyleMask := [];
      mouseLink.Foreground := clNone;
      mouseLink.Background := clNone;
      //
      bracketMatch.Foreground := clRed;
      bracketMatch.Background := clNone;
      //
      identifierMatch.Foreground:= clNone;
      identifierMatch.Background:= clSilver;
      identifierMatch.BackAlpha:=70;
      identifierMatch.BackPriority:= 10;
      //
      selection.Background:= $CCCCCC;
      selection.Foreground:= clNone;
      //
      currentLine.Background:= $DEDEDE;
      currentLine.Foreground:= clNone;
    end;
    with fPresets.addPreset do
    begin
      fName :='dark';
      getHl.whites.FrameEdges := sfeNone;
      getHl.numbers.Foreground := 16761218;
      getHl.numbers.FrameEdges := sfeNone;
      getHl.numbers.Style := [];
      getHl.symbols.Foreground := clYellow;
      getHl.symbols.FrameEdges := sfeNone;
      getHl.identifiers.Foreground := 14807024;
      getHl.identifiers.FrameEdges := sfeNone;
      getHl.comments.Foreground := 13092807;
      getHl.comments.FrameEdges := sfeNone;
      getHl.strings.Foreground := 5157104;
      getHl.strings.FrameEdges := sfeNone;
      getHl.keywords.Foreground := 9684887;
      getHl.keywords.FrameEdges := sfeNone;
      getHl.ddoc.Foreground := 14671730;
      getHl.ddoc.FrameEdges := sfeNone;
      getHl.inlineAsm.Foreground := 15500491;
      getHl.inlineAsm.FrameEdges := sfeNone;
      getHl.special.Foreground := 9684887;
      getHl.special.FrameEdges := sfeNone;
      getHl.errors.Foreground := 14807024;
      getHl.attributes.Foreground := 9684887;
      getHl.attributes.FrameEdges := sfeNone;
      background := 4210752;
      bracketMatch.Background := clNone;
      bracketMatch.Foreground := clFuchsia;
      currentLine.Background := 6184542;
      currentLine.Foreground := clNone;
      folding.Background := 7303023;
      folding.Foreground := clYellow;
      identifierMatch.Background := 7697781;
      identifierMatch.BackPriority:= 10;
      identifierMatch.Foreground := clNone;
      selection.Background := 10132122;
      selection.Foreground := clNone;
    end;
    with fPresets.addPreset do
    begin
      fName :='Mustard';
      getHl.whites.FrameEdges := sfeNone;
      getHl.numbers.FrameEdges := sfeNone;
      getHl.symbols.Foreground := 3487083;
      getHl.symbols.FrameEdges := sfeNone;
      getHl.identifiers.Foreground := 1975089;
      getHl.identifiers.FrameEdges := sfeNone;
      getHl.comments.Foreground := 5206404;
      getHl.comments.FrameEdges := sfeNone;
      getHl.strings.Foreground := 6056852;
      getHl.strings.FrameEdges := sfeNone;
      getHl.keywords.Foreground := 3226202;
      getHl.keywords.FrameEdges := sfeNone;
      getHl.ddoc.Foreground := 6259092;
      getHl.ddoc.FrameEdges := sfeNone;
      getHl.inlineAsm.Foreground := 3379344;
      getHl.inlineAsm.FrameEdges := sfeNone;
      getHl.special.Foreground := 3226202;
      getHl.special.FrameEdges := sfeNone;
      getHl.errors.Foreground := 1975089;
      getHl.attributes.Foreground := 3226202;
      getHl.attributes.FrameEdges := sfeNone;
      background := 9818842;
      currentLine.Background := 9030871;
      currentLine.Foreground := clNone;
      folding.Background := clNone;
      folding.Foreground := clYellow;
      folding.FrameColor := clYellow;
      identifierMatch.Background := 10278890;
      identifierMatch.BackPriority:= 10;
      identifierMatch.Foreground := clNone;
      selection.Background := 8448232;
      selection.Foreground := clNone;
    end;
    with fPresets.addPreset do
    begin
      name := 'Mars bright';
      getHl.numbers.Foreground := 7763655;
      getHl.comments.Foreground := clMedGray;
      getHl.strings.Foreground := 3750276;
      getHl.keywords.Foreground := 2631874;
      getHl.ddoc.Foreground := 7105644;
      getHl.special.Foreground := 2631874;
      getHl.attributes.Foreground := 2631874;
      background := 16448250;
      bracketMatch.Background := 9276865;
      bracketMatch.Foreground := clNone;
      currentLine.Background := 13421772;
      currentLine.Foreground := clNone;
      folding.Background := clNone;
      folding.Foreground := clNone;
      folding.FrameColor := clBlack;
      identifierMatch.Background := 14145500;
      identifierMatch.Foreground := clNone;
      identifierMatch.BackPriority := 10;
      mouseLink.Background := clNone;
      mouseLink.Foreground := clNone;
      mouseLink.FrameColor := clRed;
      mouseLink.FrameEdges := sfeBottom;
      selection.Background := 12837345;
      selection.Foreground := clNone;
    end;
    with fPresets.addPreset do
    begin
      name := 'Mars dark';
      getHl.Enabled := False;
      getHl.numbers.Foreground := 7763655;
      getHl.symbols.Foreground := 5460961;
      getHl.identifiers.Foreground := clCream;
      getHl.comments.Foreground := 5095359;
      getHl.strings.Foreground := 10790107;
      getHl.keywords.Foreground := 4539883;
      getHl.ddoc.Foreground := 10540501;
      getHl.inlineAsm.Foreground := 12303291;
      getHl.special.Foreground := 2631874;
      getHl.errors.Foreground := clCream;
      getHl.attributes.Foreground := 2631874;
      background := 5263440;
      bracketMatch.Background := 9276865;
      bracketMatch.Foreground := clNone;
      currentLine.Background := 4013373;
      currentLine.Foreground := clNone;
      folding.Background := clNone;
      folding.Foreground := clNone;
      folding.FrameColor := clBlack;
      identifierMatch.Background := 6381928;
      identifierMatch.Foreground := clNone;
      identifierMatch.BackPriority := 10;
      mouseLink.Background := clNone;
      mouseLink.Foreground := clNone;
      mouseLink.FrameColor := clRed;
      mouseLink.FrameEdges := sfeBottom;
      selection.Background := 12837345;
      selection.Foreground := clNone;
    end;
  end;
  // TODO-cd2synpresets: add more presets
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
  fEditor.lines.Add('import std.stdio;');
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
  pnl.BorderSpacing.Around:= 2;
  pnl.Height:=30;
  //
  fList := TComboBox.Create(self);
  fList.Align:= alClient;
  fList.BorderSpacing.Around:= 2;
  fList.Parent := pnl;
  fList.ReadOnly:=true;
  fList.OnSelect:= @lstBoxSelChange;
  updateList;
  //
  btn := TBitBtn.Create(self);
  btn.Parent := pnl;
  btn.Width:= 28;
  btn.Align:= alRight;
  btn.OnClick:=@btnAddClick;
  btn.Hint:='add preset';
  AssignPng(btn, 'DOCUMENT_ADD');
  //
  btn := TBitBtn.Create(self);
  btn.Parent := pnl;
  btn.Width:= 28;
  btn.Align:= alRight;
  btn.OnClick:=@btnDelClick;
  btn.Hint:='delete preset';
  AssignPng(btn, 'DOCUMENT_DELETE');
  //
  btn := TBitBtn.Create(self);
  btn.Parent := pnl;
  btn.Width:= 28;
  btn.Align:= alRight;
  btn.OnClick:=@btnCloneClick;
  btn.Hint:='clone preset';
  AssignPng(btn, 'DOCUMENT_PLUS');
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

procedure TCED2SynPresetsLoaderForm.btnCloneClick(sender: TObject);
var
  old: TCED2SynPreset;
begin
  if fList.ItemIndex = -1 then
    exit;
  old := fPresets[fList.ItemIndex];
  btnAddClick(nil);
  fPresets[fList.ItemIndex].Assign(old);
  updateEditor;
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
  fList.Items[fList.ItemIndex] := fPresets[fList.ItemIndex].name;
end;
{$ENDREGION}

initialization
  presetsLoaderForm:= TCED2SynPresetsLoaderForm.Create(nil);
finalization
  presetsLoaderForm.Free;
end.

