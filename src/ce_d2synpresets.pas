unit ce_d2synpresets;
{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, SynEditMiscClasses, Graphics, Controls, StdCtrls, ExtCtrls,
  SynEditHighlighter, SynEditTypes,
  ce_interfaces, ce_common, ce_writableComponent, ce_d2syn, ce_observer,
  ce_editoroptions;

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
    fd2syn: TSynD2Syn;
    fName: string;
    procedure setBracketMatchColor(value: TSynSelectedColor);
    procedure setCurrLineAttribs(value: TSynSelectedColor);
    procedure setFoldedColor(value: TSynSelectedColor);
    procedure setIdentifierMarkup(value: TSynSelectedColor);
    procedure setMouseLinkColor(value: TSynSelectedColor);
    procedure setSelCol(value: TSynSelectedColor);
    procedure setD2syn(value: TSynD2Syn);
  published
    property name: string read fName write fName;
    property highlighter: TSynD2Syn read fd2syn write setD2Syn;
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
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function addPreset: TCED2SynPreset;
    property preset[index: integer]: TCED2SynPreset read getPreset ; default;
  end;

  (**
   * UI for loading highlighter presets in the options editor.
   *)
  TCED2SynPresetsLoaderForm = class(TWinControl, ICEEditableOptions)
  private
    fPresets: TCED2SynPresets;
    flstBox: TListBox;
    fBackup: TCED2SynPreset;
    function optionedWantCategory(): string;
    function optionedWantEditorKind: TOptionEditorKind;
    function optionedWantContainer: TPersistent;
    procedure optionedEvent(anEvent: TOptionEditorEvent);
    function optionedOptionsModified: boolean;
    procedure lstBoxChange(sender: TObject);
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

procedure TCED2SynPreset.setD2syn(value: TSynD2Syn);
begin
  fd2syn.Assign(value);
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
  i: integer;
begin
  inherited;
  fBackup:= TCED2SynPreset.Create(nil);
  fPresets:= TCED2SynPresets.Create(self);
  fname := getCoeditDocPath + optfname;
  if fname.fileExists then
    fPresets.loadFromFile(fname)
  else begin
    // Default
    with fPresets.addPreset do
    begin
      fName :='bright';
      fBackground := clWhite;
      fd2syn.whites.define(clNone);
      fd2syn.numbers.define($000079F2);
      fd2syn.symbols.define(clMaroon);
      fd2syn.identifiers.define(clBlack);
      fd2syn.comments.define(clGreen,[fsItalic]);
      fd2syn.strings.define(clBlue);
      fd2syn.keywords.define(clNavy,[fsBold]);
      fd2syn.ddoc.define(clTeal);
      fd2syn.inlineAsm.define(clGray,[fsBold]);
      fd2syn.special.define(clNavy,[fsBold]);
      fd2syn.errors.define(clBlack,[],clNone,clRed,slsWaved,sfeBottom,[]);
      fd2syn.attributes.define(clNavy,[fsBold]);
    end;
    with fPresets.addPreset do
    begin
      fName :='dark';
      fBackground := $00404040;
      fd2syn.whites.define(clNone);
      fd2syn.numbers.define($000079F2,[fsBold]);
      fd2syn.symbols.define(clMaroon);
      fd2syn.identifiers.define($00F0EFE1);
      fd2syn.comments.define($00C7C7C7,[fsItalic]);
      fd2syn.strings.define($ECD284);
      fd2syn.keywords.define($0097C793,[fsBold]);
      fd2syn.ddoc.define(clTeal);
      fd2syn.inlineAsm.define($00CB84EC,[fsBold]);
      fd2syn.special.define($0097C793,[fsBold]);
      fd2syn.errors.define($00F0EFE1,[],clNone,clRed,slsWaved,sfeBottom,[]);
      fd2syn.attributes.define($0097C793,[fsBold]);
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
      fBackground :=clWhite;
      fName :='Mustard';
    end;
  end;
  //
  flstBox := TListBox.Create(self);
  flstBox.Align:= alClient;
  flstBox.BorderSpacing.Around:= 4;
  flstBox.Parent := self;
  flstbox.OnClick:=@lstBoxChange;
  for i:= 0 to fPresets.fCollection.Count-1 do
    flstBox.AddItem(fPresets[i].name, fPresets[i]);
  //
  EntitiesConnector.addObserver(self);
end;

destructor TCED2SynPresetsLoaderForm.Destroy;
begin
  //fPresets.saveToFile(getCoeditDocPath + optfname);
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
    oeeAccept: fBackup.assignFromOptions;
    oeeCancel: fBackup.assignToOptions;
    oeeSelectCat: fBackup.assignFromOptions;
  end;
end;

function TCED2SynPresetsLoaderForm.optionedOptionsModified: boolean;
begin
  exit(false);
end;

procedure TCED2SynPresetsLoaderForm.lstBoxChange(sender: TObject);
begin
  if flstBox.ItemIndex = -1 then
    exit;
  fPresets[flstBox.ItemIndex].assignToOptions;
end;
{$ENDREGION}

initialization
  presetsLoaderForm:= TCED2SynPresetsLoaderForm.Create(nil);
finalization
  presetsLoaderForm.Free;
end.

