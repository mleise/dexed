unit ce_synmemo;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, SynEdit, SynMemo, ce_d2syn, ce_txtsyn ,SynEditHighlighter,
  controls, lcltype, LazSynEditText, SynEditKeyCmds, SynHighlighterLFM, SynEditMouseCmds,
  ce_common, ce_observer;

type
  TCESynMemo = class(TSynMemo)
  private
    fFilename: string;
    fModified: boolean;
    fFileDate: double;
    fIsDSource: boolean;
    fIsTxtFile: boolean;
    fIsConfig: boolean;
    fIdentifier: string;
    fTempFileName: string;
    fMultiDocSubject: TCECustomSubject;
    fStoredFontSize: Integer;
    procedure changeNotify(Sender: TObject);
    procedure identifierToD2Syn;
  protected
    procedure SetHighlighter(const Value: TSynCustomHighlighter); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor destroy; override;
    procedure setFocus; override;
    procedure UpdateShowing; override;
    procedure DoEnter; override;
    //
    procedure checkFileDate;
    procedure loadFromFile(const aFilename: string);
    procedure saveToFile(const aFilename: string);
    procedure save;
    //
    property Identifier: string read fIdentifier;
    property fileName: string read fFilename;
    property modified: boolean read fModified;
    property tempFilename: string read fTempFileName;
    //
    property isDSource: boolean read fIsDSource;
    property isProjectSource: boolean read fIsConfig;
  end;

var
  D2Syn: TSynD2Syn;
  LfmSyn: TSynLfmSyn;
  TxtSyn: TSynTxtSyn;

implementation

uses
  graphics, ce_interfaces;

constructor TCESynMemo.Create(aOwner: TComponent);
begin
  inherited;
  Font.Quality := fqProof;
  //Font.CharSet := OEM_CHARSET;
  Font.Pitch := fpFixed;
  TabWidth := 4;
  BlockIndent := 4;
  Font.Size:= 10;
  Options :=
    [eoAutoIndent, eoBracketHighlight, eoGroupUndo, eoTabsToSpaces,
    eoDragDropEditing, eoShowCtrlMouseLinks, eoEnhanceHomeKey, eoTabIndent];
  Options2 :=
    [eoEnhanceEndKey, eoFoldedCopyPaste, eoOverwriteBlock];
  fStoredFontSize := Font.Size;

  MouseOptions := MouseOptions +
    [emAltSetsColumnMode, emDragDropEditing, emCtrlWheelZoom];
  Gutter.LineNumberPart.ShowOnlyLineNumbersMultiplesOf := 5;
  Gutter.LineNumberPart.MarkupInfo.Foreground := clGray;
  Gutter.SeparatorPart.LineOffset := 1;
  Gutter.SeparatorPart.LineWidth := 1;
  Gutter.SeparatorPart.MarkupInfo.Foreground := clGray;
  Gutter.CodeFoldPart.MarkupInfo.Foreground := clGray;
  //
  Highlighter := D2Syn;
  D2Syn.FoldKinds := [fkBrackets, fkComments1, fkComments2, fkStrings];
  //
  fTempFileName := GetTempDir(false) + 'temp_' + uniqueObjStr(self) + '.d';
  fFilename := '<new document>';
  fModified := false;
  ShowHint := true;
  TextBuffer.AddNotifyHandler(senrUndoRedoAdded, @changeNotify);
  //
  fMultiDocSubject := TCEMultiDocSubject.create;
  subjDocNew(TCEMultiDocSubject(fMultiDocSubject), self);
end;

destructor TCESynMemo.destroy;
begin
  subjDocClosing(TCEMultiDocSubject(fMultiDocSubject), self);
  fMultiDocSubject.Free;
  //
  if fileExists(fTempFileName) then
    sysutils.DeleteFile(fTempFileName);
  inherited;
end;

procedure TCESynMemo.setFocus;
begin
  inherited;
  if not Visible then exit;
  checkFileDate;
  identifierToD2Syn;
  subjDocFocused(TCEMultiDocSubject(fMultiDocSubject), self);
end;

procedure TCESynMemo.UpdateShowing;
begin
  inherited;
  if not Visible then exit;
  identifierToD2Syn;
  subjDocFocused(TCEMultiDocSubject(fMultiDocSubject), self);
end;

procedure TCESynMemo.DoEnter;
begin
  Inherited;
  checkFileDate;
  identifierToD2Syn;
end;

procedure TCESynMemo.SetHighlighter(const Value: TSynCustomHighlighter);
begin
  inherited;
  fIsDSource := Highlighter = D2Syn;
  fIsConfig := Highlighter = LfmSyn;
  fIsTxtFile := Highlighter = TxtSyn;
end;

procedure TCESynMemo.identifierToD2Syn;
begin
  fIdentifier := GetWordAtRowCol(LogicalCaretXY);
  if fIsDSource then
    D2Syn.CurrentIdentifier := fIdentifier
  else if fIsTxtFile then
    TxtSyn.CurrIdent := fIdentifier;
end;

procedure TCESynMemo.changeNotify(Sender: TObject);
begin
  identifierToD2Syn;
  fModified := true;
  subjDocChanged(TCEMultiDocSubject(fMultiDocSubject), self);
end;

procedure TCESynMemo.loadFromFile(const aFilename: string);
var
  ext: string;
begin
  ext := extractFileExt(aFilename);
  if dExtList.IndexOf(ext) = -1 then
    Highlighter := TxtSyn;
  Lines.LoadFromFile(aFilename);
  fFilename := aFilename;
  FileAge(fFilename, fFileDate);
  fModified := false;
  subjDocChanged(TCEMultiDocSubject(fMultiDocSubject), self);
end;

procedure TCESynMemo.saveToFile(const aFilename: string);
var
  ext: string;
begin
  Lines.SaveToFile(aFilename);
  fFilename := aFilename;
  ext := extractFileExt(aFilename);
  if dExtList.IndexOf(ext) <> -1 then
    Highlighter := D2Syn;
  FileAge(fFilename, fFileDate);
  fModified := false;
  if fFilename <> fTempFileName then
    subjDocChanged(TCEMultiDocSubject(fMultiDocSubject), self);
end;

procedure TCESynMemo.save;
begin
  Lines.SaveToFile(fFilename);
  FileAge(fFilename, fFileDate);
  fModified := false;
  if fFilename <> fTempFileName then
    subjDocChanged(TCEMultiDocSubject(fMultiDocSubject), self);
end;

procedure TCESynMemo.checkFileDate;
var
  newDate: double;
begin
  if fFilename = fTempFileName then exit;
  if not FileAge(fFilename, newDate) then exit;
  if fFileDate = newDate then exit;
  if fFileDate <> 0.0 then
  begin
    if dlgOkCancel(format('"%s" has been modified by another program, load the new version ?',
      [shortenPath(fFilename, 25)])) = mrOk then
    begin
      Lines.LoadFromFile(fFilename);
      fModified := false;
    end;
  end;
  fFileDate := newDate;
end;

procedure TCESynMemo.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  identifierToD2Syn;
  //
  if (Shift = [ssCtrl]) then case Key of
    VK_ADD: if Font.Size < 50 then Font.Size := Font.Size + 1;
    VK_SUBTRACT: if Font.Size > 3 then Font.Size := Font.Size - 1;
    VK_DECIMAL: Font.Size := fStoredFontSize;
  end;
end;

procedure TCESynMemo.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if ssLeft in Shift then
    identifierToD2Syn;
end;

procedure TCESynMemo.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:Integer);
begin
  inherited;
  identifierToD2Syn;
end;

procedure TCESynMemo.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y:Integer);
begin
  inherited;
  if (Button = mbMiddle) and (Shift = [ssCtrl]) then
    Font.Size := fStoredFontSize;
end;

initialization
  D2Syn := TSynD2Syn.create(nil);
  LfmSyn := TSynLFMSyn.Create(nil);
  TxtSyn := TSynTxtSyn.create(nil);
  //
  LfmSyn.KeyAttri.Foreground := clNavy;
  LfmSyn.KeyAttri.Style := [fsBold];
  LfmSyn.NumberAttri.Foreground := clMaroon;
  LfmSyn.StringAttri.Foreground := clBlue;
finalization
  D2Syn.Free;
  LfmSyn.Free;
  TxtSyn.Free;
end.
