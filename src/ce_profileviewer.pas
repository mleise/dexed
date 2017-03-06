unit ce_profileviewer;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, TASources, TAGraph, TATransformations, TASeries,
  TATools, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus, ComCtrls,
  StdCtrls, ce_widget, ce_common, ce_stringrange, ce_dsgncontrols, ce_ddemangle;

type

  TCEProfileViewerWidget = class(TCEWidget)
    btnOpen: TCEToolButton;
    btnRefresh: TCEToolButton;
    button0: TCEToolButton;
    selPieSource: TComboBox;
    pieTools: TChartToolset;
    pieToolsPanDragTool1: TPanDragTool;
    pieToolsZoomMouseWheelTool1: TZoomMouseWheelTool;
    datNumCalls: TListChartSource;
    datTreeTime: TListChartSource;
    datFuncTime: TListChartSource;
    datPerCall: TListChartSource;
    Panel1: TPanel;
    pie: TChart;
    list: TListView;
    pieSeries: TPieSeries;
    Splitter1: TSplitter;
    procedure btnOpenClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure selPieSourceSelect(Sender: TObject);
    procedure selPieSourceSelectionChange(Sender: TObject; User: boolean);
    procedure Splitter1Moved(Sender: TObject);
  private
    logFname: string;
    procedure clearViewer;
    procedure updateFromFile(const fname: string);
    procedure updatePie;
    procedure listCompare(Sender: TObject; item1, item2: TListItem; Data: Integer; var Compare: Integer);
  public
    constructor create(aOwner: TComponent); override;
  end;

implementation
{$R *.lfm}

constructor TCEProfileViewerWidget.create(aOwner: TComponent);
begin
  inherited;
  clearViewer;
  updatePie;
  list.OnCompare:=@listCompare;
end;

procedure TCEProfileViewerWidget.btnRefreshClick(Sender: TObject);
var
  fname: string;
begin
  if logFname.isNotEmpty and logFname.fileExists then
    updateFromFile(logFname)
  else
  begin
    fname := GetCurrentDir + DirectorySeparator + 'trace.log';
    if fileExists(fname) then
    begin
      updateFromFile(fname);
      logFname:=fname;
    end;
  end;
end;

procedure TCEProfileViewerWidget.selPieSourceSelect(Sender: TObject);
begin
  updatePie;
end;

procedure TCEProfileViewerWidget.btnOpenClick(Sender: TObject);
begin
  with TOpenDialog.Create(nil) do
  try
    if logFname.fileExists and logFname.isNotEmpty then
      FileName := logFname;
    if execute then
    begin
      updateFromFile(filename);
      logFname := FileName;
    end;
  finally
    free;
  end;
end;

procedure TCEProfileViewerWidget.updatePie;
begin
  case selPieSource.ItemIndex of
    1: pieSeries.ListSource.DataPoints.Assign(datTreeTime.DataPoints);
    2: pieSeries.ListSource.DataPoints.Assign(datFuncTime.DataPoints);
    3: pieSeries.ListSource.DataPoints.Assign(datPerCall.DataPoints);
    else pieSeries.ListSource.DataPoints.Assign(datNumCalls.DataPoints);
  end;
  pieSeries.FixedRadius:= pie.Height div 2 - 10;
end;

procedure TCEProfileViewerWidget.selPieSourceSelectionChange(Sender: TObject;User: boolean);
begin
  updatePie;
end;

procedure TCEProfileViewerWidget.Splitter1Moved(Sender: TObject);
begin
  updatePie;
end;

procedure TCEProfileViewerWidget.clearViewer;
begin
  list.Clear;
  pieSeries.Clear;
  datFuncTime.Clear;
  datNumCalls.Clear;
  datTreeTime.Clear;
  datPerCall.Clear;
end;

procedure TCEProfileViewerWidget.updateFromFile(const fname: string);
var
  log: string;
  rng: TStringRange = (ptr:nil; pos:0; len: 0);
  tps: qword;
  idt: string;
  fnc: qword;
  fft: qword;
  ftt: qword;
  fpc: qword;

  procedure fillRow();
  var
    itm: TListItem;
  begin
    list.AddItem(fnc.ToString, nil);
    itm := list.Items[list.Items.Count-1];
    itm.SubItems.Add(fft.ToString);
    itm.SubItems.Add(ftt.ToString);
    itm.SubItems.Add(fpc.ToString);
    itm.SubItems.Add(idt);
    datNumCalls.Add(100, fnc, idt);
    datFuncTime.Add(100, fft, idt);
    datTreeTime.Add(100, ftt, idt);
    datPerCall.Add(100, fpc, idt);
  end;

begin
  clearViewer;

  if not fname.fileExists or (fname.extractFileName <> 'trace.log') then
    exit;

  with TStringList.Create do
  try
    loadFromFile(fname);
    log := strictText;
  finally
    free;
  end;

  if log.length = 0 then
    exit;

  // ======== Timer Is 35.... ============
  rng.init(log);
  rng.popUntil('=')^.popUntil(['0','1','2','3','4','5','6','7','8','9']);
  if rng.empty then
    exit;
  idt := rng.nextWord;
  tps := StrToQWordDef(idt, 0);

  // columns headers
  if rng.popLine^.empty then
    exit;
  if rng.popLine^.empty then
    exit;
  if rng.popLine^.empty then
    exit;
  if rng.popLine^.empty then
    exit;
  if rng.popLine^.empty then
    exit;

  list.BeginUpdate;

  // each function
  while true do
  begin

    idt:= 'unknown function';
    fnc:= 0;
    fft:= 0;
    ftt:= 0;
    fpc:= 0;
    // num calls
    fnc := StrToQWordDef(rng.nextWord, 0);
    if not rng.empty then
      rng.popFront
    else
      break;
    // function time
    fft := StrToQWordDef(rng.nextWord, 0);
    if not rng.empty then
      rng.popFront
    else
      break;
    // tree time
    ftt := StrToQWordDef(rng.nextWord, 0);
    if not rng.empty then
      rng.popFront
    else
      break;
    // per call
    fpc := StrToQWordDef(rng.nextWord, 0);
    if not rng.empty then
      rng.popFront
    else
      break;
    // function name
    rng.popWhile(' ')^.empty;
    idt := demangle(rng.takeUntil(#10).yield);

    fillRow;

    if not rng.empty then
      rng.popFront
    else
      break;
  end;

  list.EndUpdate;
  updatePie;

end;

procedure TCEProfileViewerWidget.listCompare(Sender: TObject; item1, item2: TListItem; Data: Integer; var Compare: Integer);
var
  i1, i2: qword;
  col: Integer;
begin
  col := list.SortColumn;
  if col = 4 then
  begin
    Compare := AnsiCompareStr(item1.SubItems[3], item2.SubItems[3]);
  end
  else
  begin
    if col = 0 then
    begin
      i1 := item1.Caption.ToInt64;
      i2 := item2.Caption.ToInt64;
    end
    else
    begin
      i1 := item1.SubItems[col-1].ToInt64;
      i2 := item2.SubItems[col-1].ToInt64;
    end;
    if (i1 = i2)
      then Compare := 0
    else if (i1 < i2)
      then Compare := -1
    else
      Compare := 1;
  end;
  if list.SortDirection = sdDescending then
    Compare := -Compare;
end;

end.

